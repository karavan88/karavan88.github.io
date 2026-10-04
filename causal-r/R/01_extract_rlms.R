# 01_extract_rlms.R — извлечение учебной панели из «сырых» файлов RLMS-HSE.
#
# Запускается ОДИН раз (несколько минут, нужно ~4 ГБ памяти).
# Данные RLMS скачиваются с сайта проекта после регистрации и в репозиторий не попадают.
#
# Вход (путь в переменной окружения RLMS_PATH, например в ~/.Renviron):
#   RLMS_IND_1994_2025_rus.dta  — объединённый файл индивидов (обязательно)
#   ind_relatives_...sav        — файл «Идентификационные номера родственников»
#                                 (по желанию; даёт детей по ростеру для event study и IV)
# Выход:
#   causal-r/data/rlms_panel_raw.rds, value_labels.csv, variable_labels.csv

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(haven); library(readr) })

rlms_path <- Sys.getenv("RLMS_PATH")
stopifnot("Задайте RLMS_PATH в ~/.Renviron" = nzchar(rlms_path))
ind_file <- list.files(rlms_path, pattern = "^RLMS_IND_1994_20[0-9]{2}.*\\.(dta|sav)$", full.names = TRUE)[1]
rel_file <- list.files(rlms_path, pattern = "relatives", full.names = TRUE, recursive = TRUE)[1]
stopifnot("Не найден объединённый файл индивидов RLMS_IND_1994_20xx" = !is.na(ind_file))
out_dir <- here::here("causal-r/data"); dir.create(out_dir, showWarnings = FALSE)

cols <- c(
  "id_w","year","int_y","h7_2","idind","id_i","id_h","psu","region","status","inwgt",
  "age","h5","h6","marst","j322","j72_171","j72_172","j72_173",
  "diplom","educ","j70_1","j319","j70a","j70_2","j72_1a","j72_2a","j72_3a","j72_4a","j72_5a","j72_6a","j72_5e4","j72_11",
  "j1","j10","j13_2","j6_2","j8","j11_1","j23","j26","j4_1","occup08","j2cod08","j5a","j5b","j161_3y","j161_3m","j32","j40",
  "j30","j31","j81","j81_1","j82","j85","j87","j90","j1_1_1","j1_1_2","j1_1_3","j1_1_4","j1_1_10","j482","j484","j60","j62","j65","j66",
  "j73","j74_1","j73_1y","j73_1m",
  "m3","m20_7","m20_8","m71","m77","m80_0","m81","m1","m2","m20_61","m20_62","m20_63","m20_64","m20_65","m20_66",
  "m131", "l131_6",                                # психическое здоровье (лекция 15)
  paste0("j445_", c(1:5, 9:12, 14, 16:20)),
  "j125_1"
)

message("Читаем ", basename(ind_file), " (только нужные столбцы)…")
reader <- if (grepl("\\.dta$", ind_file)) read_dta else read_sav
ind <- reader(ind_file, col_select = all_of(cols))

# --- метки значений и переменных → csv (чтобы 02_recode_panel.R строил факторы) ---
vl <- bind_rows(lapply(cols, function(v) {
  l <- attr(ind[[v]], "labels")
  if (is.null(l)) return(NULL)
  tibble(variable = v, value = unname(l), label = names(l))
}))
write_csv(vl, file.path(out_dir, "value_labels.csv"))
write_csv(tibble(variable = cols,
                 label = vapply(cols, function(v) attr(ind[[v]], "label") %||% "", "")),
          file.path(out_dir, "variable_labels.csv"))

ind <- ind |> mutate(across(everything(), ~ as.numeric(zap_labels(.x))))

# --- атрибуты человека (все волны): пол, год рождения -------------------------
pers <- ind |>
  mutate(by = ifelse(h6 > 1890 & h6 < 2030, h6, year - ifelse(age < 120, age, NA))) |>
  group_by(idind) |>
  summarise(sex = { s <- h5[h5 %in% 1:2]; if (length(s)) as.numeric(names(which.max(table(s)))) else NA_real_ },
            birth_year = median(by, na.rm = TRUE), .groups = "drop")

# --- дети по ростеру (если есть файл родственников) ----------------------------
per_parent <- NULL
if (!is.na(rel_file)) {
  message("Связываем родителей и детей по файлу родственников…")
  ccols <- paste0("r04a", 1:9)                     # r04a* = id_i родных детей в данной волне
  rel <- read_sav(rel_file, col_select = all_of(c("id_w", "id_i", "idind", ccols))) |>
    mutate(across(everything(), ~ as.numeric(zap_labels(.x)))) |>
    filter(!is.na(idind))
  lookup <- ind |> distinct(id_w, id_i, idind) |> drop_na() |>
    rename(child_id_i = id_i, child_idind = idind)
  edges <- rel |>
    pivot_longer(all_of(ccols), values_to = "child_id_i", values_drop_na = TRUE) |>
    filter(child_id_i > 0) |>
    inner_join(lookup, by = c("id_w", "child_id_i")) |>
    transmute(parent_idind = idind, child_idind) |>
    distinct() |> filter(parent_idind != child_idind)
  kids <- edges |>
    left_join(pers |> rename(child_idind = idind, child_sex = sex, child_by = birth_year), by = "child_idind") |>
    arrange(parent_idind, child_by, child_idind) |>
    group_by(parent_idind) |> mutate(rank = row_number()) |> ungroup()
  per_parent <- kids |>
    group_by(idind = parent_idind) |>
    summarise(n_children_linked = n(), first_birth_year = min(child_by, na.rm = TRUE),
              last_birth_year = max(child_by, na.rm = TRUE),
              child1_birth_year = child_by[rank == 1][1], child1_sex = child_sex[rank == 1][1],
              child2_birth_year = child_by[rank == 2][1], child2_sex = child_sex[rank == 2][1],
              child3_birth_year = child_by[rank == 3][1], child3_sex = child_sex[rank == 3][1],
              .groups = "drop") |>
    mutate(across(c(first_birth_year, last_birth_year), ~ ifelse(is.infinite(.x), NA, .x)))
}

# --- учебная панель: волны с 2000 года (id_w >= 9) -----------------------------
panel <- ind |> filter(id_w >= 9) |> left_join(pers, by = "idind")
if (!is.null(per_parent)) panel <- panel |> left_join(per_parent, by = "idind") |>
  mutate(n_children_linked = coalesce(n_children_linked, 0))
panel <- panel |> arrange(idind, id_w)
saveRDS(panel, file.path(out_dir, "rlms_panel_raw.rds"), compress = "xz")
message(sprintf("Готово: %d строк, %d индивидов, %d переменных", nrow(panel), n_distinct(panel$idind), ncol(panel)))
