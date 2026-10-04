# 02_recode_panel.R — из «сырой» учебной панели (коды RLMS) делаем аналитическую:
# чистим сентинелы, строим факторы с русскими метками, производные переменные.
#
# Вход:  causal-r/data/rlms_panel_raw.rds  (строит 01_extract_rlms.R)
#        causal-r/data/value_labels.csv
# Выход: causal-r/data/rlms_panel.rds      (одна строка = индивид × волна, 2000–2025)
#
# Соглашения:
#   * коды 88888888 / 99999996–99999999 («не знаю», «отказ», «нет ответа», пропуск) → NA
#   * имена переменных — английские snake_case, метки и уровни факторов — русские
#   * бинарные переменные — 0/1 (а не факторы), чтобы прямо идти в регрессии

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(forcats); library(stringr)
})
source(here::here("causal-r/R/cpi_russia.R"))

raw <- readRDS(here::here("causal-r/data/rlms_panel_raw.rds"))
for (v in c("m131", "l131_6")) if (!v %in% names(raw)) raw[[v]] <- NA_real_   # старые выгрузки без них
vl  <- read_csv(here::here("causal-r/data/value_labels.csv"), show_col_types = FALSE)

na_sentinel <- function(x, floor = 88888888) ifelse(!is.na(x) & x >= floor, NA_real_, x)
yes <- function(x) as.integer(na_sentinel(x) == 1)          # 1 = Да, 2 = Нет → 1/0 (NA сохраняется)
lab <- function(var, x) {                                   # коды → фактор по value_labels.csv
  d <- vl[vl$variable == var & vl$value < 88888888, ]
  factor(x, levels = d$value, labels = str_squish(d$label))
}

# Площадка RLMS (переменная region — это 38 PSU) → субъект федерации
site_to_subject <- c(
  `1`="Ленинградская область", `9`="Краснодарский край", `10`="Удмуртская Республика",
  `12`="Пермский край", `14`="Калужская область", `33`="Тамбовская область",
  `39`="Волгоградская область", `45`="Республика Татарстан", `46`="Курганская область",
  `47`="Оренбургская область", `48`="Чувашская Республика", `52`="Ставропольский край",
  `58`="Алтайский край", `66`="Красноярский край", `67`="Тверская область",
  `70`="Саратовская область", `71`="Томская область", `72`="Липецкая область",
  `73`="Красноярский край", `77`="Кабардино-Балкарская Республика", `84`="Алтайский край",
  `86`="Ханты-Мансийский АО", `89`="Республика Коми", `92`="Приморский край",
  `93`="Амурская область", `100`="Саратовская область", `105`="Республика Коми",
  `106`="Челябинская область", `107`="Челябинская область", `116`="Нижегородская область",
  `117`="Пензенская область", `129`="Краснодарский край", `135`="Смоленская область",
  `136`="Тульская область", `137`="Ростовская область", `138`="Москва",
  `141`="Санкт-Петербург", `142`="Московская область", `161`="Новосибирская область",
  `200`="Республика Коми"
)

d <- raw |>
  mutate(across(-c(id_w, year, idind, id_i, id_h, psu, region, status, inwgt,
                   sex, birth_year, n_children_linked, first_birth_year, last_birth_year,
                   starts_with("child")),
                na_sentinel))

panel <- d |>
  transmute(
    # ---- идентификаторы и дизайн выборки -------------------------------------
    idind, id_h, id_w, year, psu,
    site        = lab("region", region),
    site_code   = region,
    subject     = unname(site_to_subject[as.character(region)]),
    settlement  = lab("status", status),                 # Областной центр / Город / ПГТ / Село
    urban       = as.integer(status %in% c(1, 2)),
    inwgt,
    repres      = as.integer(!is.na(inwgt) & inwgt > 0),  # входит в репрезентативную выборку
    int_month   = h7_2,

    # ---- демография ------------------------------------------------------------
    female      = as.integer(sex == 2),
    sex         = factor(sex, levels = c(1, 2), labels = c("Мужчины", "Женщины")),
    age_exact = age,                                         # возраст с шагом 0.5, как в RLMS (для RD)
    age = floor(age),                                        # полные годы
    birth_year = round(birth_year),
    marital     = lab("marst", marst),
    married     = as.integer(marst %in% c(2, 3, 7)),      # зарегистрированный или гражданский брак
    has_children = yes(j72_171),
    n_children  = ifelse(j72_171 == 2, 0, j72_172),
    n_children_u18 = ifelse(j72_171 == 2, 0, j72_173),

    # ---- образование -----------------------------------------------------------
    educ_level  = factor(case_when(diplom %in% 1:3 ~ 1, diplom == 4 ~ 2, diplom == 5 ~ 3, diplom == 6 ~ 4),
                         levels = 1:4,
                         labels = c("Ниже среднего", "Среднее общее", "Среднее профессиональное", "Высшее")),
    higher_ed   = as.integer(diplom == 6),
    educ_detail = educ,
    # приближённые годы обучения по детальному коду educ
    years_educ  = case_when(educ <= 9 ~ educ, educ %in% c(10, 13) ~ 9, educ == 11 ~ 10, educ == 12 ~ 10,
                            educ == 14 ~ 11, educ %in% c(15, 16) ~ 12, educ == 17 ~ 13, educ == 18 ~ 14,
                            educ == 19 ~ 13, educ == 20 ~ 15, educ == 21 ~ 16, educ == 22 ~ 17, educ == 23 ~ 19),
    school_type = lab("j319", j319),
    ege_taken   = yes(j70a),
    in_school   = yes(j70_2),
    in_university = as.integer(j72_5a == 2),
    training_12m = yes(j72_11),

    # ---- занятость и заработки ------------------------------------------------
    work_status = lab("j1", j1),
    employed    = as.integer(j1 %in% 1:4),
    working_now = as.integer(j1 == 1),
    wage        = ifelse(j1 %in% 1:4, j10, NA_real_),       # за последние 30 дней, номинал
    wage_real   = to_rub_2024(wage, year),                   # в рублях 2024 года
    log_wage    = log(wage_real),
    wage_12m    = j13_2,
    hours_week  = ifelse(j6_2 > 0 & j6_2 <= 120, j6_2, NA_real_),
    hours_month = ifelse(j8 > 0 & j8 <= 500, j8, NA_real_),
    official    = yes(j11_1),
    state_owned = yes(j23),
    owner       = yes(j26),
    industry    = lab("j4_1", j4_1),
    occup_group = lab("occup08", occup08),
    occup_code  = j2cod08,
    tenure      = ifelse(!is.na(j5a) & j5a > 1900, year - j5a, NA_real_),
    experience  = j161_3y + coalesce(j161_3m, 0) / 12,
    second_job  = yes(j32),
    second_wage = j40,
    want_other_job = yes(j30),
    worry_job_loss = as.integer(j31 %in% 1:2),
    want_job    = yes(j81),
    search_months = j81_1,
    searched    = yes(j82),
    registered_unemp = yes(j85),
    unemp_benefit = yes(j87),
    main_activity = lab("j90", j90),
    satisf_job  = as.integer(j1_1_1 %in% 1:2),
    satisf_wage = as.integer(j1_1_3 %in% 1:2),
    overtime    = as.integer(j482 %in% 3:5),
    weekends    = as.integer(j484 %in% 3:5),
    income_30d  = j60,
    income_real = to_rub_2024(j60, year),

    # ---- пенсия ----------------------------------------------------------------
    pension     = yes(j73),
    pension_age = ifelse(j73 == 2, 0L, yes(j74_1)),
    pension_year = ifelse(j73_1y > 1900, j73_1y, NA_real_),

    # ---- здоровье и поведение --------------------------------------------------
    health_self = lab("m3", m3),                             # 1 = Очень хорошее … 5 = Совсем плохое
    health_score = 6 - m3,                                   # выше = лучше
    good_health = as.integer(m3 %in% 1:2),
    poor_health = as.integer(m3 %in% 4:5),
    disabled    = as.integer(m20_7 %in% c(1, 5)),
    nerv_12m    = yes(m131),                                 # серьёзные нервные расстройства за 12 мес
    depression  = yes(l131_6),                               # «характерна периодическая депрессия»
    smokes      = yes(m71),
    ever_smoked = yes(m77),
    drinks      = yes(m80_0),
    drink_freq  = lab("m81", m81),
    drinks_weekly = as.integer(m81 %in% 1:4),
    weight_kg   = ifelse(m1 > 25 & m1 < 250, m1, NA_real_),
    height_cm   = ifelse(m2 > 100 & m2 < 230, m2, NA_real_),
    chronic_heart = yes(m20_61), chronic_lungs = yes(m20_62), chronic_liver = yes(m20_63),
    chronic_kidney = yes(m20_64), chronic_stomach = yes(m20_65), chronic_spine = yes(m20_66),

    # ---- субъективное ----------------------------------------------------------
    life_sat    = 6 - j65,                                   # 1..5, выше = довольнее
    life_sat_hi = as.integer(j65 %in% 1:2),
    wealth_ladder = j62,                                     # 1..9
    internet    = yes(j125_1),

    # ---- Большая пятёрка (сырые средние, волны 2016/2019/2020/2023–2025) ------
    O_raw = rowMeans(cbind(5 - j445_3, 5 - j445_11, 5 - j445_14), na.rm = TRUE),
    C_raw = rowMeans(cbind(5 - j445_2,     j445_12, 5 - j445_17), na.rm = TRUE),
    E_raw = rowMeans(cbind(5 - j445_1,     j445_4,  5 - j445_20), na.rm = TRUE),
    A_raw = rowMeans(cbind(5 - j445_9, 5 - j445_16, 5 - j445_19), na.rm = TRUE),
    ES_raw = rowMeans(cbind(5 - j445_5,    j445_10,    j445_18), na.rm = TRUE),

    # ---- дети по ростеру (из файла родственников) ------------------------------
    n_children_linked,
    first_birth_year = round(first_birth_year), last_birth_year = round(last_birth_year),
    child1_birth_year = round(child1_birth_year), child1_sex,
    child2_birth_year = round(child2_birth_year), child2_sex, child3_birth_year = round(child3_birth_year),
    same_sex_first2 = as.integer(!is.na(child1_sex) & !is.na(child2_sex) & child1_sex == child2_sex),
    event_time_first_birth = year - round(first_birth_year)
  ) |>
  mutate(
    across(c(O_raw, C_raw, E_raw, A_raw, ES_raw), ~ ifelse(is.nan(.x), NA_real_, .x)),
    bmi = weight_kg / (height_cm / 100)^2,
    any_chronic = as.integer(chronic_heart == 1 | chronic_lungs == 1 | chronic_liver == 1 |
                             chronic_kidney == 1 | chronic_stomach == 1 | chronic_spine == 1),
    age_group = cut(age, c(0, 17, 24, 34, 44, 54, 64, Inf),
                    labels = c("до 18", "18–24", "25–34", "35–44", "45–54", "55–64", "65+"), right = TRUE),
    adult = as.integer(age >= 18)
  )

# Big Five: стандартизация на базе волны 2016 (как в исходном пайплайне автора)
base <- panel |> filter(year == 2016)
z16 <- function(x, b) (x - mean(b, na.rm = TRUE)) / sd(b, na.rm = TRUE)
panel <- panel |>
  mutate(O = z16(O_raw, base$O_raw), C = z16(C_raw, base$C_raw), E = z16(E_raw, base$E_raw),
         A = z16(A_raw, base$A_raw), ES = z16(ES_raw, base$ES_raw))

# Переменные панели: число волн на человека, первая/последняя волна
panel <- panel |>
  group_by(idind) |>
  mutate(n_waves = n(), first_year = min(year), last_year = max(year)) |>
  ungroup() |>
  arrange(idind, year)

saveRDS(panel, here::here("causal-r/data/rlms_panel.rds"), compress = "gzip")
cat(sprintf("rlms_panel.rds: %d строк, %d индивидов, %d переменных, %d–%d\n",
            nrow(panel), n_distinct(panel$idind), ncol(panel), min(panel$year), max(panel$year)))
