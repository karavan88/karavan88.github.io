# load_panel.R — подключается в начале каждой лекции: source(here::here("causal-r/R/load_panel.R"))
# Загружает аналитическую панель `rlms` (индивид × волна, 2000–2025) и задаёт общий стиль.

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(modelsummary)
  library(marginaleffects)
  library(broom)
})

options(
  modelsummary_factory_default = "kableExtra",
  modelsummary_format_numeric_latex = "plain",
  width = 90, scipen = 999, digits = 3
)
# fixest: кластеризация по индивиду по умолчанию, компактный вывод
setFixest_estimation(panel.id = ~ idind + year)
setFixest_etable(digits = 3, digits.stats = 2, fitstat = ~ n + r2, drop.section = "fixef")
setFixest_notes(FALSE)

# Палитра курса (совпадает с темой слайдов)
pal <- c(indigo = "#1c5cab", coral = "#d9534f", teal = "#0f8a7e",
         sage = "#c7861e", lavender = "#7fb3ee", charcoal = "#344054")
theme_set(theme_minimal(base_size = 15) +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#e7ecf4"),
                  axis.text = element_text(colour = "#475467"), axis.title = element_text(colour = "#344054"),
                  plot.title = element_text(colour = "#0d366b", face = "bold"), strip.text = element_text(colour = "#0d366b", face = "bold"),
                  plot.title.position = "plot", legend.position = "bottom"))
scale_colour_discrete <- function(...) scale_colour_manual(values = unname(pal), ...)
scale_fill_discrete   <- function(...) scale_fill_manual(values = unname(pal), ...)

panel_path <- here::here("causal-r/data/rlms_panel.rds")
if (file.exists(panel_path)) {
  rlms <- readRDS(panel_path)
} else {
  stop("Нет causal-r/data/rlms_panel.rds — запустите causal-r/R/build_rlms_panel.R")
}

# Удобная «рабочая» выборка: взрослые 18–64, репрезентативная подвыборка не требуется
adults <- rlms |> filter(age >= 18, age <= 64)
