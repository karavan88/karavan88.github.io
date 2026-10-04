# build_rlms_panel.R — сборка учебной панели курса в два шага.
#   1) 01_extract_rlms.R  — из сырых файлов RLMS (нужен RLMS_PATH) → rlms_panel_raw.rds
#   2) 02_recode_panel.R  — чистка и производные переменные      → rlms_panel.rds
# Если rlms_panel_raw.rds уже есть, шаг 1 пропускается.

raw <- here::here("causal-r/data/rlms_panel_raw.rds")
if (!file.exists(raw)) source(here::here("causal-r/R/01_extract_rlms.R"))
source(here::here("causal-r/R/02_recode_panel.R"))
