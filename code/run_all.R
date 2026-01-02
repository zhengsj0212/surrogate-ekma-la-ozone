#!/usr/bin/env Rscript
PROJECT_ROOT <- "proj_root"
CODE_DIR <- file.path(PROJECT_ROOT, "code")
DATA_RAW <- file.path(PROJECT_ROOT, "data_raw")
DATA_PROC <- file.path(PROJECT_ROOT, "data_proc")
FIG_DIR <- file.path(PROJECT_ROOT, "figures")
RESULTS_DIR <- file.path(PROJECT_ROOT, "results")

dir.create(DATA_RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_PROC, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

source(file.path(CODE_DIR, "00_setup.R"))
source(file.path(CODE_DIR, "01_download_aqs_airdata.R"))
source(file.path(CODE_DIR, "03_fig41_temporal_patterns.R"))
source(file.path(CODE_DIR, "04_fig42_weekend_effect.R"))
source(file.path(CODE_DIR, "05_fig43_rf_performance.R"))
source(file.path(CODE_DIR, "06_fig44_rf_perm_importance.R"))
source(file.path(CODE_DIR, "07_fig45_rf_ekma_surface.R"))
