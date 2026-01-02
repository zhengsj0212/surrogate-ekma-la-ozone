#!/usr/bin/env Rscript
# Environment setup only: project paths, directories, and core packages.
PROJECT_ROOT <- "/Users/zhengsijie/LinearRegressionGemini/O3"
CODE_DIR <- file.path(PROJECT_ROOT, "code")
DATA_RAW_DIR <- file.path(PROJECT_ROOT, "data_raw")
DATA_PROC_DIR <- file.path(PROJECT_ROOT, "data_proc")
FIG_DIR <- file.path(PROJECT_ROOT, "figures")
RESULTS_DIR <- file.path(PROJECT_ROOT, "results")

dir.create(DATA_RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_PROC_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(readr)
})

theme_set(theme_bw())
