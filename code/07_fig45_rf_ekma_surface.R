#!/usr/bin/env Rscript
PROJECT_ROOT <- "/Users/zhengsijie/LinearRegressionGemini/O3"
CODE_DIR <- file.path(PROJECT_ROOT, "code")
DATA_RAW <- file.path(PROJECT_ROOT, "data_raw")
DATA_PROC <- file.path(PROJECT_ROOT, "data_proc")
FIG_DIR <- file.path(PROJECT_ROOT, "figures")
RESULTS_DIR <- file.path(PROJECT_ROOT, "results")

dir.create(DATA_RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_PROC, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
})
source(file.path(CODE_DIR, "utils.R"))

model_path <- file.path(DATA_PROC, "rf_model.rds")
if (!file.exists(model_path)) stop("Missing RF model: ", model_path)
rf_obj <- readRDS(model_path)

if (is.null(rf_obj$feature_cols) || length(rf_obj$feature_cols) == 0) {
  stop("rf_model.rds missing feature_cols")
}

train_path <- file.path(DATA_PROC, "model_data_train.rds")
if (!file.exists(train_path)) stop("Missing model_data_train.rds")
train_data <- readRDS(train_path)

if (is.null(train_data$X) || is.null(train_data$y)) {
  stop("model_data_train.rds missing X or y")
}

# Baseline subset for EKMA-like surface: summer afternoons in 2024
raw_path <- file.path(DATA_PROC, "la_hourly_aqs_2024_2025.rds")
if (!file.exists(raw_path)) stop("Missing processed data: ", raw_path)
raw_df <- readRDS(raw_path) %>%
  mutate(
    dt_local = as.POSIXct(dt_local, tz = "America/Los_Angeles"),
    year = year(dt_local),
    month = month(dt_local),
    hour = hour(dt_local),
    dow = wday(dt_local, week_start = 1),
    o3 = pmax(o3, 0),
    no2 = pmax(no2, 0),
    co = if ("co" %in% names(.)) pmax(co, 0) else NA_real_,
    hour_sin = sin(2 * pi * hour / 24),
    hour_cos = cos(2 * pi * hour / 24),
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12),
    dow_sin = sin(2 * pi * dow / 7),
    dow_cos = cos(2 * pi * dow / 7)
  )

baseline_df <- raw_df %>%
  filter(year == 2024, month %in% 6:9, hour %in% 12:17) %>%
  filter(if_all(all_of(rf_obj$feature_cols), is.finite))

if (nrow(baseline_df) == 0) {
  stop("No baseline samples available for EKMA surface")
}

if (!("no2" %in% rf_obj$feature_cols)) {
  stop("no2 must be included in feature_cols for EKMA surface")
}

if (!("co" %in% rf_obj$feature_cols)) {
  stop("co must be included in feature_cols for EKMA surface")
}

no2_base <- median(baseline_df$no2, na.rm = TRUE)
co_base <- median(baseline_df$co, na.rm = TRUE)
alpha <- seq(0.5, 1.5, by = 0.1)
beta <- seq(0.5, 1.5, by = 0.1)

grid <- expand.grid(alpha = alpha, beta = beta)

make_grid_pred <- function(grid, baseline_df) {
  preds <- vector("numeric", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    df_i <- baseline_df
    df_i$no2 <- no2_base * grid$alpha[i]
    df_i$co <- co_base * grid$beta[i]
    X_i <- as.matrix(df_i[, rf_obj$feature_cols, drop = FALSE])
    pred_i <- predict(rf_obj$model, data = X_i)$predictions
    preds[i] <- mean(pred_i, na.rm = TRUE)
  }
  preds
}

grid$o3_pred <- make_grid_pred(grid, baseline_df)

p1 <- ggplot(grid, aes(x = alpha, y = beta, fill = o3_pred)) +
  geom_tile() +
  geom_contour(aes(z = o3_pred), color = "white", linewidth = 0.4) +
  labs(
    title = "EKMA-like surface (RF surrogate)",
    subtitle = "NO2 and CO scaled from baseline (alpha, beta = 0.5–1.5)",
    x = "NO2 scaling (alpha)",
    y = "CO scaling (beta)",
    fill = "Predicted O3"
  ) +
  theme_bw()

save_plot(p1, file.path(FIG_DIR, "Fig4_5_EKMA_2D"), width = 6.5, height = 5.5)

# Optional hour x NO2 surface for supplementary
hours <- 0:23
alpha2 <- seq(0.5, 1.5, by = 0.1)
sub_grid <- expand.grid(alpha = alpha2, hour = hours)

sub_pred <- lapply(seq_len(nrow(sub_grid)), function(i) {
  df_i <- baseline_df
  df_i$no2 <- no2_base * sub_grid$alpha[i]
  df_i$hour <- sub_grid$hour[i]
  df_i$hour_sin <- sin(2 * pi * df_i$hour / 24)
  df_i$hour_cos <- cos(2 * pi * df_i$hour / 24)
  X_i <- as.matrix(df_i[, rf_obj$feature_cols, drop = FALSE])
  mean(predict(rf_obj$model, data = X_i)$predictions, na.rm = TRUE)
})

sub_plot_df <- sub_grid
sub_plot_df$o3_pred <- unlist(sub_pred)

p2 <- ggplot(sub_plot_df, aes(x = hour, y = no2_base * alpha, fill = o3_pred)) +
  geom_tile() +
  geom_contour(aes(z = o3_pred), color = "white", linewidth = 0.3) +
  labs(
    title = "Supplementary EKMA-like surface (RF surrogate)",
    subtitle = "NO2 scaled from baseline by alpha (0.5–1.5)",
    x = "Hour (local time)",
    y = "NO2 (ppb), scaled from baseline (alpha × baseline)",
    fill = "Predicted O3"
  ) +
  theme_bw()

save_plot(p2, file.path(FIG_DIR, "FigS_1_EKMA_hour_NO2"), width = 7, height = 5.5)
