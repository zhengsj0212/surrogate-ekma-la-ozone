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
  library(ranger)
})
source(file.path(CODE_DIR, "utils.R"))

path <- file.path(DATA_PROC, "la_hourly_aqs_2024_2025.rds")
if (!file.exists(path)) stop("Missing processed data: ", path)

df <- readRDS(path)
df <- add_time_features(df)

time_col <- if ("dt_local" %in% names(df)) "dt_local" else "datetime_local"

df <- df %>%
  mutate(
    year = year(.data[[time_col]]),
    o3 = pmax(o3, 0),
    no2 = pmax(no2, 0)
  )

base_features <- c("no2", "hour_sin", "hour_cos", "month_sin", "month_cos",
                   "dow_sin", "dow_cos", "latitude", "longitude")
optional_features <- c("co", "pm25")
feature_cols <- c(base_features, intersect(optional_features, names(df)))

if (length(feature_cols) < 3) {
  stop("Insufficient feature columns after preprocessing")
}

model_df <- df %>%
  filter(is.finite(o3), is.finite(no2)) %>%
  filter(if_all(all_of(feature_cols), is.finite)) %>%
  mutate(row_id = row_number())

train_df <- model_df %>% filter(year == 2024)
test_df <- model_df %>% filter(year == 2025)
if (nrow(train_df) < 100 || nrow(test_df) < 50) {
  stop("Insufficient data for training/testing after filtering")
}

standardize <- function(train_mat, test_mat) {
  mu <- colMeans(train_mat, na.rm = TRUE)
  sdv <- apply(train_mat, 2, sd, na.rm = TRUE)
  sdv[sdv == 0 | !is.finite(sdv)] <- 1
  train_scaled <- sweep(sweep(train_mat, 2, mu, "-"), 2, sdv, "/")
  test_scaled <- sweep(sweep(test_mat, 2, mu, "-"), 2, sdv, "/")
  list(train = train_scaled, test = test_scaled, mean = mu, sd = sdv)
}

knn_impute <- function(train_df, test_df, feature_cols, k = 5) {
  train_x <- as.matrix(train_df[, feature_cols])
  test_x <- as.matrix(test_df[, feature_cols])
  std <- standardize(train_x, test_x)
  train_scaled <- std$train
  test_scaled <- std$test

  impute_one <- function(row_vec, train_scaled, train_raw, col_idx, k) {
    avail <- which(is.finite(row_vec))
    if (length(avail) < 2) return(NA_real_)
    cand <- which(is.finite(train_raw[, col_idx]))
    if (length(cand) == 0) return(NA_real_)
    dists <- apply(train_scaled[cand, avail, drop = FALSE], 1, function(x) {
      sqrt(sum((x - row_vec[avail])^2))
    })
    nn <- cand[order(dists)][seq_len(min(k, length(cand)))]
    mean(train_raw[nn, col_idx], na.rm = TRUE)
  }

  for (i in seq_len(nrow(train_df))) {
    for (j in seq_along(feature_cols)) {
      if (!is.finite(train_x[i, j])) {
        train_x[i, j] <- impute_one(train_scaled[i, ], train_scaled, train_x, j, k)
      }
    }
  }

  for (i in seq_len(nrow(test_df))) {
    for (j in seq_along(feature_cols)) {
      if (!is.finite(test_x[i, j])) {
        test_x[i, j] <- impute_one(test_scaled[i, ], train_scaled, train_x, j, k)
      }
    }
  }

  list(train = train_x, test = test_x, center = std$mean, scale = std$sd)
}

# KNN imputation fit on train only (no O3 in distance)
imp <- knn_impute(train_df, test_df, feature_cols, k = 5)
train_x <- imp$train
test_x <- imp$test

train_y <- train_df$o3
test_y <- test_df$o3

set.seed(123)
rf_fit <- ranger(
  x = train_x,
  y = train_y,
  num.trees = 500,
  importance = "permutation",
  seed = 123
)

pred_test <- predict(rf_fit, data = test_x)$predictions
rmse <- sqrt(mean((pred_test - test_y)^2, na.rm = TRUE))
r2 <- 1 - sum((pred_test - test_y)^2, na.rm = TRUE) /
  sum((test_y - mean(test_y, na.rm = TRUE))^2, na.rm = TRUE)

metrics <- tibble(
  model = "rf",
  r2 = r2,
  rmse = rmse,
  n_train = nrow(train_df),
  n_test = nrow(test_df)
)
write.csv(metrics, file.path(RESULTS_DIR, "model_metrics.csv"), row.names = FALSE)

pred_df <- tibble(observed = test_y, predicted = pred_test)
write.csv(pred_df, file.path(RESULTS_DIR, "rf_test_predictions.csv"), row.names = FALSE)

saveRDS(list(
  X = train_x,
  y = train_y,
  feature_cols = feature_cols
), file.path(DATA_PROC, "model_data_train.rds"))

saveRDS(list(
  X = test_x,
  y = test_y,
  feature_cols = feature_cols
), file.path(DATA_PROC, "model_data_test.rds"))

saveRDS(list(
  model = rf_fit,
  feature_cols = feature_cols,
  center = imp$center,
  scale = imp$scale
), file.path(DATA_PROC, "rf_model.rds"))

p <- ggplot(pred_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.8) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.5,
           label = sprintf("R^2=%.3f\nRMSE=%.3f", r2, rmse)) +
  labs(
    title = "Observed vs predicted O3 (RF, test set 2025)",
    x = "Observed O3",
    y = "Predicted O3"
  ) +
  theme_bw()

ggsave(file.path(FIG_DIR, "Fig4_3_pred_vs_obs.png"), plot = p, width = 6, height = 5, dpi = 300)
