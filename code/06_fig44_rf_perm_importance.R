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
  library(ggplot2)
})
source(file.path(CODE_DIR, "utils.R"))

model_path <- file.path(DATA_PROC, "rf_model.rds")
if (!file.exists(model_path)) stop("Missing RF model: ", model_path)
rf_obj <- readRDS(model_path)

if (is.null(rf_obj$feature_cols) || length(rf_obj$feature_cols) == 0) {
  stop("rf_model.rds missing feature_cols")
}

if (is.null(rf_obj$model)) stop("rf_model.rds missing model object")

test_path <- file.path(DATA_PROC, "model_data_test.rds")
if (!file.exists(test_path)) stop("Missing model_data_test.rds")

test_data <- readRDS(test_path)
if (is.null(test_data$X) || is.null(test_data$y)) {
  stop("model_data_test.rds missing X or y")
}

X_test <- test_data$X
y_test <- test_data$y
feature_cols <- rf_obj$feature_cols

baseline_pred <- predict(rf_obj$model, data = X_test)$predictions
baseline_rmse <- sqrt(mean((baseline_pred - y_test)^2, na.rm = TRUE))

set.seed(123)
imp_list <- lapply(seq_along(feature_cols), function(i) {
  X_perm <- X_test
  X_perm[, i] <- sample(X_perm[, i])
  pred <- predict(rf_obj$model, data = X_perm)$predictions
  rmse <- sqrt(mean((pred - y_test)^2, na.rm = TRUE))
  tibble(feature = feature_cols[i], importance = rmse - baseline_rmse)
})

imp_df <- bind_rows(imp_list) %>%
  arrange(desc(importance))

write.csv(imp_df, file.path(RESULTS_DIR, "rf_perm_importance.csv"), row.names = FALSE)

p <- ggplot(imp_df %>% slice(1:10), aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "RF permutation importance (top 10)",
    x = "Variable",
    y = "RMSE increase"
  ) +
  theme_bw()

save_plot(p, file.path(FIG_DIR, "Fig4_4_perm_importance"), width = 6.5, height = 5)
