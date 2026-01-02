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

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})
source(file.path(CODE_DIR, "utils.R"))

path <- file.path(DATA_PROC, "la_hourly_aqs_2024_2025.rds")
if (!file.exists(path)) stop("Missing processed data: ", path)

df <- readRDS(path)
plot_df <- df %>%
  filter(is.finite(o3)) %>%
  mutate(day_type = ifelse(is_weekend, "Weekend", "Weekday")) %>%
  group_by(day_type, hour) %>%
  summarize(
    mean_o3 = mean(o3, na.rm = TRUE),
    sd_o3 = sd(o3, na.rm = TRUE),
    n = sum(is.finite(o3)),
    se_o3 = sd_o3 / sqrt(n),
    .groups = "drop"
  )

p <- ggplot(plot_df, aes(x = hour, y = mean_o3, color = day_type, fill = day_type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_o3 - se_o3, ymax = mean_o3 + se_o3), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  labs(
    title = "O3 diurnal cycle: weekdays vs weekends (2024–2025)",
    x = "Hour (local time)",
    y = "Mean O3",
    color = "Day type",
    fill = "Day type"
  ) +
  theme_bw()

save_plot(p, file.path(FIG_DIR, "fig42_o3_weekday_weekend_diurnal"), width = 7.5, height = 5)
