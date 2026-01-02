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
  library(lubridate)
  library(ggplot2)
})

path <- file.path(DATA_PROC, "la_hourly_aqs_2024_2025.rds")
if (!file.exists(path)) stop("Missing processed data: ", path)

df <- readRDS(path) %>%
  filter(!is.na(o3)) %>%
  mutate(
    o3 = pmax(o3, 0),
    dt_local = as.POSIXct(dt_local, tz = "America/Los_Angeles"),
    year = year(dt_local),
    month = month(dt_local),
    hour = hour(dt_local)
  )

# Fig 4.1a: monthly mean time series
monthly_ts <- df %>%
  mutate(month_date = floor_date(dt_local, "month")) %>%
  group_by(month_date) %>%
  summarize(mean_o3 = mean(o3, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(monthly_ts, aes(x = month_date, y = mean_o3)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Monthly mean O3 (2024–2025)",
    x = "Month",
    y = "Mean O3"
  ) +
  theme_bw()

ggsave(file.path(FIG_DIR, "Fig4_1a_timeseries.png"), plot = p1, width = 7, height = 4.5, dpi = 300)

# Fig 4.1b: monthly climatology
monthly_clim <- df %>%
  group_by(month) %>%
  summarize(
    mean_o3 = mean(o3, na.rm = TRUE),
    q25 = quantile(o3, 0.25, na.rm = TRUE),
    q75 = quantile(o3, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(monthly_clim, aes(x = month, y = mean_o3)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Monthly climatology of O3 (2024–2025)",
    x = "Month of year",
    y = "Mean O3"
  ) +
  theme_bw()

ggsave(file.path(FIG_DIR, "Fig4_1b_seasonal.png"), plot = p2, width = 7, height = 4.5, dpi = 300)

# Fig 4.1c: diurnal pattern with seasons
season_lab <- function(m) {
  if (m %in% c(12, 1, 2)) return("DJF")
  if (m %in% c(3, 4, 5)) return("MAM")
  if (m %in% c(6, 7, 8)) return("JJA")
  "SON"
}

diurnal_df <- df %>%
  mutate(season = vapply(month, season_lab, character(1))) %>%
  group_by(season, hour) %>%
  summarize(mean_o3 = mean(o3, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(diurnal_df, aes(x = hour, y = mean_o3, color = season)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  labs(
    title = "Diurnal O3 pattern by season (2024–2025)",
    x = "Hour (local time)",
    y = "Mean O3",
    color = "Season"
  ) +
  theme_bw()

ggsave(file.path(FIG_DIR, "Fig4_1c_diurnal.png"), plot = p3, width = 7, height = 4.5, dpi = 300)
