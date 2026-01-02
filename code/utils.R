suppressPackageStartupMessages({
  library(ggplot2)
  library(tibble)
  library(lubridate)
})

save_plot <- function(p, filename_base, width = 7, height = 5, dpi = 300) {
  ggsave(paste0(filename_base, ".png"), plot = p, width = width, height = height, dpi = dpi)
  ggsave(paste0(filename_base, ".pdf"), plot = p, width = width, height = height)
}

summarize_missingness <- function(df, cols) {
  tibble(
    variable = cols,
    missing = sapply(cols, function(cn) sum(!is.finite(df[[cn]]))),
    missing_pct = sapply(cols, function(cn) mean(!is.finite(df[[cn]]))) * 100
  )
}

add_time_features <- function(df) {
  if ("datetime_local" %in% names(df)) {
    dt_local <- df$datetime_local
  } else if ("dt_local" %in% names(df)) {
    dt_local <- df$dt_local
  } else if (all(c("date_local", "time_local") %in% names(df))) {
    dt_local <- ymd_hm(paste(df$date_local, df$time_local), tz = "America/Los_Angeles")
  } else {
    stop("No local timestamp column found (expected datetime_local or date_local/time_local)")
  }

  if (!inherits(dt_local, "POSIXct")) {
    dt_local <- as.POSIXct(dt_local, tz = "America/Los_Angeles")
  }

  hour <- hour(dt_local)
  month <- month(dt_local)
  dow <- wday(dt_local, week_start = 1)

  df$hour <- hour
  df$month <- month
  df$dow <- dow
  df$hour_sin <- sin(2 * pi * hour / 24)
  df$hour_cos <- cos(2 * pi * hour / 24)
  df$dow_sin <- sin(2 * pi * dow / 7)
  df$dow_cos <- cos(2 * pi * dow / 7)
  df$month_sin <- sin(2 * pi * month / 12)
  df$month_cos <- cos(2 * pi * month / 12)
  df
}
