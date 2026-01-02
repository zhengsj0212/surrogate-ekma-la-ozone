#!/usr/bin/env Rscript
PROJECT_ROOT <- "/Users/zhengsijie/LinearRegressionGemini/O3"
CODE_DIR <- file.path(PROJECT_ROOT, "code")
DATA_RAW <- file.path(PROJECT_ROOT, "data_raw")
DATA_PROC <- file.path(PROJECT_ROOT, "data_proc")
FIG_DIR <- file.path(PROJECT_ROOT, "figures")
RESULTS_DIR <- file.path(PROJECT_ROOT, "results")

AQS_DIR <- file.path(DATA_RAW, "aqs")

# Ensure directories exist
for (d in c(DATA_RAW, DATA_PROC, FIG_DIR, RESULTS_DIR, AQS_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(lubridate)
  library(tidyr)
})
source(file.path(CODE_DIR, "utils.R"))

BASE_URL <- "https://aqs.epa.gov/aqsweb/airdata"
YEARS <- c(2024, 2025)
STATE_CODE <- "06"
COUNTY_CODE <- "037"

PARAMETERS <- list(
  o3 = "44201",
  no2 = "42602",
  co = "42101",
  pm25 = "88101"
)

read_airdata_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  df <- df %>%
    rename(
      state_code = `State Code`,
      county_code = `County Code`,
      site_id = `Site Num`,
      latitude = Latitude,
      longitude = Longitude,
      date_local = `Date Local`,
      time_local = `Time Local`,
      value = `Sample Measurement`,
      units = `Units of Measure`
    )
  df <- df %>%
    mutate(
      state_code = sprintf("%02d", as.integer(state_code)),
      county_code = sprintf("%03d", as.integer(county_code))
    )
  df
}

download_zip <- function(url, dest) {
  req <- request(url)
  resp <- req_perform(req, path = dest)
  if (resp_status(resp) >= 400) {
    stop("AirData download failed: ", url)
  }
  dest
}

fetch_airdata_param_year <- function(param_code, year) {
  zip_name <- paste0("hourly_", param_code, "_", year, ".zip")
  csv_name <- paste0("hourly_", param_code, "_", year, ".csv")
  zip_path <- file.path(AQS_DIR, zip_name)
  csv_path <- file.path(AQS_DIR, csv_name)
  url <- paste0(BASE_URL, "/", zip_name)

  if (!file.exists(csv_path)) {
    if (!file.exists(zip_path)) {
      message("[download] ", url)
      download_zip(url, zip_path)
    }
    unzip(zip_path, exdir = AQS_DIR)
  }

  if (!file.exists(csv_path)) {
    stop("Missing expected CSV after unzip: ", csv_path)
  }

  df <- read_airdata_csv(csv_path)
  df
}

make_timestamps <- function(df) {
  dt_local <- ymd_hm(
    paste(df$date_local, df$time_local),
    tz = "America/Los_Angeles"
  )
  dt_utc <- with_tz(dt_local, "UTC")
  list(dt_local = dt_local, dt_utc = dt_utc)
}

all_list <- list()

for (param_name in names(PARAMETERS)) {
  param_code <- PARAMETERS[[param_name]]
  for (yr in YEARS) {
    df <- fetch_airdata_param_year(param_code, yr)
    if (nrow(df) == 0) next

    df <- df %>%
      filter(state_code == STATE_CODE, county_code == COUNTY_CODE)

    if (nrow(df) == 0) {
      message(sprintf("[filter] empty %s %d after LA county filter", param_name, yr))
      next
    }

    ts <- make_timestamps(df)
    out <- tibble(
      dt_local = ts$dt_local,
      dt_utc = ts$dt_utc,
      site_id = df$site_id,
      latitude = suppressWarnings(as.numeric(df$latitude)),
      longitude = suppressWarnings(as.numeric(df$longitude)),
      parameter = param_name,
      value = suppressWarnings(as.numeric(df$value))
    )

    all_list[[length(all_list) + 1]] <- out
  }
}

if (length(all_list) == 0) {
  stop("No AQS AirData records found for LA County in 2024-2025")
}

long_df <- bind_rows(all_list) %>%
  filter(is.finite(value))

wide_df <- long_df %>%
  group_by(site_id, latitude, longitude, dt_local, dt_utc, parameter) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = parameter, values_from = value)

wide_df <- wide_df %>%
  mutate(
    year = year(dt_local),
    month = month(dt_local),
    day = day(dt_local),
    hour = hour(dt_local),
    dow = wday(dt_local, label = TRUE, abbr = FALSE, week_start = 1),
    is_weekend = dow %in% c("Saturday", "Sunday")
  ) %>%
  arrange(dt_utc)

out_path <- file.path(DATA_PROC, "la_hourly_aqs_2024_2025.rds")
saveRDS(wide_df, out_path)
message("[output] saved: ", out_path)
