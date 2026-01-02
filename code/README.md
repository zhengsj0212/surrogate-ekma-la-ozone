# LA Ozone AQS AirData Pipeline (2024–2025)

## Status
- OpenAQ ingestion has been removed.
- The pipeline now uses EPA AQS AirData hourly ZIPs exclusively.
- `code/01_download_aqs_airdata.R` performs ingestion, cleaning, and merging into the final hourly dataset.

## Data Source
EPA AQS AirData hourly ZIP files (public, no API key required):

- `hourly_44201_2024.zip`, `hourly_44201_2025.zip` (O3)
- `hourly_42602_2024.zip`, `hourly_42602_2025.zip` (NO2)
- `hourly_42101_2024.zip`, `hourly_42101_2025.zip` (CO)
- `hourly_88101_2024.zip`, `hourly_88101_2025.zip` (PM2.5)

Period: 2024–2025, Los Angeles County (state_code 06, county_code 037).

To change years, edit `YEARS` in `code/01_download_aqs_airdata.R`.

## Run the pipeline
From the `O3` directory:

```bash
Rscript code/run_all.R
```

## Minimal pipeline order
1) `code/00_setup.R`
2) `code/01_download_aqs_airdata.R`
3) `code/03_fig41_temporal_patterns.R`
4) `code/04_fig42_weekend_effect.R`
5) `code/05_fig43_knn_performance.R`
6) `code/06_fig44_knn_perm_importance.R`
7) `code/07_fig45_knn_ekma_surface.R`

## Outputs
- Raw downloads: `data_raw/aqs/`
- Processed hourly dataset: `data_proc/la_hourly_aqs_2024_2025.rds`
- Figures (PNG + PDF): `figures/`
- Results tables: `results/`
  - `model_metrics.csv`
  - `knn_perm_importance.csv`
  - `knn_ekma_grid_predictions.csv`

## Notes
- All timestamps are converted to America/Los_Angeles for local-time analyses.
- If meteorology is unavailable, the KNN model uses time and location features only.
