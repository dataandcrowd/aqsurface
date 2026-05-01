# Full NO2 benchmark across all 5 months (12, 1, 2, 8, 9) x 3 decade
# windows. Mirror of `run_benchmark_pm10.R` for the second pollutant
# in the legacy archive. Combined with the PM10 sweep, the resulting
# rds files form the empirical backbone of the R Journal paper.
#
# Run from the package root after AQSURFACE_DATA_DIR is set:
#   devtools::load_all()
#   source("data-raw/run_benchmark_no2.R")
#
# Expected runtime: ~5-10 minutes on a modern laptop.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

data_dir <- aqs_data_dir()
stations <- make_station_sf(file.path(data_dir, "stations_10km.shp"))
parts    <- split_stations(stations)

benchmark_cell <- function(season, month, decade) {
  pollutant_path <- file.path(data_dir, "no2.RData")
  train <- load_pollutant(pollutant_path,
                          season = season, station_type = "bk",
                          stations = parts$train)
  holdout <- load_pollutant(pollutant_path,
                            season = season, station_type = "rd",
                            stations = parts$holdout)
  targets <- decade_columns("no2", month = month, decade = decade)
  targets <- intersect(targets, names(train))
  if (length(targets) == 0L) return(NULL)

  res <- benchmark_methods(
    train       = train,
    holdout     = holdout,
    targets     = targets,
    algorithms  = c("uk", "gam", "rf"),
    cv_folds    = 5,
    fold_method = "kmeans",
    seed        = 1,
    progress    = FALSE
  )
  res |>
    dplyr::mutate(season = season, month = month, decade = decade,
                  .before = "algorithm")
}

plan <- dplyr::bind_rows(
  expand.grid(season = "winter", month = c(12, 1, 2),
              decade = c("S1", "S2", "S3"),
              stringsAsFactors = FALSE),
  expand.grid(season = "summer", month = c(8, 9),
              decade = c("S1", "S2", "S3"),
              stringsAsFactors = FALSE)
)

cli::cli_h1("Running {nrow(plan)} (season, month, decade) cells for NO2")

results <- vector("list", nrow(plan))
for (i in seq_len(nrow(plan))) {
  cli::cli_h2("[{i}/{nrow(plan)}] {plan$season[i]} m{plan$month[i]} {plan$decade[i]}")
  results[[i]] <- benchmark_cell(plan$season[i],
                                 plan$month[i],
                                 plan$decade[i])
}

benchmark_no2 <- dplyr::bind_rows(results)
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(benchmark_no2,
        file = "inst/extdata/benchmark_no2.rds",
        compress = "xz")

cli::cli_alert_success(
  "Saved {nrow(benchmark_no2)} rows to inst/extdata/benchmark_no2.rds"
)
