# Full PM10 benchmark across all 5 months × 3 decade-windows for the
# winter and summer pollutant archives. Produces one master tibble of
# results and writes it to inst/extdata/benchmark_pm10.rds for the
# R Journal article's headline figure.
#
# Run from the package root after AQSURFACE_DATA_DIR is set:
#   devtools::load_all()
#   source("data-raw/run_benchmark_pm10.R")
#
# Expected runtime: ~5-10 minutes on a modern laptop.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

data_dir <- aqs_data_dir()
stations <- make_station_sf(file.path(data_dir, "stations_10km.shp"))
parts    <- split_stations(stations)

# Run one (season, month, decade) cell of the benchmark.
benchmark_cell <- function(season, month, decade) {
  pollutant_path <- file.path(data_dir, "pm10.RData")
  train <- load_pollutant(pollutant_path,
                          season = season, station_type = "bk",
                          stations = parts$train)
  holdout <- load_pollutant(pollutant_path,
                            season = season, station_type = "rd",
                            stations = parts$holdout)
  targets <- decade_columns("pm10", month = month, decade = decade)
  targets <- intersect(targets, names(train))
  if (length(targets) == 0L) return(NULL)

  res <- benchmark_methods(
    train       = train,
    holdout     = holdout,
    targets     = targets,
    algorithms  = c("uk", "gam"),
    cv_folds    = 5,
    fold_method = "kmeans",
    seed        = 1,
    progress    = FALSE
  )
  res |>
    dplyr::mutate(season = season, month = month, decade = decade,
                  .before = "algorithm")
}

# Plan: winter = months 12, 1, 2; summer = months 8, 9.
plan <- dplyr::bind_rows(
  expand.grid(season = "winter", month = c(12, 1, 2),
              decade = c("S1", "S2", "S3"),
              stringsAsFactors = FALSE),
  expand.grid(season = "summer", month = c(8, 9),
              decade = c("S1", "S2", "S3"),
              stringsAsFactors = FALSE)
)

cli::cli_h1("Running {nrow(plan)} (season, month, decade) cells")

results <- vector("list", nrow(plan))
for (i in seq_len(nrow(plan))) {
  cli::cli_h2("[{i}/{nrow(plan)}] {plan$season[i]} m{plan$month[i]} {plan$decade[i]}")
  results[[i]] <- benchmark_cell(plan$season[i],
                                 plan$month[i],
                                 plan$decade[i])
}

benchmark_pm10 <- dplyr::bind_rows(results)
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(benchmark_pm10,
        file = "inst/extdata/benchmark_pm10.rds",
        compress = "xz")

cli::cli_alert_success(
  "Saved {nrow(benchmark_pm10)} rows to inst/extdata/benchmark_pm10.rds"
)
