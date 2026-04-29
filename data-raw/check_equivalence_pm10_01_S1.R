# Equivalence check: rerun the legacy pm10_01_S1 workflow with the new
# package and confirm the predicted surface (and the in-sample RMSE
# numbers the legacy scripts reported) line up with the original output.
#
# Run from the package root:
#   devtools::load_all()
#   source("data-raw/check_equivalence_pm10_01_S1.R")
#
# Requires AQSURFACE_DATA_DIR (see ?aqs_data_dir). Add to ~/.Renviron:
#   AQSURFACE_DATA_DIR=/Users/<you>/OneDrive/.../RJournal/Code/Data
#
# Notes:
# - The legacy RMSE was *in-sample* (predictions extracted at the same
#   stations used to fit the variogram). We reproduce that here purely
#   for equivalence; Step 2 of the project switches to spatial CV.
# - The legacy script hand-tuned variogram families on a few days; we
#   pass those overrides explicitly so the results match.

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
})

data_dir <- aqs_data_dir()
stations <- make_station_sf(file.path(data_dir, "stations_10km.shp"))
parts    <- split_stations(stations)

train <- load_pollutant(
  file.path(data_dir, "pm10.RData"),
  season = "winter", station_type = "bk",
  stations = parts$train
)

targets <- decade_columns("pm10", month = 1, decade = "S1")
grid    <- build_prediction_grid(train, n_x = 200, n_y = 200)

# Legacy hand tweaks for January S1 (from kr_pm10_01_S1.R):
overrides <- list(
  pm10_1_02_day   = list(model = "Sph", psill = 40, range = 150000, nugget = 40),
  pm10_1_07_night = list(model = "Gau", psill = 20, range = 45000,  nugget = 40),
  pm10_1_08_day   = list(model = "Ste", psill = 10, range = 55000,  nugget = 95),
  pm10_1_09_night = list(model = "Sph", psill = 25, range = 30000,  nugget = 18),
  pm10_1_10_day   = list(model = "Ste", psill = 12, nugget = 11),
  pm10_1_10_night = list(model = "Ste", psill = 22, range = 30000,  nugget = 20)
)

uk <- predict_surface_batch(
  data      = train,
  targets   = targets,
  newgrid   = grid,
  algorithm = "uk",
  trend     = "coords",         # legacy: krige(... ~ X + Y, ...)
  cutoff    = 30000,
  width     = 3000,
  model     = "Ste",
  psill     = 100, nugget = 15,
  overrides = overrides
)

# Re-extract predictions at the *training* stations to mimic the legacy
# in-sample RMSE.
in_sample <- extract_at_stations(uk$pred, train, targets)
cat("In-sample RMSE (Fixed monitors), legacy-style:\n")
print(compute_metrics(in_sample, by = "target"))

# Held-out validation against Road monitors. This is the number that
# matters for the R Journal paper; expect it to be considerably larger.
holdout <- load_pollutant(
  file.path(data_dir, "pm10.RData"),
  season = "winter", station_type = "rd",
  stations = parts$holdout
)
held_out <- extract_at_stations(uk$pred, holdout, targets)
cat("\nHeld-out RMSE (Road monitors):\n")
print(compute_metrics(held_out, by = "target"))

cat("\nOverall comparison:\n")
print(dplyr::bind_rows(
  in_sample |>
    compute_metrics() |>
    dplyr::mutate(set = "in-sample (Fixed)"),
  held_out |>
    compute_metrics() |>
    dplyr::mutate(set = "held-out (Road)")
))
