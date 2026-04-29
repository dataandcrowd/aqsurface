test_that("benchmark_methods supports covariates and the rf algorithm", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("ranger")

  data("pm10_jan_s1",      package = "aqsurface")
  data("pm10_jan_s1_road", package = "aqsurface")
  data("stations_demo",    package = "aqsurface")

  # Attach Road_Dist + DEM by station id.
  meta <- sf::st_drop_geometry(stations_demo)[, c("Station", "Road_Dist", "DEM")]
  train <- merge(pm10_jan_s1,      meta, by.x = "Station.ID", by.y = "Station")
  holdout <- merge(pm10_jan_s1_road, meta, by.x = "Station.ID", by.y = "Station")
  train <- sf::st_as_sf(train, coords = c("X", "Y"), crs = 5181,
                        remove = FALSE)
  holdout <- sf::st_as_sf(holdout, coords = c("X", "Y"), crs = 5181,
                          remove = FALSE)

  res <- benchmark_methods(
    train      = train,
    holdout    = holdout,
    targets    = c("pm10_1_01_day", "pm10_1_01_night"),
    algorithms = c("uk", "gam", "rf"),
    covariates = c("Road_Dist", "DEM"),
    cv_folds   = 4,
    seed       = 1,
    progress   = FALSE
  )
  expect_setequal(unique(res$algorithm), c("uk", "gam", "rf"))
  expect_setequal(unique(res$strategy),
                  c("in-sample", "spatial-cv", "held-out"))
  expect_true("ALL" %in% res$target)
})
