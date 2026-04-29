test_that("fit_rf + predict_rf recover signal on synthetic data", {
  skip_if_not_installed("ranger")

  pts <- make_synth_sf(n = 80, seed = 4)
  fit <- fit_rf(pts, target = "pm10_1_01_day",
                predictors = c("X", "Y"), num_trees = 100)
  expect_s3_class(fit, "aqs_rf")

  grid <- build_prediction_grid(pts, n_x = 25, n_y = 25)
  pr <- predict_rf(fit, grid)
  expect_s4_class(pr$raster, "SpatRaster")

  # Predictions at training points should correlate strongly with truth.
  pred_at_pts <- terra::extract(pr$raster, terra::vect(pts))[, 2]
  expect_gt(stats::cor(pred_at_pts, pts$pm10_1_01_day,
                       use = "complete.obs"), 0.7)
})

test_that("fit_rf accepts factor predictors via combine_stations", {
  skip_if_not_installed("ranger")

  data("pm10_jan_s1",      package = "aqsurface")
  data("pm10_jan_s1_road", package = "aqsurface")
  train <- sf::st_as_sf(pm10_jan_s1, coords = c("X", "Y"),
                        crs = 5181, remove = FALSE)
  holdout <- sf::st_as_sf(pm10_jan_s1_road, coords = c("X", "Y"),
                          crs = 5181, remove = FALSE)

  combined <- combine_stations(train, holdout)
  expect_s3_class(combined, "sf")
  expect_true("station_type" %in% names(combined))
  expect_setequal(levels(combined$station_type), c("fixed", "road"))

  fit <- fit_rf(combined,
                target = "pm10_1_01_day",
                predictors = c("X", "Y", "station_type"),
                num_trees = 100)
  pr <- predict_rf(fit, combined)
  expect_equal(length(pr$pred), nrow(combined))
})
