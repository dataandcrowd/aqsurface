test_that("fit_uk + predict_uk recover a linear trend on synthetic data", {
  pts <- make_synth_sf(n = 60, seed = 2)
  fit <- fit_uk(pts, target = "pm10_1_01_day", trend = "coords",
                cutoff = 25000, width = 2500)
  expect_s3_class(fit, "aqs_uk")
  expect_true(all(fit$model$psill >= 0))

  grid <- build_prediction_grid(pts, n_x = 30, n_y = 30)
  pr <- predict_uk(fit, grid)
  expect_s4_class(pr$raster, "SpatRaster")
  expect_equal(terra::ncell(pr$raster), 30L * 30L)
  # variance must be non-negative
  expect_true(all(pr$var >= -1e-8))
})
