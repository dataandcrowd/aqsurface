test_that("fit_gam + predict_gam produce a SpatRaster", {
  pts <- make_synth_sf(n = 60, seed = 3)
  fit <- fit_gam(pts, target = "pm10_1_01_day", smooth = "tp")
  expect_s3_class(fit, "aqs_gam")

  grid <- build_prediction_grid(pts, n_x = 30, n_y = 30)
  pr <- predict_gam(fit, grid)
  expect_s4_class(pr$raster, "SpatRaster")
  # GAM smoother of a linear trend: prediction at training points should
  # be highly correlated with truth.
  pred_at_pts <- terra::extract(pr$raster, terra::vect(pts))[, 2]
  expect_gt(stats::cor(pred_at_pts, pts$pm10_1_01_day,
                       use = "complete.obs"), 0.7)
})
