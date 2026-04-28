test_that("decade_columns enumerates day/night pairs in order", {
  cols <- decade_columns("pm10", month = 1, decade = "S1")
  expect_length(cols, 20L)
  expect_equal(cols[1:2], c("pm10_1_01_day", "pm10_1_01_night"))
  expect_equal(cols[length(cols)], "pm10_1_10_night")
})

test_that("decade_columns S3 respects days_in_month", {
  feb <- decade_columns("pm10", month = 2, decade = "S3")
  expect_equal(length(feb), (28 - 21 + 1) * 2)

  feb_leap <- decade_columns("pm10", month = 2, decade = "S3",
                             days_in_month = 29)
  expect_equal(length(feb_leap), (29 - 21 + 1) * 2)
})

test_that("build_prediction_grid returns a SpatRaster of expected size", {
  pts <- make_synth_sf()
  g <- build_prediction_grid(pts, n_x = 50, n_y = 60)
  expect_s4_class(g, "SpatRaster")
  expect_equal(terra::ncol(g), 50L)
  expect_equal(terra::nrow(g), 60L)
})
