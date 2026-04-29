test_that("shipped sample data have the expected shapes", {
  skip_if_not_installed("sf")

  data("pm10_jan_s1",      package = "aqsurface")
  data("pm10_jan_s1_road", package = "aqsurface")
  data("stations_demo",    package = "aqsurface")
  data("ratio_demo",       package = "aqsurface")

  # Backgrounds: ~57 monitors, road: 19, both with the 20 Jan S1 cols.
  jan_s1 <- decade_columns("pm10", month = 1, decade = "S1")
  expect_true(all(jan_s1 %in% names(pm10_jan_s1)))
  expect_true(all(jan_s1 %in% names(pm10_jan_s1_road)))
  expect_gte(nrow(pm10_jan_s1), 20)
  expect_gte(nrow(pm10_jan_s1_road), 5)

  # Stations: sf POINT layer, has station_type column with both levels.
  expect_s3_class(stations_demo, "sf")
  expect_true("station_type" %in% names(stations_demo))
  expect_setequal(levels(stations_demo$station_type), c("fixed", "road"))

  # Ratio: one row per Jan S1 column, in order.
  expect_equal(as.character(ratio_demo$Dates), jan_s1)
})

test_that("an end-to-end UK fit works on the shipped sample", {
  skip_if_not_installed("gstat")

  data("pm10_jan_s1",      package = "aqsurface")
  data("pm10_jan_s1_road", package = "aqsurface")

  train <- sf::st_as_sf(pm10_jan_s1, coords = c("X", "Y"),
                        crs = 5181, remove = FALSE)
  holdout <- sf::st_as_sf(pm10_jan_s1_road, coords = c("X", "Y"),
                          crs = 5181, remove = FALSE)
  fit <- fit_uk(train, target = "pm10_1_01_day",
                trend = "const", cutoff = 30000, width = 3000)
  expect_s3_class(fit, "aqs_uk")

  grid <- build_prediction_grid(train, n_x = 30, n_y = 30)
  pr <- predict_uk(fit, grid)
  expect_s4_class(pr$raster, "SpatRaster")

  # auto-detect station_id (Station.ID) and validate at the held-out
  # road monitors.
  long <- extract_at_stations(pr$raster, holdout, "pm10_1_01_day")
  expect_true(all(c("station", "target", "obs", "pred") %in% names(long)))
  expect_equal(unique(long$target), "pm10_1_01_day")
  expect_gte(nrow(long), 5)

  m <- compute_metrics(long)
  expect_true(is.finite(m$rmse))
})
