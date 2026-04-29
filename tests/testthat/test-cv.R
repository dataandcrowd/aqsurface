test_that("spatial_kmeans_folds returns disjoint train/test indices", {
  data("stations_demo", package = "aqsurface")
  folds <- spatial_kmeans_folds(stations_demo, k = 5, seed = 1)
  expect_length(folds, 5L)

  all_test <- unlist(lapply(folds, `[[`, "test"))
  expect_setequal(all_test, seq_len(nrow(stations_demo)))

  for (f in folds) {
    expect_true(length(intersect(f$train, f$test)) == 0L)
    expect_equal(length(f$train) + length(f$test), nrow(stations_demo))
  }
})

test_that("spatial_block_folds tiles all stations across k folds", {
  data("stations_demo", package = "aqsurface")
  folds <- spatial_block_folds(stations_demo, k = 4,
                               block_size_m = 5000, seed = 7)
  expect_length(folds, 4L)
  all_test <- unlist(lapply(folds, `[[`, "test"))
  expect_setequal(all_test, seq_len(nrow(stations_demo)))
})

test_that("cv_predict produces (obs, pred) per held-out station", {
  skip_if_not_installed("gstat")
  data("pm10_jan_s1", package = "aqsurface")

  train <- sf::st_as_sf(pm10_jan_s1, coords = c("X", "Y"),
                        crs = 5181, remove = FALSE)
  folds <- spatial_kmeans_folds(train, k = 4, seed = 2)

  out <- cv_predict(train, target = "pm10_1_01_day",
                    folds = folds, algorithm = "uk",
                    trend = "const", cutoff = 30000, width = 3000,
                    quiet = TRUE)
  expect_true(all(c("station", "target", "obs", "pred", "fold") %in%
                    names(out)))
  expect_equal(unique(out$target), "pm10_1_01_day")
  expect_equal(sort(unique(out$fold)), 1:4)
  expect_equal(nrow(out), nrow(train))   # every station predicted once
})
