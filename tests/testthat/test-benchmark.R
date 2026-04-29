test_that("benchmark_methods returns the expected long-format tibble", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("mgcv")

  data("pm10_jan_s1",      package = "aqsurface")
  data("pm10_jan_s1_road", package = "aqsurface")

  train <- sf::st_as_sf(pm10_jan_s1, coords = c("X", "Y"),
                        crs = 5181, remove = FALSE)
  holdout <- sf::st_as_sf(pm10_jan_s1_road, coords = c("X", "Y"),
                          crs = 5181, remove = FALSE)

  # Tiny scope: 2 targets x 2 algorithms x 3 strategies + overall row.
  targets <- c("pm10_1_01_day", "pm10_1_01_night")

  res <- benchmark_methods(
    train      = train,
    holdout    = holdout,
    targets    = targets,
    algorithms = c("uk", "gam"),
    cv_folds   = 4,
    progress   = FALSE,
    seed       = 1
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(c("algorithm", "strategy", "target",
                    "n", "rmse", "mae", "bias", "r") %in% names(res)))
  expect_setequal(unique(res$strategy),
                  c("in-sample", "spatial-cv", "held-out"))
  expect_setequal(unique(res$algorithm), c("uk", "gam"))
  expect_true("ALL" %in% res$target)

  # Held-out RMSE should be at least as large as in-sample RMSE for UK
  # (reproduces the headline finding of step 1).
  uk_rows <- dplyr::filter(res, algorithm == "uk", target == "ALL")
  uk_held  <- uk_rows$rmse[uk_rows$strategy == "held-out"]
  uk_in    <- uk_rows$rmse[uk_rows$strategy == "in-sample"]
  expect_gte(uk_held, uk_in * 0.9)  # allow some tolerance
})
