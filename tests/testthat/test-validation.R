test_that("compute_metrics returns RMSE/MAE/bias/r consistently", {
  set.seed(11)
  df <- data.frame(
    obs  = rnorm(40),
    pred = rnorm(40)
  )
  m <- compute_metrics(df)
  expect_named(m, c("n", "rmse", "mae", "bias", "r"))
  expect_equal(m$n, 40L)
  expect_equal(m$rmse, sqrt(mean((df$pred - df$obs)^2)))
  expect_equal(m$mae,  mean(abs(df$pred - df$obs)))
  expect_equal(m$bias, mean(df$pred - df$obs))
})

test_that("compute_metrics groups by `by`", {
  df <- data.frame(
    target = rep(c("a", "b"), each = 20),
    obs    = rnorm(40),
    pred   = rnorm(40)
  )
  out <- compute_metrics(df, by = "target")
  expect_equal(nrow(out), 2L)
  expect_setequal(out$target, c("a", "b"))
})
