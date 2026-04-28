# Small synthetic monitoring network used across tests. The pattern is a
# spatial trend (linear in X, Y) plus Gaussian noise so that both UK and
# GAM should recover the surface roughly correctly.
make_synth_data <- function(n = 40, seed = 1L) {
  set.seed(seed)
  X <- runif(n, 190000, 210000)
  Y <- runif(n, 440000, 470000)
  a <- 50; bx <- 1e-3; by <- -5e-4
  noise <- rnorm(n, 0, 5)
  pm10_1_01_day   <- a + bx * (X - 200000) + by * (Y - 455000) + noise
  pm10_1_01_night <- (a - 5) + bx * (X - 200000) + by * (Y - 455000) +
    rnorm(n, 0, 4)
  data.frame(
    Station = sprintf("S%03d", seq_len(n)),
    X = X, Y = Y,
    pm10_1_01_day   = pm10_1_01_day,
    pm10_1_01_night = pm10_1_01_night
  )
}

make_synth_sf <- function(n = 40, seed = 1L, crs = 5181) {
  df <- make_synth_data(n = n, seed = seed)
  sf::st_as_sf(df, coords = c("X", "Y"), crs = crs, remove = FALSE)
}
