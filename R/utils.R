#' Build a list of pollutant column names for a decade
#'
#' The legacy data archives store one numeric column per day half (day or
#' night), named `<pollutant>_<month>_<dd>_<half>` (e.g. `pm10_1_05_day`).
#' This helper enumerates the columns belonging to a chosen ten-day window
#' (`S1` = days 1-10, `S2` = days 11-20, `S3` = days 21-end of month).
#'
#' @param pollutant Pollutant prefix, typically `"pm10"` or `"no2"`.
#' @param month Integer month (1, 2, 8, 9, 12).
#' @param decade One of `"S1"`, `"S2"`, `"S3"`.
#' @param days_in_month Optional integer; if `NULL`, inferred from `month`
#'   assuming a non-leap year.
#'
#' @return Character vector of column names.
#' @export
#'
#' @examples
#' decade_columns("pm10", month = 1, decade = "S1")
decade_columns <- function(pollutant, month, decade = c("S1", "S2", "S3"),
                           days_in_month = NULL) {
  decade <- match.arg(decade)
  if (is.null(days_in_month)) {
    days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month]
  }
  day_range <- switch(decade,
    S1 = 1:10,
    S2 = 11:20,
    S3 = 21:days_in_month
  )
  halves <- c("day", "night")
  outer_grid <- expand.grid(day = day_range, half = halves,
                            stringsAsFactors = FALSE)
  outer_grid <- outer_grid[order(outer_grid$day,
                                 match(outer_grid$half, halves)), ]
  sprintf("%s_%d_%02d_%s", pollutant, month,
          outer_grid$day, outer_grid$half)
}

#' Build a regular prediction grid over the convex extent of a station set
#'
#' @param stations An `sf` POINT or `data.frame` with `X`, `Y` columns
#'   in the same CRS as the stations.
#' @param n_x,n_y Grid dimensions.
#' @param crs CRS to attach (defaults to that of `stations` if `sf`).
#'
#' @return A [terra::SpatRaster] of `NA` cells covering the extent.
#' @export
build_prediction_grid <- function(stations, n_x = 200, n_y = 200, crs = NULL) {
  if (inherits(stations, "sf")) {
    bb <- sf::st_bbox(stations)
    if (is.null(crs)) crs <- sf::st_crs(stations)$wkt
  } else {
    bb <- c(xmin = min(stations$X), xmax = max(stations$X),
            ymin = min(stations$Y), ymax = max(stations$Y))
  }
  ext <- terra::ext(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"])
  r <- terra::rast(ext, ncols = n_x, nrows = n_y)
  if (!is.null(crs)) terra::crs(r) <- crs
  r
}

# Internal: convert a data.frame to sf, dropping rows with missing coords.
df_to_sf <- function(df, crs = 5181, x = "X", y = "Y") {
  df <- df[stats::complete.cases(df[, c(x, y)]), , drop = FALSE]
  sf::st_as_sf(df, coords = c(x, y), crs = crs, remove = FALSE)
}
