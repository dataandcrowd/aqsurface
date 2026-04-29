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

# Internal: normalise date-style identifiers so the day component is
# always two digits and zero-padded. The legacy archives use two
# inconsistent conventions:
#   * data column names:   pm10_1_01_day  (day already zero-padded)
#   * ratio table Dates:   pm10_1_1_day   (day NOT padded)
# This helper rewrites the latter into the former so downstream joins
# work without surprises. Inputs already padded are returned unchanged.
normalise_date_keys <- function(x) {
  x <- as.character(x)
  pat <- "^([A-Za-z0-9]+)_([0-9]+)_([0-9]+)_(day|night)$"
  m <- regmatches(x, regexec(pat, x))
  out <- x
  for (i in seq_along(m)) {
    parts <- m[[i]]
    if (length(parts) == 5L) {
      out[i] <- sprintf("%s_%s_%02d_%s",
                        parts[2], parts[3],
                        as.integer(parts[4]), parts[5])
    }
  }
  out
}

#' Locate the legacy data directory
#'
#' The raw `pm10.RData`, `no2.RData`, and `stations_10km.shp` files are
#' bulky and remain outside the repository (in OneDrive, on a shared
#' network drive, etc.). This helper resolves a single directory path
#' from, in order:
#' \enumerate{
#'   \item the `path` argument if non-`NULL`,
#'   \item the `AQSURFACE_DATA_DIR` environment variable,
#'   \item the `aqsurface.data_dir` R option.
#' }
#'
#' Add a line to `~/.Renviron` such as
#' `AQSURFACE_DATA_DIR=/Users/you/OneDrive/.../Code/Data` so the path is
#' picked up automatically in every R session.
#'
#' @param path Optional explicit path. When supplied it is returned
#'   unchanged (after a `dir.exists()` check).
#' @param required Logical; if `TRUE` (default) and no path resolves,
#'   throw an error rather than returning `NA`.
#'
#' @return A character path, or `NA_character_` when `required = FALSE`
#'   and nothing resolves.
#' @export
#'
#' @examples
#' \dontrun{
#'   Sys.setenv(AQSURFACE_DATA_DIR = "~/OneDrive/.../Code/Data")
#'   aqs_data_dir()
#' }
aqs_data_dir <- function(path = NULL, required = TRUE) {
  candidate <- if (!is.null(path) && nzchar(path)) {
    path
  } else if (nzchar(Sys.getenv("AQSURFACE_DATA_DIR"))) {
    Sys.getenv("AQSURFACE_DATA_DIR")
  } else if (!is.null(getOption("aqsurface.data_dir"))) {
    getOption("aqsurface.data_dir")
  } else {
    NA_character_
  }
  if (is.na(candidate) || !nzchar(candidate)) {
    if (required) {
      cli::cli_abort(c(
        "Cannot resolve a data directory.",
        "i" = "Set the {.envvar AQSURFACE_DATA_DIR} environment variable,",
        "i" = "or pass {.arg path} explicitly."
      ))
    }
    return(NA_character_)
  }
  candidate <- path.expand(candidate)
  if (!dir.exists(candidate)) {
    if (required) {
      cli::cli_abort("Resolved data directory {.path {candidate}} does not exist.")
    }
    return(NA_character_)
  }
  candidate
}
