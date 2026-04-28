#' Read the legacy pm10/no2 RData archive
#'
#' The original analysis stored five winter and four summer objects per
#' pollutant in a single `.RData` archive: `<poll>.win.bk`, `<poll>.win.rd`,
#' `<poll>.sum.bk`, `<poll>.sum.rd`, and ratio tables `<poll>.win.ratio`,
#' `<poll>.sum.ratio`. This function loads the archive in a private
#' environment and returns a tidy named list, so downstream code does not
#' rely on global side-effects of `load()`.
#'
#' @param path Path to `pm10.RData` or `no2.RData`.
#' @param pollutant `"pm10"` or `"no2"`. If `NULL`, inferred from `path`.
#'
#' @return Named list with elements `win_bk`, `win_rd`, `sum_bk`, `sum_rd`,
#'   `win_ratio`, `sum_ratio` (any element absent in the archive is `NULL`).
#' @export
read_pollutant_rdata <- function(path, pollutant = NULL) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  if (is.null(pollutant)) {
    pollutant <- if (grepl("no2", path, ignore.case = TRUE)) "no2" else "pm10"
  }
  e <- new.env()
  load(path, envir = e)
  pull <- function(suffix) {
    nm <- paste0(pollutant, ".", suffix)
    if (exists(nm, envir = e, inherits = FALSE)) get(nm, envir = e) else NULL
  }
  list(
    pollutant  = pollutant,
    win_bk     = pull("win.bk"),
    win_rd     = pull("win.rd"),
    sum_bk     = pull("sum.bk"),
    sum_rd     = pull("sum.rd"),
    win_ratio  = pull("win.ratio"),
    sum_ratio  = pull("sum.ratio")
  )
}

#' Load a pollutant slice as an `sf` POINT layer
#'
#' Convenience wrapper around [read_pollutant_rdata()] that returns a single
#' season + station-type slice as an `sf` object joined with station
#' attributes. This is the typical input for [fit_uk()] or [fit_gam()].
#'
#' @param path Path to `pm10.RData` or `no2.RData`.
#' @param season `"winter"` or `"summer"`.
#' @param station_type `"bk"` (background / fixed) or `"rd"` (road-side).
#' @param stations Optional station `sf` from [make_station_sf()] used to
#'   join attributes such as `Road_Dist` and `DEM`. If `NULL` only the
#'   columns present in the RData object are returned.
#' @param crs EPSG code or CRS object; defaults to `5181` (Korea Central
#'   Belt 2000), which matches the legacy archive.
#'
#' @return An `sf` object with the original day/night columns preserved
#'   and an active POINT geometry.
#' @export
load_pollutant <- function(path,
                           season = c("winter", "summer"),
                           station_type = c("bk", "rd"),
                           stations = NULL,
                           crs = 5181) {
  season <- match.arg(season)
  station_type <- match.arg(station_type)
  raw <- read_pollutant_rdata(path)
  slot <- paste0(if (season == "winter") "win" else "sum", "_", station_type)
  df <- raw[[slot]]
  if (is.null(df)) {
    cli::cli_abort("Slot {.val {slot}} not found in {.path {path}}.")
  }
  if (!is.null(stations)) {
    join_keys <- c("Station.ID" = "Station", "X" = "X", "Y" = "Y")
    stations_df <- sf::st_drop_geometry(stations)
    df <- merge(df, stations_df, by.x = names(join_keys),
                by.y = unname(join_keys), all.x = TRUE)
  }
  df_to_sf(df, crs = crs)
}

#' Build a station `sf` from the legacy 10 km buffer shapefile
#'
#' The legacy `stations_10km.shp` carries an `F/R` column (read by
#' [sf::read_sf()] as `F.R`) distinguishing background / fixed monitors
#' from road-side monitors. This function loads the shapefile, normalises
#' that column to `station_type`, and keeps the covariates `Road_Dist`
#' and `DEM` for downstream use.
#'
#' @param path Path to `stations_10km.shp`.
#'
#' @return An `sf` POINT layer with columns
#'   `Station`, `X`, `Y`, `Long`, `Lat`, `Province`, `City`, `Name`,
#'   `station_type` (factor: `"fixed"` / `"road"`), `Road_Dist`, `DEM`.
#' @export
make_station_sf <- function(path) {
  s <- sf::read_sf(path)
  fr_col <- intersect(c("F.R", "F/R", "FR"), names(s))
  if (length(fr_col) == 0) {
    cli::cli_abort("Column F/R (or F.R) not found in {.path {path}}.")
  }
  s$station_type <- factor(
    tolower(as.character(s[[fr_col[1]]])),
    levels = c("fixed", "road")
  )
  s[[fr_col[1]]] <- NULL
  s
}

#' Split a station layer into training and held-out monitors
#'
#' Returns the `Fixed` (background) monitors as a training set and the
#' `Road` monitors as an independent held-out set. This split mirrors the
#' planned R Journal validation strategy where road-side monitors are
#' deliberately excluded from variogram and GAM fits.
#'
#' @param stations An `sf` from [make_station_sf()].
#' @return A list with elements `train` (Fixed) and `holdout` (Road).
#' @export
split_stations <- function(stations) {
  if (!"station_type" %in% names(stations)) {
    cli::cli_abort("`stations` must have a `station_type` column.")
  }
  list(
    train   = stations[stations$station_type == "fixed", , drop = FALSE],
    holdout = stations[stations$station_type == "road", , drop = FALSE]
  )
}
