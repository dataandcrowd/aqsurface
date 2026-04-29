#' PM10 background concentrations: Seoul, January 1-10
#'
#' A 10-day sample from the legacy Seoul PM10 monitoring archive
#' (winter background / Fixed sites). The columns follow the
#' `pm10_<month>_<dd>_<half>` convention; days are split into day
#' (08:00-19:00) and night (20:00-07:00) means.
#'
#' @format A data frame with 57 rows (one per Fixed monitor) and
#'   23 columns: `X` and `Y` (Korea Central Belt 2000, EPSG:5181),
#'   `Station.ID`, plus 20 numeric columns named
#'   `pm10_1_01_day` ... `pm10_1_10_night` (PM10 in micrograms / m^3).
#' @source Seoul Air Quality Monitoring Network, 2014. Subset
#'   produced by `data-raw/create_sample_data.R`.
"pm10_jan_s1"

#' PM10 road-side concentrations: Seoul, January 1-10
#'
#' Companion to [pm10_jan_s1] for the held-out road-side monitoring
#' stations (used as an independent validation set in the vignette).
#'
#' @format A data frame with 19 rows and the same 23 columns as
#'   [pm10_jan_s1].
#' @source As [pm10_jan_s1].
"pm10_jan_s1_road"

#' Seoul air quality monitoring stations
#'
#' All 76 monitoring stations from the legacy `stations_10km.shp`
#' (a 10 km buffer around the Seoul metropolitan boundary). Includes
#' both fixed background and road-side monitors. The original `F/R`
#' shapefile column has been normalised to a `station_type` factor.
#'
#' @format An `sf` POINT layer (CRS EPSG:5181) with 76 features and
#'   columns `X`, `Y`, `Station`, `Long`, `Lat`, `Province`, `City`,
#'   `Name`, `Road_Dist` (distance to nearest road, m), `DEM`
#'   (elevation, m), `station_type` (factor: "fixed" / "road"),
#'   `geometry`.
#' @source As [pm10_jan_s1].
"stations_demo"

#' Background-to-road concentration ratios: Seoul, January 1-10
#'
#' Empirical ratios used by the legacy post-processing step
#' [apply_road_ratio()] to amplify background-monitor interpolations
#' on road and highway pixels. One row per `Dates` value matching
#' [pm10_jan_s1]'s 20 day/night columns.
#'
#' @format A data frame with 20 rows and 7 columns: `Dates`, `Back`
#'   (mean background concentration), `Road` (mean road-side),
#'   `Road.high` (mean highway), `Period`, `Back.Road.Ratio`,
#'   `Back.High.Ratio`.
#' @source As [pm10_jan_s1].
"ratio_demo"
