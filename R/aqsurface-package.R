#' aqsurface: spatial interpolation surfaces for urban air quality
#'
#' Reproducible workflow for comparing universal kriging ([gstat::krige()])
#' with generalised additive models ([mgcv::gam()]) on hourly urban air
#' pollutant concentrations.
#'
#' Typical pipeline:
#' \enumerate{
#'   \item [read_pollutant_rdata()] / [load_pollutant()] to ingest the
#'         legacy `pm10.RData` / `no2.RData` archives.
#'   \item [make_station_sf()] / [split_stations()] to align stations.
#'   \item [fit_uk()] or [fit_gam()] for one column; [predict_surface_batch()]
#'         to loop over a decade of day/night columns.
#'   \item [apply_road_ratio()] (optional) for road-side correction.
#'   \item [extract_at_stations()] + [compute_metrics()] for validation
#'         against the held-out road monitoring stations.
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom stats as.formula median predict sd
## usethis namespace: end
NULL
