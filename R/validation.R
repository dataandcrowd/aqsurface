#' Extract pollutant predictions and observations at monitoring stations
#'
#' Companion to [predict_surface_batch()] and [apply_road_ratio()].
#' Returns a long-format tibble with one row per (station, target),
#' carrying both observed and predicted values, ready for
#' [compute_metrics()] or `ggplot2`. By default, observations are taken
#' from the same station `sf` (assumes the response columns are present);
#' if not, supply `obs_df` explicitly.
#'
#' @param surface Multi-layer [terra::SpatRaster] of predictions.
#' @param stations An `sf` POINT layer of monitoring sites (training or
#'   held-out). Must carry the response columns when `obs_df` is `NULL`.
#' @param targets Character vector of target column names.
#' @param obs_df Optional long-format `data.frame` with columns
#'   `Station`, `target`, `obs`. If supplied, observations are taken
#'   from this table.
#' @param station_id Column in `stations` to use as a station identifier
#'   (default `"Station"`).
#'
#' @return A tibble with columns
#'   `station`, `target`, `obs`, `pred`, `geometry` (dropped on return),
#'   `station_type` (if present in `stations`).
#' @export
extract_at_stations <- function(surface, stations, targets,
                                obs_df = NULL, station_id = "Station") {
  if (!inherits(surface, "SpatRaster")) {
    cli::cli_abort("`surface` must be a terra::SpatRaster.")
  }
  layers_present <- intersect(targets, names(surface))
  if (length(layers_present) == 0L) {
    cli::cli_abort("None of {.val {targets}} match the surface layers.")
  }
  if (length(setdiff(targets, layers_present)) > 0L) {
    cli::cli_warn("Skipping targets absent from surface: {.val {setdiff(targets, layers_present)}}")
  }

  vec_stations <- terra::vect(stations)
  pred_mat <- terra::extract(surface[[layers_present]], vec_stations)
  pred_mat$ID <- NULL

  obs_long <- if (!is.null(obs_df)) {
    tibble::as_tibble(obs_df)
  } else {
    sd_df <- sf::st_drop_geometry(stations)
    if (!all(layers_present %in% names(sd_df))) {
      cli::cli_abort("Observations missing for some targets; supply `obs_df`.")
    }
    obs <- sd_df[, c(station_id, layers_present), drop = FALSE]
    tidyr::pivot_longer(
      obs,
      cols = layers_present,
      names_to = "target",
      values_to = "obs"
    ) |>
      dplyr::rename(station = !!station_id)
  }

  station_meta <- sf::st_drop_geometry(stations)
  station_meta$.row <- seq_len(nrow(station_meta))
  pred_long <- tidyr::pivot_longer(
    cbind(.row = seq_len(nrow(pred_mat)), pred_mat),
    cols = layers_present,
    names_to = "target",
    values_to = "pred"
  )
  pred_long <- dplyr::left_join(
    pred_long,
    station_meta[, c(".row", station_id,
                     intersect("station_type", names(station_meta)))],
    by = ".row"
  )
  pred_long <- dplyr::rename(pred_long, station = !!station_id)
  pred_long$.row <- NULL

  out <- dplyr::inner_join(pred_long, obs_long,
                           by = c("station", "target"))
  tibble::as_tibble(out)
}

#' Standard validation metrics
#'
#' Computes RMSE, MAE, mean bias, and Pearson correlation, optionally
#' grouped by one or more columns (e.g. `target`, `station_type`).
#'
#' @param df A data frame with columns `obs` and `pred`.
#' @param by Optional character vector of grouping columns.
#'
#' @return A tibble with one row per group.
#' @export
compute_metrics <- function(df, by = NULL) {
  required <- c("obs", "pred")
  if (!all(required %in% names(df))) {
    cli::cli_abort("`df` must have columns {.field {required}}.")
  }
  df <- df[stats::complete.cases(df[, required]), , drop = FALSE]
  summarise_one <- function(x) {
    err <- x$pred - x$obs
    tibble::tibble(
      n     = nrow(x),
      rmse  = sqrt(mean(err^2)),
      mae   = mean(abs(err)),
      bias  = mean(err),
      r     = if (nrow(x) > 1) stats::cor(x$obs, x$pred) else NA_real_
    )
  }
  if (is.null(by)) {
    return(summarise_one(df))
  }
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(~ summarise_one(.x)) |>
    dplyr::ungroup()
}
