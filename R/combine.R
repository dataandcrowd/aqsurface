#' Combine training and held-out monitors with a `station_type` factor
#'
#' Stacks two `sf` POINT layers (typically `train` = Fixed monitors
#' and `holdout` = Road monitors) into a single layer ready for
#' covariate-driven modelling. A `station_type` factor column is
#' added with levels `"fixed"` and `"road"`, so it can be used as a
#' covariate by `fit_uk(trend = ~ station_type)`,
#' `fit_gam(extra_terms = "station_type")`, or
#' `fit_rf(predictors = c("X", "Y", "station_type", ...))`.
#'
#' Only columns common to both layers are retained, so this is also
#' a way to drop attributes that exist in only one of the input
#' tables.
#'
#' @param train An `sf` POINT layer of training monitors.
#' @param holdout An `sf` POINT layer of held-out monitors.
#' @param tag Character of length 2: factor labels to attach to
#'   `train` and `holdout` rows respectively. Defaults to
#'   `c("fixed", "road")`.
#'
#' @return An `sf` POINT layer with the union of rows and a
#'   `station_type` factor column.
#' @export
combine_stations <- function(train, holdout,
                             tag = c("fixed", "road")) {
  if (!inherits(train, "sf") || !inherits(holdout, "sf")) {
    cli::cli_abort("Both inputs must be sf POINT layers.")
  }
  if (length(tag) != 2L) {
    cli::cli_abort("`tag` must be of length 2.")
  }
  if (sf::st_crs(train) != sf::st_crs(holdout)) {
    cli::cli_abort("CRS mismatch between train and holdout.")
  }

  common <- intersect(names(train), names(holdout))
  train_keep   <- train[, common, drop = FALSE]
  holdout_keep <- holdout[, common, drop = FALSE]
  train_keep$station_type   <- factor(tag[1], levels = tag)
  holdout_keep$station_type <- factor(tag[2], levels = tag)
  rbind(train_keep, holdout_keep)
}
