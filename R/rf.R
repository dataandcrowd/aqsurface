#' Fit a random forest baseline for one pollutant column
#'
#' Fast tree-based regression baseline using [ranger::ranger()].
#' Provides a reference point against which UK and GAM are compared in
#' the planned R Journal article. RF naturally handles non-linear
#' interactions between coordinates and station-level covariates
#' (`Road_Dist`, `DEM`, `station_type`), so it is the most
#' appropriate "modern ML" benchmark for the comparison.
#'
#' @param data An `sf` POINT layer or `data.frame` with the response
#'   and predictor columns.
#' @param target Character; response column name.
#' @param predictors Character vector of predictor columns. Defaults
#'   to `c("X", "Y")`. Pass e.g.
#'   `c("X", "Y", "Road_Dist", "DEM", "station_type")` to enable
#'   covariate-driven RF.
#' @param num_trees Number of trees.
#' @param respect_unordered_factors `ranger` argument; default
#'   `"order"` for compact handling of factor predictors.
#' @param ... Extra arguments forwarded to [ranger::ranger()].
#'
#' @return A list with class `"aqs_rf"` containing the fitted ranger
#'   object, the predictor names, the training data and the call.
#' @export
fit_rf <- function(data,
                   target,
                   predictors = c("X", "Y"),
                   num_trees = 500,
                   respect_unordered_factors = "order",
                   ...) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ranger} is required for {.fn fit_rf}.\n
      Install with: {.code install.packages('ranger')}."
    )
  }
  if (inherits(data, "sf")) {
    df <- as.data.frame(sf::st_drop_geometry(data))
  } else {
    df <- as.data.frame(data)
  }
  if (!target %in% names(df)) {
    cli::cli_abort("Target {.val {target}} not in data.")
  }
  miss <- setdiff(predictors, names(df))
  if (length(miss) > 0L) {
    cli::cli_abort("Missing predictor columns: {.val {miss}}")
  }
  df <- df[stats::complete.cases(df[, c(target, predictors), drop = FALSE]),
           , drop = FALSE]

  form <- stats::as.formula(
    paste(target, "~", paste(predictors, collapse = " + "))
  )
  fit <- ranger::ranger(
    formula = form,
    data = df[, c(target, predictors), drop = FALSE],
    num.trees = num_trees,
    respect.unordered.factors = respect_unordered_factors,
    ...
  )

  structure(
    list(
      target     = target,
      predictors = predictors,
      formula    = form,
      fit        = fit,
      data       = df,
      call       = match.call()
    ),
    class = "aqs_rf"
  )
}

#' Predict a random forest surface
#'
#' @param object Result from [fit_rf()].
#' @param newdata An `sf`, [terra::SpatRaster] or `data.frame` with at
#'   least the predictor columns. Rasters work only when all
#'   predictors are `c("X", "Y")` (per-pixel covariates would need
#'   their own raster layers, which is left as future work).
#'
#' @return A list with `pred` and, if `newdata` was a raster,
#'   `raster`.
#' @export
predict_rf <- function(object, newdata) {
  stopifnot(inherits(object, "aqs_rf"))
  if (!requireNamespace("ranger", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ranger} is required.")
  }
  is_raster <- inherits(newdata, "SpatRaster")

  if (is_raster) {
    extra <- setdiff(object$predictors, c("X", "Y"))
    if (length(extra) > 0L) {
      cli::cli_abort(c(
        "Cannot predict on a raster when predictors include {.val {extra}}.",
        "i" = "Use sf points or a data.frame carrying every predictor column."
      ))
    }
    template <- newdata
    cell_xy <- terra::xyFromCell(template, seq_len(terra::ncell(template)))
    nd <- data.frame(X = cell_xy[, 1], Y = cell_xy[, 2])
  } else if (inherits(newdata, "sf")) {
    coords <- sf::st_coordinates(newdata)
    nd <- as.data.frame(sf::st_drop_geometry(newdata))
    nd$X <- coords[, 1]
    nd$Y <- coords[, 2]
  } else {
    nd <- as.data.frame(newdata)
  }

  miss <- setdiff(object$predictors, names(nd))
  if (length(miss) > 0L) {
    cli::cli_abort("newdata missing predictors: {.val {miss}}")
  }
  pr <- stats::predict(object$fit, data = nd[, object$predictors,
                                              drop = FALSE])
  out <- list(pred = pr$predictions)
  if (is_raster) {
    r_pred <- terra::setValues(template, as.numeric(out$pred))
    names(r_pred) <- object$target
    out$raster <- r_pred
  }
  out
}
