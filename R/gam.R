#' Fit a thin-plate / tensor-product GAM surface for one pollutant column
#'
#' Wraps [mgcv::gam()] for the standard `y ~ s(X, Y)` (or `te(X, Y)`)
#' model used in the legacy scripts. The estimation method defaults to
#' REML, which the original code switched to ML on a few days; in our
#' experience REML is more stable for the small Korean monitoring sample
#' and is the modern recommendation.
#'
#' @param data An `sf` POINT layer or `data.frame` with `X`, `Y` columns
#'   plus the response.
#' @param target Character; name of the response column.
#' @param smooth `"tp"` for an isotropic thin-plate spline (`s(X, Y)`),
#'   `"te"` for a tensor-product smooth (`te(X, Y)`).
#' @param k Optional basis dimension; passed to the smooth.
#' @param method Smoothing parameter selection method.
#' @param extra_terms Optional character vector of additional terms to
#'   add to the right-hand side, e.g. `c("Road_Dist", "DEM")`.
#'
#' @return A list with class `"aqs_gam"` containing the fitted `gam`
#'   object, the formula, the training data and the call.
#' @export
fit_gam <- function(data,
                    target,
                    smooth = c("tp", "te"),
                    k = NULL,
                    method = "REML",
                    extra_terms = NULL) {
  smooth <- match.arg(smooth)
  if (inherits(data, "sf")) {
    df <- as.data.frame(sf::st_drop_geometry(data))
  } else {
    df <- as.data.frame(data)
  }
  if (!target %in% names(df)) {
    cli::cli_abort("Target {.val {target}} not in data.")
  }
  df <- df[!is.na(df[[target]]), , drop = FALSE]

  smooth_term <- if (smooth == "tp") {
    sprintf("s(X, Y%s)", if (!is.null(k)) sprintf(", k = %d", k) else "")
  } else {
    sprintf("te(X, Y%s)", if (!is.null(k)) sprintf(", k = %d", k) else "")
  }
  rhs <- c(smooth_term, extra_terms)
  form <- stats::as.formula(
    paste(target, "~", paste(rhs, collapse = " + "))
  )
  fit <- mgcv::gam(form, data = df, method = method)

  structure(
    list(
      target  = target,
      formula = form,
      fit     = fit,
      data    = df,
      call    = match.call()
    ),
    class = "aqs_gam"
  )
}

#' Predict a GAM surface onto a regular grid
#'
#' @param object Result from [fit_gam()].
#' @param newgrid An `sf`, [terra::SpatRaster] or `data.frame` with
#'   `X`, `Y` columns. Rasters are converted to cell centres.
#' @param se_fit If `TRUE`, also return prediction standard errors.
#'
#' @return A list with `pred` (and optionally `se`) and, if `newgrid` was
#'   a raster, `raster` and `raster_se`.
#' @export
predict_gam <- function(object, newgrid, se_fit = FALSE) {
  stopifnot(inherits(object, "aqs_gam"))
  is_raster <- inherits(newgrid, "SpatRaster")

  if (is_raster) {
    template <- newgrid
    cell_xy <- terra::xyFromCell(template, seq_len(terra::ncell(template)))
    nd <- data.frame(X = cell_xy[, 1], Y = cell_xy[, 2])
  } else if (inherits(newgrid, "sf")) {
    coords <- sf::st_coordinates(newgrid)
    nd <- data.frame(X = coords[, 1], Y = coords[, 2])
    extra <- setdiff(all.vars(object$formula), c(object$target, "X", "Y"))
    if (length(extra) > 0) {
      for (v in extra) nd[[v]] <- newgrid[[v]]
    }
  } else {
    nd <- as.data.frame(newgrid)
  }

  pr <- stats::predict(object$fit, newdata = nd,
                       type = "response", se.fit = se_fit)
  out <- if (se_fit) list(pred = pr$fit, se = pr$se.fit) else list(pred = pr)

  if (is_raster) {
    # `predict.gam` returns a named numeric vector; on some terra
    # versions the names trip the multi-band setValues dispatch, so
    # strip them with `as.numeric()`.
    r_pred <- terra::setValues(template, as.numeric(out$pred))
    names(r_pred) <- object$target
    out$raster <- r_pred
    if (se_fit) {
      r_se <- terra::setValues(template, as.numeric(out$se))
      names(r_se) <- paste0(object$target, "_se")
      out$raster_se <- r_se
    }
  }
  out
}
