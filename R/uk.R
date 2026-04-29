#' Fit a universal kriging model for one pollutant column
#'
#' Wraps [gstat::variogram()] and [gstat::fit.variogram()] with a small set
#' of sensible defaults and a fallback strategy for the cases where
#' automatic Stein-Matern fits collapse on small Korean monitoring
#' networks. The companion legacy scripts hand-tuned variogram families
#' and starting values for problematic days; this function exposes those
#' overrides as arguments so the choices can be inspected and version
#' controlled.
#'
#' @param data An `sf` POINT layer or `data.frame` with `X`, `Y` columns
#'   plus the response column.
#' @param target Character; name of the response column.
#' @param trend Trend specification. Either `"const"` (ordinary kriging,
#'   `~ 1`), `"coords"` (universal kriging on coordinates, `~ X + Y`), or a
#'   one-sided formula such as `~ DEM + Road_Dist` (kriging with external
#'   drift).
#' @param cutoff,width Empirical variogram cutoff and bin width, in metres.
#' @param model Initial variogram family for [gstat::vgm()]. Default
#'   `"Ste"` (Matern with Stein parameterisation).
#' @param psill,nugget,range Optional starting values; if `NULL`,
#'   `gstat`'s defaults are used.
#' @param fit_kappa,fit_method Forwarded to [gstat::fit.variogram()].
#' @param crs CRS for the response data when `data` is a plain
#'   `data.frame`. Ignored if `data` is already `sf`.
#'
#' @return A list with class `"aqs_uk"` containing the empirical
#'   variogram (`emp`), the fitted model (`model`), the formula,
#'   the training data as `sf`, and the call.
#' @export
fit_uk <- function(data,
                   target,
                   trend = c("const", "coords"),
                   cutoff = 30000,
                   width = 3000,
                   model = "Ste",
                   psill = NULL,
                   nugget = NULL,
                   range = NULL,
                   fit_kappa = TRUE,
                   fit_method = 6,
                   crs = 5181) {
  if (is.character(trend) && length(trend) > 1L) trend <- match.arg(trend)
  if (!inherits(data, "sf")) data <- df_to_sf(data, crs = crs)
  if (!target %in% names(data)) {
    cli::cli_abort("Target {.val {target}} not in data.")
  }
  data <- data[!is.na(data[[target]]), , drop = FALSE]

  trend_form <- if (inherits(trend, "formula")) {
    stats::as.formula(paste(target, paste(deparse(trend), collapse = " ")))
  } else if (identical(trend, "const")) {
    stats::as.formula(paste(target, "~ 1"))
  } else if (identical(trend, "coords")) {
    stats::as.formula(paste(target, "~ X + Y"))
  } else {
    cli::cli_abort("Unrecognised `trend`.")
  }

  emp <- gstat::variogram(trend_form, data = data,
                          cutoff = cutoff, width = width)
  init <- gstat::vgm(psill = psill %||% NA, model = model,
                     range = range %||% NA, nugget = nugget %||% NA)
  fit <- tryCatch(
    gstat::fit.variogram(emp, model = init,
                         fit.kappa = fit_kappa,
                         fit.method = fit_method),
    error = function(e) NULL,
    warning = function(w) {
      suppressWarnings(
        gstat::fit.variogram(emp, model = init,
                             fit.kappa = fit_kappa,
                             fit.method = fit_method)
      )
    }
  )
  if (is.null(fit)) {
    cli::cli_warn(
      "Variogram fit failed for {.val {target}}; falling back to spherical."
    )
    fallback <- gstat::vgm(psill = psill %||% stats::var(data[[target]], na.rm = TRUE),
                           model = "Sph",
                           range = range %||% (cutoff / 2),
                           nugget = nugget %||% 0)
    fit <- gstat::fit.variogram(emp, model = fallback)
  }

  structure(
    list(
      target  = target,
      formula = trend_form,
      emp     = emp,
      model   = fit,
      data    = data,
      call    = match.call()
    ),
    class = "aqs_uk"
  )
}

#' Predict a kriging surface onto a regular grid
#'
#' @param object Result from [fit_uk()].
#' @param newgrid An `sf`, [terra::SpatRaster], or `data.frame` with
#'   `X`, `Y` columns indicating prediction locations. Rasters are
#'   converted to their cell centres.
#'
#' @return A list with elements `pred` (named numeric vector or column),
#'   `var` (kriging variance) and `raster` ([terra::SpatRaster] aligned
#'   with `newgrid` when a raster was passed).
#' @export
predict_uk <- function(object, newgrid) {
  stopifnot(inherits(object, "aqs_uk"))
  is_raster <- inherits(newgrid, "SpatRaster")

  if (is_raster) {
    template <- newgrid
    cell_xy <- terra::xyFromCell(template, seq_len(terra::ncell(template)))
    grid_sf <- sf::st_as_sf(
      data.frame(X = cell_xy[, 1], Y = cell_xy[, 2]),
      coords = c("X", "Y"),
      crs = sf::st_crs(object$data)
    )
    grid_sf$X <- cell_xy[, 1]
    grid_sf$Y <- cell_xy[, 2]
  } else if (inherits(newgrid, "sf")) {
    grid_sf <- newgrid
    if (!all(c("X", "Y") %in% names(grid_sf))) {
      coords <- sf::st_coordinates(grid_sf)
      grid_sf$X <- coords[, 1]
      grid_sf$Y <- coords[, 2]
    }
  } else {
    grid_sf <- df_to_sf(newgrid, crs = sf::st_crs(object$data))
  }

  k <- gstat::krige(formula = object$formula,
                    locations = object$data,
                    newdata = grid_sf,
                    model = object$model,
                    debug.level = 0)
  out <- list(
    pred = k$var1.pred,
    var  = k$var1.var
  )
  if (is_raster) {
    r_pred <- terra::setValues(template, k$var1.pred)
    r_var  <- terra::setValues(template, k$var1.var)
    names(r_pred) <- object$target
    names(r_var)  <- paste0(object$target, "_var")
    out$raster     <- r_pred
    out$raster_var <- r_var
  }
  out
}

# Internal helper.
`%||%` <- function(a, b) if (is.null(a) || is.na(a[1])) b else a
