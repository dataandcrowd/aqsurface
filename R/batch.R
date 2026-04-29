#' Predict pollutant surfaces for a batch of day/night columns
#'
#' Loops [fit_uk()] or [fit_gam()] over a vector of target columns and
#' returns a multi-layer [terra::SpatRaster] of predictions plus a
#' tibble of per-column diagnostics.
#'
#' @param data An `sf` POINT layer with `X`, `Y` columns and one response
#'   column per element of `targets`.
#' @param targets Character vector of response columns (typically the
#'   output of [decade_columns()]).
#' @param newgrid Output of [build_prediction_grid()] (a [terra::SpatRaster]).
#' @param algorithm `"uk"` or `"gam"`. (Renamed from `method` to avoid a
#'   clash with `mgcv::gam()`'s own `method` argument when forwarded via
#'   `...`; e.g. `algorithm = "gam", method = "REML"` is now legal.)
#' @param ... Extra arguments passed to [fit_uk()] or [fit_gam()].
#' @param overrides Optional named list mapping `target` -> list of
#'   argument overrides, used to reproduce the per-day variogram tweaks
#'   in the legacy scripts. For example
#'   `list(pm10_1_02_day = list(model = "Sph", psill = 40))`.
#' @param progress Logical; show a `cli` progress bar.
#'
#' @return A list with elements `pred` ([terra::SpatRaster] of predictions,
#'   one layer per target), `var` (kriging variance, NULL for GAM),
#'   `models` (list of fitted model objects), and `diagnostics` (tibble
#'   with `target`, `n_obs`, `mean`, `sd`, `nugget`, `psill`, `range`).
#' @export
predict_surface_batch <- function(data,
                                  targets,
                                  newgrid,
                                  algorithm = c("uk", "gam"),
                                  ...,
                                  overrides = list(),
                                  progress = TRUE) {
  algorithm <- match.arg(algorithm)
  if (!inherits(newgrid, "SpatRaster")) {
    cli::cli_abort("`newgrid` must be a terra::SpatRaster.")
  }
  pred_layers <- vector("list", length(targets))
  var_layers  <- vector("list", length(targets))
  models      <- vector("list", length(targets))
  diags       <- vector("list", length(targets))
  names(pred_layers) <- names(var_layers) <-
    names(models) <- names(diags) <- targets

  if (progress) {
    cli::cli_progress_bar("Fitting surfaces", total = length(targets))
  }
  for (tgt in targets) {
    args <- list(data = data, target = tgt, ...)
    if (!is.null(overrides[[tgt]])) {
      args <- utils::modifyList(args, overrides[[tgt]])
    }
    if (algorithm == "uk") {
      uk_args <- args[intersect(names(args),
                                c("data", "target", "trend", "cutoff",
                                  "width", "model", "psill", "nugget",
                                  "range", "fit_kappa", "fit_method",
                                  "crs"))]
      m <- do.call(fit_uk, uk_args)
      p <- predict_uk(m, newgrid)
      var_layers[[tgt]] <- p$raster_var
      diags[[tgt]] <- single_uk_diag(m)
    } else {
      gam_args <- args[intersect(names(args),
                                 c("data", "target", "smooth", "k",
                                   "method", "extra_terms"))]
      m <- do.call(fit_gam, gam_args)
      p <- predict_gam(m, newgrid)
      diags[[tgt]] <- single_gam_diag(m)
    }
    pred_layers[[tgt]] <- p$raster
    models[[tgt]] <- m
    if (progress) cli::cli_progress_update()
  }
  if (progress) cli::cli_progress_done()

  pred_stack <- terra::rast(pred_layers)
  names(pred_stack) <- targets
  var_stack <- if (algorithm == "uk") terra::rast(var_layers) else NULL
  if (!is.null(var_stack)) names(var_stack) <- paste0(targets, "_var")
  diag_tbl <- do.call(rbind, diags)
  rownames(diag_tbl) <- NULL
  list(
    algorithm   = algorithm,
    pred        = pred_stack,
    var         = var_stack,
    models      = models,
    diagnostics = tibble::as_tibble(diag_tbl)
  )
}

# Internal: extract diagnostics from a single fitted UK / GAM model.
single_uk_diag <- function(m) {
  vg <- m$model
  nug <- vg$psill[vg$model == "Nug"]
  if (length(nug) == 0L) nug <- NA_real_
  partial <- sum(vg$psill[vg$model != "Nug"])
  rng <- vg$range[vg$model != "Nug"][1]
  data.frame(
    target = m$target,
    n_obs  = nrow(m$data),
    mean   = mean(m$data[[m$target]], na.rm = TRUE),
    sd     = stats::sd(m$data[[m$target]], na.rm = TRUE),
    nugget = nug,
    psill  = partial,
    range  = rng,
    stringsAsFactors = FALSE
  )
}

single_gam_diag <- function(m) {
  s <- summary(m$fit)
  data.frame(
    target  = m$target,
    n_obs   = nrow(m$data),
    mean    = mean(m$data[[m$target]], na.rm = TRUE),
    sd      = stats::sd(m$data[[m$target]], na.rm = TRUE),
    edf     = sum(s$edf),
    r_sq    = s$r.sq,
    dev_exp = s$dev.expl,
    stringsAsFactors = FALSE
  )
}
