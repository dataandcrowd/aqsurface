#' Apply per-column road / highway ratio post-processing to a surface stack
#'
#' Reproduces the legacy ad hoc post-processing where background-monitor
#' interpolations were multiplied, on road and highway pixels, by
#' empirical Back-to-Road and Back-to-Highway ratios computed elsewhere.
#' This step is deliberately kept separate from [fit_uk()] / [fit_gam()]:
#' a future revision is expected to replace it with a covariate
#' (kriging with external drift, or a `by`-factor smooth in `mgcv`).
#'
#' Encoding convention for the road raster (matching the legacy
#' `road_10km_re.tif`):
#' \describe{
#'   \item{0 / NA}{background pixel; surface returned unchanged.}
#'   \item{1}{ordinary road; multiply surface by `Back.Road.Ratio`.}
#'   \item{2}{highway; multiply surface by `Back.High.Ratio`.}
#' }
#'
#' @param surface A multi-layer [terra::SpatRaster] of background
#'   predictions, typically the `pred` element returned by
#'   [predict_surface_batch()]. One layer per target column.
#' @param road_raster A single-layer [terra::SpatRaster] using the
#'   encoding above. Typically a finer-resolution raster than `surface`.
#' @param ratio Data frame with columns `target`,
#'   `Back.Road.Ratio`, `Back.High.Ratio`, providing one row per
#'   `surface` layer. The `target` column must match `names(surface)`.
#' @param template Resolution to return the result at. Either
#'   `"road"` (upsample `surface` to match `road_raster`, the legacy
#'   behaviour) or `"surface"` (downsample `road_raster` to match
#'   `surface`).
#' @param resample_method Passed to [terra::resample()] when changing
#'   the resolution of `surface` (default `"bilinear"`).
#'
#' @return A multi-layer [terra::SpatRaster] of corrected concentrations.
#' @export
apply_road_ratio <- function(surface, road_raster, ratio,
                             template = c("road", "surface"),
                             resample_method = "bilinear") {
  template <- match.arg(template)
  if (!inherits(surface, "SpatRaster")) {
    cli::cli_abort("`surface` must be a terra::SpatRaster.")
  }
  if (!inherits(road_raster, "SpatRaster")) {
    road_raster <- terra::rast(road_raster)
  }
  required <- c("target", "Back.Road.Ratio", "Back.High.Ratio")
  if (!all(required %in% names(ratio))) {
    cli::cli_abort("`ratio` must have columns: {.field {required}}.")
  }

  if (template == "road") {
    surf <- terra::resample(surface, road_raster, method = resample_method)
    rr   <- road_raster
  } else {
    surf <- surface
    rr   <- terra::resample(road_raster, surface[[1]], method = "near")
  }

  out_layers <- vector("list", terra::nlyr(surf))
  names(out_layers) <- names(surf)
  for (i in seq_len(terra::nlyr(surf))) {
    nm <- names(surf)[i]
    row <- ratio[ratio$target == nm, , drop = FALSE]
    if (nrow(row) != 1L) {
      cli::cli_abort("Layer {.val {nm}} has {nrow(row)} matching rows in `ratio`.")
    }
    base <- surf[[i]]
    rd_mask   <- rr == 1
    high_mask <- rr == 2
    base <- terra::ifel(rd_mask,   base * row$Back.Road.Ratio, base)
    base <- terra::ifel(high_mask, base * row$Back.High.Ratio, base)
    names(base) <- nm
    out_layers[[i]] <- base
  }
  out <- terra::rast(out_layers)
  names(out) <- names(surf)
  out
}

#' Reshape the legacy ratio table into a per-target tibble
#'
#' The legacy `pm10.win.ratio` table is keyed by `Dates` strings, but
#' it uses single-digit days (`pm10_1_5_day`) while the matching data
#' columns use two-digit zero-padded days (`pm10_1_05_day`). This
#' helper normalises `Dates` to the zero-padded form and selects the
#' columns that [apply_road_ratio()] expects.
#'
#' @param ratio_df Legacy ratio data frame.
#' @param targets Optional character vector to subset and reorder rows
#'   (also expected in the zero-padded form, as returned by
#'   [decade_columns()]).
#'
#' @return Tibble with columns `target`, `Back.Road.Ratio`,
#'   `Back.High.Ratio`.
#' @export
tidy_ratio_table <- function(ratio_df, targets = NULL) {
  out <- tibble::tibble(
    target = normalise_date_keys(ratio_df$Dates),
    Back.Road.Ratio = as.numeric(ratio_df$Back.Road.Ratio),
    Back.High.Ratio = as.numeric(ratio_df$Back.High.Ratio)
  )
  if (!is.null(targets)) {
    out <- out[match(targets, out$target), , drop = FALSE]
  }
  out
}
