#' Faceted plot of fitted variograms
#'
#' Replaces the `gridExtra::grid.arrange()` block in the legacy scripts.
#'
#' @param models A list of `aqs_uk` objects (e.g. the `models` element of
#'   [predict_surface_batch()] when `method = "uk"`).
#' @param ncol Number of facet columns.
#'
#' @return A `ggplot` object.
#' @export
plot_variogram_grid <- function(models, ncol = 5) {
  bits <- lapply(models, function(m) {
    emp <- m$emp
    fitted <- gstat::variogramLine(m$model, maxdist = max(emp$dist))
    list(
      emp = data.frame(target = m$target, dist = emp$dist,
                       gamma = emp$gamma, np = emp$np),
      fit = data.frame(target = m$target, dist = fitted$dist,
                       gamma = fitted$gamma)
    )
  })
  emp_df <- do.call(rbind, lapply(bits, `[[`, "emp"))
  fit_df <- do.call(rbind, lapply(bits, `[[`, "fit"))
  ggplot2::ggplot() +
    ggplot2::geom_point(data = emp_df,
                        ggplot2::aes(x = .data$dist, y = .data$gamma,
                                     size = .data$np),
                        alpha = 0.7) +
    ggplot2::geom_line(data = fit_df,
                       ggplot2::aes(x = .data$dist, y = .data$gamma),
                       colour = "steelblue", linewidth = 0.7) +
    ggplot2::scale_size_continuous(range = c(0.5, 3)) +
    ggplot2::facet_wrap(~ target, ncol = ncol) +
    ggplot2::labs(x = "Distance (m)", y = "Semivariance",
                  size = "Pairs") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 9))
}

#' Faceted choropleth of a multi-layer pollutant surface
#'
#' Reproduces the legacy `geom_tile + facet_wrap` plot, but driven by a
#' [terra::SpatRaster] rather than wide data frames. Optionally overlays
#' a city boundary and per-facet summary annotations.
#'
#' @param surface Multi-layer [terra::SpatRaster].
#' @param boundary Optional `sf` polygon to overlay.
#' @param palette Diverging brewer palette name.
#' @param limits Numeric length-2 vector of fill limits.
#' @param annotate If `TRUE`, prints per-facet `mean / sd` in the corner.
#' @param ncol Facet columns.
#'
#' @return A `ggplot` object.
#' @export
plot_surface_facet <- function(surface,
                               boundary = NULL,
                               palette = "Spectral",
                               limits = c(0, 150),
                               annotate = TRUE,
                               ncol = 8) {
  df <- terra::as.data.frame(surface, xy = TRUE, na.rm = FALSE)
  long <- tidyr::pivot_longer(
    df, cols = -c("x", "y"),
    names_to = "target", values_to = "value"
  )
  stats_df <- long |>
    dplyr::group_by(.data$target) |>
    dplyr::summarise(
      mean = round(mean(.data$value, na.rm = TRUE), 1),
      sd   = round(stats::sd(.data$value, na.rm = TRUE), 1),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(long, ggplot2::aes(.data$x, .data$y,
                                          fill = .data$value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_distiller(palette = palette,
                                  na.value = NA, limits = limits) +
    ggplot2::coord_equal() +
    ggplot2::facet_wrap(~ target, ncol = ncol) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 11)
    )
  if (!is.null(boundary)) {
    p <- p + ggplot2::geom_sf(data = boundary, fill = NA,
                              colour = "black", inherit.aes = FALSE,
                              linewidth = 0.4)
  }
  if (annotate) {
    p <- p +
      ggplot2::geom_text(
        data = stats_df,
        ggplot2::aes(x = -Inf, y = -Inf,
                     label = paste0("mean=", .data$mean,
                                    "  sd=", .data$sd)),
        hjust = -0.05, vjust = -0.5, size = 3,
        inherit.aes = FALSE
      )
  }
  p
}
