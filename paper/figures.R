# Reproduce every result reported in `aqsurface-rjournal.Rmd`.
# Run from the package root:
#
#   devtools::load_all()
#   source("paper/figures.R")
#
# The script writes one cached object (paper/benchmark_results.rds)
# that the Rmd loads. Splitting computation from typesetting keeps
# `rmarkdown::render()` fast and deterministic.

suppressPackageStartupMessages({
  library(aqsurface)
  library(sf)
  library(dplyr)
  library(ggplot2)
})

set.seed(1)

# ---- Data ----------------------------------------------------------------
data("pm10_jan_s1",      package = "aqsurface")
data("pm10_jan_s1_road", package = "aqsurface")
data("stations_demo",    package = "aqsurface")

meta <- sf::st_drop_geometry(stations_demo)[, c("Station", "Road_Dist", "DEM")]
attach_meta <- function(df) {
  out <- merge(df, meta, by.x = "Station.ID", by.y = "Station")
  sf::st_as_sf(out, coords = c("X", "Y"), crs = 5181, remove = FALSE)
}
train    <- attach_meta(pm10_jan_s1)
holdout  <- attach_meta(pm10_jan_s1_road)
combined <- combine_stations(train, holdout)

targets <- decade_columns("pm10", month = 1, decade = "S1")

# ---- Three benchmarks ----------------------------------------------------
common <- list(
  targets    = targets,
  algorithms = c("uk", "gam", "rf"),
  cv_folds   = 5,
  seed       = 1,
  progress   = FALSE
)

baseline <- do.call(benchmark_methods,
                    c(common, list(train = train, holdout = holdout)))
with_cov <- do.call(benchmark_methods,
                    c(common, list(train = train, holdout = holdout,
                                   covariates = c("Road_Dist", "DEM"))))
combined_res <- benchmark_methods(
  train       = combined,
  holdout     = NULL,
  targets     = targets,
  algorithms  = c("uk", "gam", "rf"),
  covariates  = "station_type",
  stratify_by = "station_type",
  cv_folds    = 5,
  seed        = 1,
  progress    = FALSE
)

results <- dplyr::bind_rows(
  dplyr::mutate(baseline,     scenario = "baseline (Fixed only)"),
  dplyr::mutate(with_cov,     scenario = "Road_Dist + DEM"),
  dplyr::mutate(combined_res, scenario = "combined + station_type")
)
saveRDS(results, "paper/benchmark_results.rds", compress = "xz")
cat("Saved", nrow(results), "rows to paper/benchmark_results.rds\n")

# ---- Figures -------------------------------------------------------------
fig_dir <- "paper/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Stations layout with Seoul boundary, scale bar and north arrow.
seoul_boundary <- tryCatch({
  e <- new.env(); utils::data("seoul_boundary", package = "aqsurface", envir = e)
  e$seoul_boundary
}, error = function(e) NULL, warning = function(w) NULL)
have_ggspatial <- requireNamespace("ggspatial", quietly = TRUE)

p_stations <- ggplot()
if (!is.null(seoul_boundary)) {
  p_stations <- p_stations +
    geom_sf(data = seoul_boundary,
            fill = "grey95", colour = "grey55", linewidth = 0.4)
}
p_stations <- p_stations +
  geom_sf(data = stations_demo,
          aes(colour = station_type, shape = station_type),
          size = 2.4, stroke = 0.5) +
  scale_colour_manual(values = c("fixed" = "#1f77b4", "road" = "#d62728"),
                      name = "Station type",
                      labels = c("Background (Fixed, n = 57)",
                                 "Road-side (n = 19)")) +
  scale_shape_manual(values = c("fixed" = 16, "road" = 17),
                     name = "Station type",
                     labels = c("Background (Fixed, n = 57)",
                                "Road-side (n = 19)")) +
  coord_sf(crs = 5181, datum = NA)

if (have_ggspatial) {
  p_stations <- p_stations +
    ggspatial::annotation_scale(
      location = "bl", width_hint = 0.25, line_width = 0.4,
      text_cex = 0.7
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = grid::unit(1.0, "cm"), width = grid::unit(0.8, "cm"),
      style = ggspatial::north_arrow_minimal()
    )
}

p_stations <- p_stations +
  theme_minimal(base_size = 11) +
  theme(panel.grid = ggplot2::element_blank(),
        axis.text  = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom",
        plot.background = ggplot2::element_rect(fill = "white", colour = NA))
ggsave(file.path(fig_dir, "stations.png"), p_stations,
       width = 6, height = 5, dpi = 150)

# Headline RMSE comparison
p_strategy <- results |>
  dplyr::filter(target == "ALL", scenario == "baseline (Fixed only)") |>
  ggplot(aes(x = strategy, y = rmse, fill = algorithm)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = NULL, y = expression(RMSE ~ "(" * mu * "g/" * m^3 * ")"),
       title = "Validation strategy dwarfs algorithm choice",
       subtitle = "PM10, January 2014, S1") +
  theme_bw()
ggsave(file.path(fig_dir, "strategy.png"), p_strategy,
       width = 7, height = 4, dpi = 150)

# Bias decomposition
p_bias <- results |>
  dplyr::filter(target == "ALL") |>
  ggplot(aes(x = strategy, y = bias, fill = algorithm)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  facet_wrap(~ scenario) +
  labs(x = NULL, y = expression(Bias ~ "(" * mu * "g/" * m^3 * ")"),
       title = "Adding station_type to a combined network removes the road-side bias") +
  theme_bw()
ggsave(file.path(fig_dir, "bias.png"), p_bias,
       width = 9, height = 4, dpi = 150)

# Per-fit wall-clock cost (the third axis of the comparison)
p_speed <- results |>
  dplyr::filter(target == "ALL", strategy == "spatial-cv",
                !is.na(time_s)) |>
  ggplot(aes(x = algorithm, y = time_s, fill = algorithm)) +
  geom_col(width = 0.6) +
  facet_wrap(~ scenario) +
  scale_y_continuous(trans = "log10") +
  labs(x = NULL, y = "Median per-fit time (s, log scale)",
       title = "Three paradigms: indistinguishable accuracy, order-of-magnitude cost difference") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(file.path(fig_dir, "speed.png"), p_speed,
       width = 9, height = 4, dpi = 150)

# ---- Boxplot: paradigm spread across the full PM10/NO2 sweep ------------
# Reads the 30-cell sweep stored in inst/extdata. Each box pools 30
# (month, decade, target column) cells per (paradigm, strategy).
boxplot_data <- tryCatch({
  pm10 <- readRDS(system.file("extdata", "benchmark_pm10.rds",
                              package = "aqsurface"))
  no2  <- readRDS(system.file("extdata", "benchmark_no2.rds",
                              package = "aqsurface"))
  dplyr::bind_rows(
    dplyr::mutate(pm10, pollutant = "PM10"),
    dplyr::mutate(no2,  pollutant = "NO2")
  ) |>
    dplyr::filter(target != "ALL", strategy != "in-sample",
                  !is.na(rmse))
}, error = function(e) NULL)

if (!is.null(boxplot_data) && nrow(boxplot_data) > 0L) {
  p_box <- boxplot_data |>
    ggplot(aes(x = algorithm, y = rmse, fill = algorithm)) +
    geom_boxplot(outlier.size = 0.6, alpha = 0.7) +
    facet_grid(pollutant ~ strategy, scales = "free_y") +
    labs(x = NULL,
         y = expression(RMSE ~ "(" * mu * "g/" * m^3 * ")"),
         title = "Paradigm RMSE distributions across the full sweep",
         subtitle = paste0(nrow(boxplot_data),
                           " (target × cell) observations")) +
    theme_bw() +
    theme(legend.position = "none")
  ggsave(file.path(fig_dir, "boxplot_paradigms.png"), p_box,
         width = 9, height = 5, dpi = 150)
}

# ---- Geographic paradigm disagreement -----------------------------------
# Predict at each station with all three paradigms (in-sample on the
# Jan S1 demo bundle), then compute the per-station SD across paradigms
# averaged over the 20 target columns. A small SD means the three
# paradigms agree at that station; a large SD identifies stations where
# the spatial structure breaks one or more paradigms.
disagree_data <- tryCatch({
  per_station <- function(alg) {
    fits <- lapply(targets, function(tgt) {
      data <- combined[!is.na(combined[[tgt]]), , drop = FALSE]
      if (alg == "uk") {
        f <- fit_uk(data, target = tgt, trend = ~ station_type,
                    cutoff = 30000, width = 3000, model = "Ste")
        predict_uk(f, data)$pred
      } else if (alg == "gam") {
        f <- fit_gam(data, target = tgt, smooth = "tp",
                     extra_terms = "station_type", method = "REML")
        predict_gam(f, data)$pred
      } else {
        f <- fit_rf(data, target = tgt,
                    predictors = c("X", "Y", "station_type"),
                    num_trees = 500)
        predict_rf(f, data)$pred
      }
    })
    do.call(cbind, fits)
  }
  pred_uk  <- per_station("uk")
  pred_gam <- per_station("gam")
  pred_rf  <- per_station("rf")
  station_sd <- apply(
    array(c(pred_uk, pred_gam, pred_rf),
          dim = c(nrow(pred_uk), ncol(pred_uk), 3)),
    c(1, 2), stats::sd
  )
  combined |>
    dplyr::mutate(paradigm_sd = rowMeans(station_sd, na.rm = TRUE))
}, error = function(e) NULL)

if (!is.null(disagree_data)) {
  seoul_b <- tryCatch({
    e <- new.env()
    utils::data("seoul_boundary", package = "aqsurface", envir = e)
    e$seoul_boundary
  }, error = function(e) NULL)

  p_dis <- ggplot()
  if (!is.null(seoul_b)) {
    p_dis <- p_dis +
      geom_sf(data = seoul_b, fill = "grey95",
              colour = "grey55", linewidth = 0.4)
  }
  p_dis <- p_dis +
    geom_sf(data = disagree_data,
            aes(colour = paradigm_sd, shape = station_type),
            size = 2.5) +
    scale_colour_viridis_c(
      option = "magma", direction = -1,
      name = expression(paste("Paradigm SD ", "(", mu, "g/m"^3, ")"))
    ) +
    scale_shape_manual(values = c("fixed" = 16, "road" = 17),
                       name = NULL) +
    coord_sf(crs = 5181, datum = NA) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank(),
          legend.position = "right")
  ggsave(file.path(fig_dir, "paradigm_disagreement_map.png"), p_dis,
         width = 7, height = 5, dpi = 150)
  saveRDS(sf::st_drop_geometry(disagree_data),
          file.path("paper", "disagreement_data.rds"))
}

# ---- Three-paradigm interpolated surfaces, both pollutants ------------
# For each pollutant we (i) pick the target column with the largest
# station-level spread (= most spatial structure to interpolate),
# (ii) fit UK / GAM / RF on that column, (iii) plot side by side with
# contour lines. The two pollutants are stacked via patchwork so each
# row keeps its own concentration scale (PM10 in µg/m³, NO2 in ppb).

build_surface_panel <- function(pollutant, combined_pp,
                                seoul_b, fig_label) {
  cands <- intersect(decade_columns(pollutant, 1, "S1"),
                     names(combined_pp))
  if (length(cands) == 0L) return(NULL)
  spread <- vapply(cands, function(t) {
    v <- combined_pp[[t]]
    if (all(is.na(v))) NA_real_ else stats::sd(v, na.rm = TRUE)
  }, numeric(1))
  tgt <- names(which.max(spread))
  message("[", pollutant, "] surface day = ", tgt,
          " (sd = ", round(spread[tgt], 2), ")")
  grid <- build_prediction_grid(combined_pp, n_x = 120, n_y = 120)
  pred_one <- function(alg) {
    tryCatch({
      if (alg == "uk") {
        # Linear (X, Y) trend so the surface keeps a spatial
        # gradient even when the empirical variogram collapses to
        # a near-nugget structure. Matches the legacy analysis.
        m <- fit_uk(combined_pp, target = tgt, trend = "coords",
                    cutoff = 30000, width = 3000, model = "Ste")
        predict_uk(m, grid)$raster
      } else if (alg == "gam") {
        m <- fit_gam(combined_pp, target = tgt,
                     smooth = "tp", method = "REML")
        predict_gam(m, grid)$raster
      } else {
        m <- fit_rf(combined_pp, target = tgt,
                    predictors = c("X", "Y"), num_trees = 500)
        predict_rf(m, grid)$raster
      }
    }, error = function(e) {
      message("[", pollutant, "] ", alg, " failed: ",
              conditionMessage(e))
      NULL
    })
  }
  rs <- list(UK = pred_one("uk"),
             GAM = pred_one("gam"),
             RF = pred_one("rf"))
  rs <- rs[!vapply(rs, is.null, logical(1))]
  if (length(rs) == 0L) return(NULL)
  surf_df <- dplyr::bind_rows(lapply(names(rs), function(nm) {
    df <- terra::as.data.frame(rs[[nm]], xy = TRUE, na.rm = FALSE)
    names(df)[3] <- "value"
    df$paradigm <- nm
    df
  }))
  surf_df$paradigm <- factor(surf_df$paradigm,
                             levels = c("UK", "GAM", "RF"))
  unit_lab <- if (pollutant == "pm10")
    expression(PM[10] ~ "(" * mu * "g/" * m^3 * ")") else
    expression(NO[2] ~ "(ppb)")
  fill_lim <- range(surf_df$value, na.rm = TRUE)
  contour_breaks <- pretty(fill_lim, n = 8)

  p <- ggplot(surf_df, aes(x, y)) +
    geom_raster(aes(fill = value)) +
    geom_contour(aes(z = value),
                 colour = "grey20", linewidth = 0.25,
                 breaks = contour_breaks, alpha = 0.6)
  if (!is.null(seoul_b)) {
    p <- p + geom_sf(data = seoul_b, fill = NA,
                     colour = "grey25", linewidth = 0.4,
                     inherit.aes = FALSE)
  }
  p <- p +
    geom_sf(data = combined_pp, aes(shape = station_type),
            colour = "black", size = 0.9, inherit.aes = FALSE) +
    scale_fill_distiller(palette = "Spectral", name = unit_lab,
                         limits = fill_lim) +
    scale_shape_manual(values = c("fixed" = 16, "road" = 17),
                       name = NULL,
                       labels = c("Background", "Road-side")) +
    facet_wrap(~ paradigm) +
    coord_sf(crs = 5181, datum = NA) +
    labs(tag = fig_label) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank(),
          legend.position = "right",
          plot.tag = element_text(face = "bold"),
          plot.tag.position = c(0.01, 0.97),
          strip.text = element_text(face = "bold"))
  list(plot = p, target = tgt, sd = unname(spread[tgt]))
}

# Combined NO2 sf if NO2 demo data shipped
combined_no2 <- tryCatch({
  e <- new.env()
  utils::data("no2_jan_s1", "no2_jan_s1_road", package = "aqsurface",
              envir = e)
  if (!is.null(e$no2_jan_s1) && !is.null(e$no2_jan_s1_road)) {
    n_train   <- attach_meta(e$no2_jan_s1)
    n_holdout <- attach_meta(e$no2_jan_s1_road)
    combine_stations(n_train, n_holdout)
  } else NULL
}, error = function(e) NULL, warning = function(w) NULL)

panels <- list(
  pm10 = build_surface_panel("pm10", combined,    seoul_boundary, "(a) PM10"),
  no2  = if (!is.null(combined_no2))
    build_surface_panel("no2", combined_no2, seoul_boundary, "(b) NO2") else NULL
)

if (!is.null(panels$pm10) && !is.null(panels$pm10$plot)) {
  if (!is.null(panels$no2) && !is.null(panels$no2$plot)) {
    if (requireNamespace("patchwork", quietly = TRUE)) {
      final_plot <- patchwork::wrap_plots(
        panels$pm10$plot, panels$no2$plot, ncol = 1
      )
      fig_h <- 9
    } else {
      message("install patchwork to stack the PM10 + NO2 panels; ",
              "using PM10 only for now.")
      final_plot <- panels$pm10$plot
      fig_h <- 4.5
    }
  } else {
    message("NO2 panel unavailable; saving PM10-only surface.")
    final_plot <- panels$pm10$plot
    fig_h <- 4.5
  }
  ggsave(file.path(fig_dir, "surface_comparison.png"), final_plot,
         width = 11, height = fig_h, dpi = 150)
  saveRDS(
    list(pm10 = list(target = panels$pm10$target,
                     sd     = panels$pm10$sd),
         no2  = if (!is.null(panels$no2))
           list(target = panels$no2$target,
                sd     = panels$no2$sd) else NULL),
    file = file.path("paper", "surface_target.rds")
  )
}

cat("Saved figures to", fig_dir, "\n")
