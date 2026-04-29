# aqsurface

<!-- badges: start -->
[![R-CMD-check](https://github.com/dataandcrowd/aqsurface/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dataandcrowd/aqsurface/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Reproducible R workflow for comparing universal kriging (`gstat`) and
generalised additive models (`mgcv`) on hourly urban air-quality
monitoring data, written to support a planned R Journal article on PM10
and NO2 interpolation in Seoul.

The package consolidates 60+ legacy scripts (`Code/PM10/*.R`,
`Code/NO2/*.R`) into a single function-based pipeline:

```
read_pollutant_rdata() / load_pollutant()       # ingest legacy archives
make_station_sf() / split_stations()             # train (Fixed) vs holdout (Road)
build_prediction_grid()                          # terra::SpatRaster template
fit_uk()    + predict_uk()                       # UK with gstat
fit_gam()   + predict_gam()                      # GAM with mgcv
predict_surface_batch()                          # loop over decade columns
apply_road_ratio()                               # legacy road-side correction
extract_at_stations() + compute_metrics()        # validation against Road monitors
spatial_kmeans_folds() + cv_predict()            # spatial cross-validation
fit_rf() + predict_rf()                          # ranger-based RF baseline
combine_stations()                               # Fixed + Road with station_type
benchmark_methods(covariates = ...)              # UK/GAM/RF x in-sample/CV/held-out
plot_variogram_grid() / plot_surface_facet()     # publication-style figures
```

## Status

- **Step 1 — done.** Code consolidation, modernisation to sf/terra/cli/testthat,
  Fixed-vs-Road validation split, lazy-loaded sample data.
- **Step 2 — done.** Spatial k-means / block CV, master `benchmark_methods()`
  entry point, three-strategy comparison vignette.
- **Step 3 — done.** Covariate-driven UK (external drift on `Road_Dist` / `DEM`),
  GAM (`extra_terms`), Random Forest baseline (`ranger`), `combine_stations()`
  for Fixed + Road training with `station_type` factor, scenario-comparison vignette.
- **Step 4 — pending.** Paper rewrite in R Journal style.

## Installation

From a fresh R session:

```r
# install.packages("remotes")
remotes::install_github("dataandcrowd/aqsurface", build_vignettes = TRUE)
```

For local development, clone the repo and use `devtools`:

```r
# install.packages("devtools")
devtools::load_all()    # work with the live source
devtools::test()        # run testthat
devtools::check()       # full R CMD check
```

## Pointing at the data

The legacy `pm10.RData`, `no2.RData`, and `stations_10km.shp` are not
shipped with the package. Tell `aqsurface` where to find them by setting
an environment variable, ideally in your user-level `~/.Renviron`:

```
AQSURFACE_DATA_DIR=/Users/you/OneDrive/.../RJournal/Code/Data
```

Then in R:

```r
data_dir <- aqs_data_dir()
stations <- make_station_sf(file.path(data_dir, "stations_10km.shp"))
```

## Validation strategy

Fixed monitors are used for fitting; road-side monitors are an
independent held-out set. This replaces the in-sample RMSE used in the
legacy scripts, where predictions were extracted at the same stations
used to fit the variogram, yielding artificially small errors.

## Reproducing one decade

A worked example covering PM10, January S1, is shipped as
`vignettes/pm10-s1-comparison.Rmd`.

## Citation

If this package supports your work, please cite the in-progress R
Journal article (preprint forthcoming):

> Shin, H. (2026). aqsurface: Spatial Interpolation Surfaces for Urban
> Air Quality Monitoring. R package version 0.0.1.
> https://github.com/dataandcrowd/aqsurface

## Licence

MIT (c) Hyesop Shin.
