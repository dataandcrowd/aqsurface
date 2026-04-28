# aqsurface

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
plot_variogram_grid() / plot_surface_facet()     # publication-style figures
```

## Status

Step 1 of 4: code consolidation and modernisation (sf / terra / cli /
testthat). Spatial cross-validation, RF / barrier INLA comparators, and
covariate-driven UK / GAM are scheduled for Steps 2 and 3.

## Installation

```r
# Local install while developing
remotes::install_local("aqsurface", build_vignettes = TRUE)
```

## Validation strategy

Fixed monitors are used for fitting; road-side monitors are an
independent held-out set. This replaces the in-sample RMSE used in the
legacy scripts, where predictions were extracted at the same stations
used to fit the variogram, yielding artificially small errors.

## Reproducing one decade

A worked example covering PM10, January S1, is shipped as
`vignettes/pm10-s1-comparison.Rmd`.
