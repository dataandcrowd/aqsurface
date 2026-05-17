# R Journal manuscript: aqsurface

Companion source for the article submitted to the R Journal under
the *Comparisons and benchmarking* track.

## Files

```
paper/
├── aqsurface-rjournal.qmd            Quarto source (recommended)
├── aqsurface-rjournal.Rmd            R Markdown alternative
├── references.bib                    bibliography (cited entries only)
├── figures.R                         regenerates caches (optional)
├── benchmark_results.rds             cached: 3-scenario benchmark (Jan S1 demo)
├── disagreement_data.rds             cached: per-station paradigm SD
├── surface_target.rds                cached: auto-selected representative day
└── figures/
    ├── stations.png
    ├── strategy.png
    ├── bias.png
    ├── speed.png
    ├── boxplot_paradigms.png
    ├── paradigm_disagreement_map.png
    └── surface_comparison.png
```

The two `inst/extdata/benchmark_*.rds` files inside the package
also feed this manuscript (the full 5-month × 3-decade ×
2-pollutant sweep used for `@tbl-robustness` and `@fig-boxplot`).

## Building the manuscript (under one minute)

The manuscript reads only from disk caches. No heavy computation
is required at render time.

```bash
# One-off: install Quarto (https://quarto.org)
brew install --cask quarto
```

```r
# install.packages(c("devtools", "quarto", "rjtools"))
devtools::load_all()                                  # exposes aqsurface
quarto::quarto_render("paper/aqsurface-rjournal.qmd") # < 1 minute
```

Produces both `aqsurface-rjournal.html` and `aqsurface-rjournal.pdf`
side by side. The Quarto setup chunk auto-detects whether
aqsurface is installed; if not, it falls back to
`pkgload::load_all("..")` so the document renders from a clean
clone without `R CMD INSTALL`.

## Regenerating the caches (optional, 2-3 minutes)

Run only when you have changed package code or the demo data.

```r
devtools::load_all()
source("paper/figures.R")    # writes benchmark_results.rds, surface_target.rds,
                             # and all paper/figures/*.png
```

The full PM10 / NO2 sweeps that back `@tbl-robustness` and
`@fig-boxplot` are stored in `inst/extdata/` and only need
re-running if the legacy raw data is updated. Both scripts
expect the `AQSURFACE_DATA_DIR` environment variable to point at
the directory containing `pm10.RData`, `no2.RData`, and
`stations_10km.shp` (see `?aqs_data_dir`).

```r
source("data-raw/run_benchmark_pm10.R")   # ~5 minutes
source("data-raw/run_benchmark_no2.R")    # ~5 minutes
```

## Reproducibility summary for reviewers

| Step | Runtime | Required for paper render? |
|---|---|---|
| `quarto_render("paper/aqsurface-rjournal.qmd")` | ~1 min | yes |
| `source("paper/figures.R")` | 2-3 min | optional (caches in git) |
| `source("data-raw/run_benchmark_pm10.R")` | ~5 min | optional (cache in git) |
| `source("data-raw/run_benchmark_no2.R")` | ~5 min | optional (cache in git) |

Total time to render the manuscript from a clean clone, without
re-running any benchmark, is under one minute. This satisfies the
R Journal guideline that submissions must be reproducible in
under ten minutes.

## R Markdown build (alternative)

The Quarto source is recommended. An R Markdown version is also
provided for reviewers who prefer `rmarkdown::render()`:

```r
install.packages("rjtools")
devtools::install(".")        # required: rmarkdown's callr subprocess
source("paper/figures.R")
rmarkdown::render("paper/aqsurface-rjournal.Rmd")
```

The Rmd uses `library(aqsurface)` directly, so the package must
be installed system-wide before rendering. The Quarto source
sidesteps this by using `pkgload::load_all()`.

## Status

- 2026-05-01: complete draft, R Journal "Comparisons and
  benchmarking" track.
- All caches (`.rds`, `.png`, `.rda`) committed to git.
- Manuscript renders under one minute from a clean clone.
