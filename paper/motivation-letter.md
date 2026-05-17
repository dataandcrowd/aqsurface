# Motivation letter for *aqsurface: Spatial Interpolation Methods for Urban Air Quality*

Dear R Journal editors,

We submit the manuscript "aqsurface: Spatial Interpolation
Methods for Urban Air Quality - A Speed-Accuracy Comparison of
Kriging, GAM, and Random Forest on Seoul's Monitoring Network"
for consideration under the **Comparisons and benchmarking**
article track of the R Journal.

The article reports a head-to-head comparison of three CRAN
packages (`gstat`, `mgcv`, `ranger`) on a 76-station Seoul
PM10 / NO2 monitoring network, evaluated under three validation
regimes (in-sample, spatial cross-validation, held-out
road-side stations) and on per-fit wall-clock cost. The
contribution is twofold:

1. **A two-axis (accuracy and speed) framing of the
   spatial-interpolation comparison literature.** Existing
   comparison studies emphasise RMSE in isolation, and rarely
   report the computational cost of each method. We show that
   the three paradigms tested (geostatistics, statistical
   inference, machine learning) are statistically
   indistinguishable in accuracy under spatial CV (within 6%
   RMSE) while differing in per-fit cost by an order of
   magnitude. The speed axis is, in this case, the dimension on
   which method choice actually matters.

2. **A reproducible benchmark harness.** The accompanying
   `aqsurface` R package (released alongside this article at
   <https://github.com/dataandcrowd/aqsurface>) implements the
   comparison as a single function `benchmark_methods()`, which
   returns RMSE, bias, and per-fit time in one long-format
   tibble keyed by (algorithm, strategy, target). The package
   wraps each of `gstat::krige()`, `mgcv::gam()`, and
   `ranger::ranger()` in a uniform fit / predict interface and
   exposes spatial k-means cross-validation with optional
   stratification on station-type to support the comparison.

We also identify two methodological choices made *before*
paradigm selection that explain more of the variance in
reported RMSE than the paradigm itself: the validation strategy
(in-sample versus spatial CV) and the inclusion of a
`station_type` factor as a covariate, which removes a
systematic 4 µg/m³ underprediction at road-side monitors
regardless of paradigm.

The empirical case study uses two pollutants (PM10 and NO2)
across five months (Dec, Jan, Feb, Aug, Sep) and three decade
windows per month, contrasting winter and summer regimes. The
manuscript also offers a station-level geographic disagreement
analysis that locates the largest paradigm divergence at the
western and north-western fringes of the metropolitan area
where the monitoring network thins out and topographic features
violate the kriging stationarity assumption.

**Reproducibility.** All caches required to render the
manuscript are committed to the repository
(`paper/*.rds`, `paper/figures/*.png`,
`inst/extdata/benchmark_*.rds`). Rendering the article from a
fresh clone takes under one minute on a modern laptop, well
within the R Journal's reproduction-time guideline. The script
`paper/figures.R` regenerates the caches (2-3 minutes), and the
full PM10 and NO2 sweeps can be re-run by
`data-raw/run_benchmark_pm10.R` and
`data-raw/run_benchmark_no2.R` (about 5 minutes each). The
legacy raw data archives (`pm10.RData`, `no2.RData`,
`stations_10km.shp`) are deposited on Zenodo at
[DOI to be added once minted] for permanent reference.

**Package status.** The `aqsurface` package is currently at
version 0.0.1 (lifecycle: experimental) and is hosted at
<https://github.com/dataandcrowd/aqsurface>. Submission to CRAN
is planned subject to peer review of the present manuscript;
the package passes `R CMD check --as-cran` with zero errors,
warnings, or notes on macOS, Linux, and Windows × R-release and
R-devel via continuous integration.

**Conflict of interest.** The author declares no competing
interests. This work is not under consideration for publication
elsewhere.

Thank you for your time and consideration.

Sincerely,

Hyesop Shin
School of Environment, University of Auckland
hyesop.shin@auckland.ac.nz
