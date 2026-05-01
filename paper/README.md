# R Journal manuscript: aqsurface

Companion to the [aqsurface](https://github.com/dataandcrowd/aqsurface)
R package.

## Files

```
paper/
├── aqsurface-rjournal.qmd     # Quarto source (recommended)
├── aqsurface-rjournal.Rmd     # R Markdown alternative (rjtools template)
├── figures.R                   # regenerates the cached results + figures
├── benchmark_results.rds       # cached output of figures.R
├── references.bib              # 373 bibliography entries
├── figures/                    # ggsave outputs
└── README.md                   # this file
```

## Quarto build (recommended)

```bash
# One-off
brew install --cask quarto      # or download from https://quarto.org
```

```r
# Step 1: regenerate the cached benchmark results (~30 seconds)
devtools::load_all()
source("paper/figures.R")

# Step 2: render
quarto::quarto_render("paper/aqsurface-rjournal.qmd")
```

Produces both `aqsurface-rjournal.html` and `aqsurface-rjournal.pdf`
side by side. The qmd has a setup chunk that auto-detects whether
aqsurface is installed; if not, it falls back to
`pkgload::load_all("..")` so the document renders without requiring
a prior `R CMD INSTALL`.

## R Markdown build (alternative)

```r
install.packages("rjtools")
devtools::install(".")          # ★ required: subprocess can't see load_all()
source("paper/figures.R")
rmarkdown::render("paper/aqsurface-rjournal.Rmd")
```

The Rmd uses `library(aqsurface)` directly, which means the package
must be installed system-wide before rendering, because rmarkdown
spawns a callr subprocess that does not inherit `devtools::load_all()`
state. The qmd version sidesteps this by using `pkgload::load_all()`
in its setup chunk.

## Updating numbers

`figures.R` writes `benchmark_results.rds`; the manuscript inlines
literal numbers from that file. After any package change, re-run
`figures.R` and update the `tibble::tribble(...)` block in the
"results" section.

A future improvement: replace the inline tribble with a direct
`readRDS("benchmark_results.rds")` read so the numbers update
automatically. We deferred that so the document still renders if
the rds cache is missing.

## Status

- 2026-04-29: first complete draft (Rmd + qmd parallel sources).
  Methods, Results and Discussion sections written; introduction
  draws on the PhD-thesis literature review chapter; numbers come
  from running the package's three benchmark scenarios on the
  shipped Jan S1 demo bundle.
- Next: full benchmark on the OneDrive `pm10.RData` archive
  (`data-raw/run_benchmark_pm10.R`) to test the headline findings
  across five months and two pollutants; rewrite the Discussion
  in light of that wider sweep.
