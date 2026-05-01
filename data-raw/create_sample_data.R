# Generate the small sample data that ships with the package.
#
# Run this ONCE from the package root after setting
# AQSURFACE_DATA_DIR (see ?aqs_data_dir):
#
#   devtools::load_all()
#   source("data-raw/create_sample_data.R")
#
# The script:
#   1. Reads the full pm10.RData and stations_10km.shp from
#      AQSURFACE_DATA_DIR.
#   2. Subsets to PM10, January S1 (10 days x 2 halves = 20 columns)
#      because that's the worked example used in the vignette.
#   3. Writes four lazy-loaded objects to data/:
#        - pm10_jan_s1         (background / Fixed monitors)
#        - pm10_jan_s1_road    (Road monitors)
#        - stations_demo       (76 monitors, sf, with station_type factor)
#        - ratio_demo          (20-row ratio table for Jan S1)
#   4. Compresses with xz so the bundle stays small.
#
# Re-running is idempotent. The output objects are documented in
# R/data-doc.R.

suppressPackageStartupMessages({
  library(sf)
  library(usethis)
})

stopifnot(packageVersion("aqsurface") >= "0.0.1")

# ---- Resolve paths -------------------------------------------------------
data_dir <- aqs_data_dir()
message("Reading raw archives from: ", data_dir)

raw <- read_pollutant_rdata(file.path(data_dir, "pm10.RData"),
                            pollutant = "pm10")

# ---- Pick one decade (Jan S1) -------------------------------------------
jan_s1     <- decade_columns("pm10", month = 1, decade = "S1")
keep_cols  <- c("X", "Y", "Station.ID", jan_s1)
missing_bk <- setdiff(jan_s1, names(raw$win_bk))
if (length(missing_bk) > 0L) {
  stop("Background data missing columns: ",
       paste(missing_bk, collapse = ", "))
}

pm10_jan_s1      <- raw$win_bk[, keep_cols]
pm10_jan_s1_road <- raw$win_rd[, keep_cols]

# ---- Same decade for NO2 ------------------------------------------------
no2_path <- file.path(data_dir, "no2.RData")
if (file.exists(no2_path)) {
  raw_no2     <- read_pollutant_rdata(no2_path, pollutant = "no2")
  jan_s1_no2  <- decade_columns("no2", month = 1, decade = "S1")
  keep_no2    <- c("X", "Y", "Station.ID", jan_s1_no2)
  if (all(jan_s1_no2 %in% names(raw_no2$win_bk))) {
    no2_jan_s1      <- raw_no2$win_bk[, keep_no2]
    no2_jan_s1_road <- raw_no2$win_rd[, keep_no2]
  } else {
    no2_jan_s1 <- no2_jan_s1_road <- NULL
    warning("NO2 archive missing some Jan S1 columns; skipping.")
  }
} else {
  no2_jan_s1 <- no2_jan_s1_road <- NULL
}

# ---- Stations ------------------------------------------------------------
stations_demo <- make_station_sf(file.path(data_dir, "stations_10km.shp"))

# ---- Seoul administrative boundary --------------------------------------
# Provides a backdrop for figure plots (e.g. fig-stations in paper/). We
# simplify the polygon to keep the bundle small (~20 KB) while preserving
# the visual outline.
seoul_path <- file.path(data_dir, "Seoul_City.shp")
if (file.exists(seoul_path)) {
  seoul_boundary <- sf::read_sf(seoul_path)
  seoul_boundary <- sf::st_transform(seoul_boundary, 5181)
  seoul_boundary <- sf::st_simplify(seoul_boundary,
                                    dTolerance = 50,
                                    preserveTopology = TRUE)
} else {
  seoul_boundary <- NULL
  warning("Seoul_City.shp not found; seoul_boundary will not ship.")
}

# ---- Ratio rows for the same decade -------------------------------------
# The legacy ratio table uses unpadded day numbers (pm10_1_5_day) while
# the data columns use zero-padded ones (pm10_1_05_day). Normalise so
# downstream joins on `target == jan_s1` succeed.
ratio_demo <- raw$win_ratio
ratio_demo$Dates <- aqsurface:::normalise_date_keys(ratio_demo$Dates)
ratio_demo <- ratio_demo[ratio_demo$Dates %in% jan_s1, ]
ratio_demo <- ratio_demo[match(jan_s1, ratio_demo$Dates), , drop = FALSE]

# ---- Save ----------------------------------------------------------------
usethis::use_data(
  pm10_jan_s1,
  pm10_jan_s1_road,
  stations_demo,
  ratio_demo,
  overwrite = TRUE,
  compress  = "xz"
)
if (!is.null(seoul_boundary)) {
  usethis::use_data(seoul_boundary, overwrite = TRUE, compress = "xz")
}
if (!is.null(no2_jan_s1)) {
  usethis::use_data(no2_jan_s1, no2_jan_s1_road,
                    overwrite = TRUE, compress = "xz")
}

message("Done. Bundle sizes:")
print(file.info(list.files("data", full.names = TRUE))[
  , "size", drop = FALSE])
