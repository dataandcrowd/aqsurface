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

# ---- Stations ------------------------------------------------------------
stations_demo <- make_station_sf(file.path(data_dir, "stations_10km.shp"))

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

message("Done. Bundle sizes:")
print(file.info(list.files("data", full.names = TRUE))[
  , "size", drop = FALSE])
