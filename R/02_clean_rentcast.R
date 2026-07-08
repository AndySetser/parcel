# ==============================================================================
# 02_clean_rentcast.R
# Reads every cached RentCast JSON file from data/raw/rentcast/ and combines
# them into one clean, analysis-ready CSV. Run this after 01_fetch_rentcast.R.
# This step makes zero API calls — safe to re-run as many times as you want.
# ==============================================================================

library(jsonlite)
library(dplyr)
library(purrr)

RAW_DIR    <- "data/raw/rentcast"
OUT_FILE   <- "data/processed/alamosa_properties.csv"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

json_files <- list.files(RAW_DIR, pattern = "\\.json$", full.names = TRUE)

if (length(json_files) == 0) {
  stop("No cached JSON files found in ", RAW_DIR,
       " — run 01_fetch_rentcast.R first.")
}

parse_one <- function(path) {
  raw <- tryCatch(fromJSON(path, flatten = TRUE), error = function(e) NULL)
  if (is.null(raw)) return(NULL)

  # RentCast's /v1/properties endpoint returns an ARRAY of matching records,
  # even for a single address search — take the first (best) match.
  # flatten=TRUE turns nested objects like features.garage, features.pool,
  # features.coolingType etc into their own flat columns automatically —
  # so we don't have to guess every possible feature name in advance.
  # Verified endpoint/base fields against https://developers.rentcast.io/reference/property-records
  if (is.data.frame(raw)) {
    if (nrow(raw) == 0) return(NULL)
    raw <- raw[1, ]
  } else if (is.list(raw) && !is.null(raw[[1]]) && is.list(raw[[1]])) {
    raw <- as.data.frame(raw[[1]], stringsAsFactors = FALSE)
  }

  get_col <- function(name) if (name %in% names(raw)) raw[[name]][1] else NA

  core <- tibble::tibble(
    address         = get_col("formattedAddress"),
    city            = get_col("city"),
    state           = get_col("state"),
    zip             = get_col("zipCode"),
    bedrooms        = get_col("bedrooms"),
    bathrooms       = get_col("bathrooms"),
    sqft            = get_col("squareFootage"),
    lot_sqft        = get_col("lotSize"),
    year_built      = get_col("yearBuilt"),
    last_sale_price = get_col("lastSalePrice"),
    last_sale_date  = get_col("lastSaleDate"),
    property_type   = get_col("propertyType"),
    source_file     = basename(path)
  )

  # Catch-all: grab EVERY flattened column whose name starts with "features."
  # (or contains water/well, since well-water availability is spotty and
  # sometimes appears outside a features.* namespace depending on county)
  # so nothing available gets silently dropped, even if RentCast's exact
  # naming differs from what we expected.
  feature_cols <- grep("^features\\.|water|well", names(raw),
                        ignore.case = TRUE, value = TRUE)

  if (length(feature_cols) > 0) {
    vals <- sapply(feature_cols, function(cn) raw[[cn]][1])
    present <- !is.na(vals) & vals != "" & vals != "FALSE"
    core$extra_features <- if (any(present)) {
      paste(paste0(gsub("^features\\.", "", feature_cols[present]), ": ",
                    vals[present]), collapse = "; ")
    } else {
      NA_character_
    }
  } else {
    core$extra_features <- NA_character_
  }

  core
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

properties <- map_dfr(json_files, parse_one)

write.csv(properties, OUT_FILE, row.names = FALSE)

message("Wrote ", nrow(properties), " properties to ", OUT_FILE)
