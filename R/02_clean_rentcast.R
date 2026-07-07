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

  # RentCast's /properties endpoint returns a list; adjust field names below
  # once you've seen a real response — this is a reasonable starting shape.
  tibble::tibble(
    address       = raw$formattedAddress %||% NA_character_,
    city          = raw$city %||% NA_character_,
    state         = raw$state %||% NA_character_,
    zip           = raw$zipCode %||% NA_character_,
    bedrooms      = raw$bedrooms %||% NA_real_,
    bathrooms     = raw$bathrooms %||% NA_real_,
    sqft          = raw$squareFootage %||% NA_real_,
    lot_sqft      = raw$lotSize %||% NA_real_,
    year_built    = raw$yearBuilt %||% NA_real_,
    last_sale_price = raw$lastSalePrice %||% NA_real_,
    last_sale_date  = raw$lastSaleDate %||% NA_character_,
    property_type   = raw$propertyType %||% NA_character_,
    source_file     = basename(path)
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

properties <- map_dfr(json_files, parse_one)

write.csv(properties, OUT_FILE, row.names = FALSE)

message("Wrote ", nrow(properties), " properties to ", OUT_FILE)
