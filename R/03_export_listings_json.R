# ==============================================================================
# 03_export_listings_json.R
# Converts the cleaned RentCast CSV into listings.json, shaped to match the
# `LISTINGS` array the site's JS expects. Run this after 02_clean_rentcast.R.
# Makes zero API calls — safe to re-run any time.
#
# IMPORTANT: RentCast's /properties data is property *records*
# (characteristics + last sale), not confirmed active "for sale" listings.
# The `status` field below is intentionally explicit about that, and
# index.html should label these as "known properties," not "for sale,"
# until real listing status exists (e.g. via owner self-submission).
# ==============================================================================

library(dplyr)
library(jsonlite)

IN_FILE  <- "data/processed/alamosa_properties.csv"
OUT_FILE <- "listings.json"   # written to repo root, next to index.html

if (!file.exists(IN_FILE)) {
  stop("No cleaned data found at ", IN_FILE, " — run 02_clean_rentcast.R first.")
}

properties <- read.csv(IN_FILE, stringsAsFactors = FALSE)

format_price <- function(p) {
  if (is.na(p) || p == 0) return("Price unavailable")
  paste0("$", format(round(p), big.mark = ","))
}

format_meta <- function(beds, baths, sqft) {
  parts <- c()
  if (!is.na(beds))  parts <- c(parts, paste0(beds, " bd"))
  if (!is.na(baths)) parts <- c(parts, paste0(baths, " ba"))
  if (!is.na(sqft))  parts <- c(parts, paste0(format(sqft, big.mark=","), " sqft"))
  paste(parts, collapse = " · ")
}

listings <- properties %>%
  mutate(
    id     = row_number() - 1,
    price  = sapply(last_sale_price, format_price),
    meta   = mapply(format_meta, bedrooms, bathrooms, sqft),
    tags   = I(lapply(property_type, function(t) {
      if (is.na(t) || t == "") list() else list(t)
    })),
    status = "known_property"   # explicitly NOT "for_sale" — see note above
  ) %>%
  select(id, addr = address, price, meta, tags, status,
         last_sale_date, year_built)

write_json(listings, OUT_FILE, auto_unbox = TRUE, pretty = TRUE, na = "null")

message("Wrote ", nrow(listings), " properties to ", OUT_FILE,
        "\nRemember: these are property records, not confirmed active listings.")
