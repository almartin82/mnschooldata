# Fetch Minnesota school directory data

Downloads and processes school and district directory data from the MDE
Organization Reference Glossary (MDE-ORG). Returns a combined dataset
with school information, including principal and superintendent
contacts.

## Usage

``` r
fetch_directory(end_year = NULL, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  A school year end used as the cache key. Defaults to the current
  school year (July-June). For example, in March 2026 the current school
  year is 2025-26, so end_year defaults to 2026.

- tidy:

  If TRUE (default), returns combined school+district data with
  superintendent info merged into school records. If FALSE, returns a
  list with separate `$schools` and `$districts` data frames.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from MDE-ORG.

## Value

If tidy=TRUE, a data frame with school-level directory data enriched
with district/superintendent info. If tidy=FALSE, a list with:

- `schools`: School directory data with principal contacts

- `districts`: District directory data with superintendent contacts

## Details

MDE-ORG is a live directory – the extract always returns the current
snapshot. There is no historical year parameter. The `end_year`
parameter is used only for cache key labeling.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get current directory (combined schools + districts)
dir <- fetch_directory()

# Get separate school and district data frames
dir_raw <- fetch_directory(tidy = FALSE)
dir_raw$schools
dir_raw$districts

# Force fresh download (bypass cache)
dir_fresh <- fetch_directory(use_cache = FALSE)

# Find schools in Minneapolis
mpls <- dir |>
  dplyr::filter(grepl("Minneapolis", district_name, ignore.case = TRUE))
} # }
```
