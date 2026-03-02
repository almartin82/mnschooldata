# Fetch Minnesota district directory data

Downloads and processes district-level directory data from MDE-ORG.
Returns district information with superintendent contacts.

## Usage

``` r
fetch_district_directory(end_year = NULL, use_cache = TRUE)
```

## Arguments

- end_year:

  School year end (cache key). Defaults to current school year.

- use_cache:

  If TRUE (default), uses locally cached data when available.

## Value

Data frame with district directory data including superintendent
contacts

## Examples

``` r
if (FALSE) { # \dontrun{
# Get district directory
districts <- fetch_district_directory()

# Find districts in Hennepin County
hennepin <- districts |>
  dplyr::filter(grepl("Hennepin", county_name))
} # }
```
