# Get assessment data for a specific district

Convenience function to fetch assessment data for a single district.

## Usage

``` r
fetch_district_assessment(end_year, district_id, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  School year end

- district_id:

  4-digit district ID (e.g., "0001" for Minneapolis)

- tidy:

  If TRUE (default), returns tidy format

- use_cache:

  If TRUE (default), uses cached data

## Value

Data frame filtered to specified district

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Minneapolis (district 0001) assessment data
mpls_assess <- fetch_district_assessment(2024, "0001")

# Get St. Paul (district 0625) data
stpaul_assess <- fetch_district_assessment(2024, "0625")
} # }
```
