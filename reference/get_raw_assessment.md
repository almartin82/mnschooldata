# Download raw assessment data from MDE

Downloads assessment data from Minnesota Department of Education's
MDEAnalytics portal.

## Usage

``` r
get_raw_assessment(end_year, level = "all")
```

## Arguments

- end_year:

  School year end (e.g., 2024 for 2023-24 school year)

- level:

  Data level: "all" (default), "state", "district", or "school"

## Value

List with state, district, and/or school data frames
