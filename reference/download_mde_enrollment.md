# Download MDE enrollment data file

Downloads enrollment data from the MDEAnalytics portal. MDE provides
Excel files for download that can be accessed via direct URLs.

## Usage

``` r
download_mde_enrollment(end_year, level = "school")
```

## Arguments

- end_year:

  School year end

- level:

  Data level: "school", "district", or "state"

## Value

Data frame with enrollment data
