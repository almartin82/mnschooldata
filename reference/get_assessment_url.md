# Get assessment data URL

Constructs the URL for downloading assessment data from MDE. MDE uses
WebFOCUS for data delivery, with URLs that include year and level
parameters.

## Usage

``` r
get_assessment_url(end_year, level)
```

## Arguments

- end_year:

  School year end

- level:

  One of "state", "district", "school"

## Value

URL string or NULL if not available
