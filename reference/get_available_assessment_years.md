# Get available assessment years

Returns information about years for which assessment data is available.

## Usage

``` r
get_available_assessment_years()
```

## Value

List with 'years' (numeric vector) and 'note' (character)

## Examples

``` r
get_available_assessment_years()
#> $years
#> [1] 2019 2021 2022 2023 2024 2025
#> 
#> $note
#> [1] "2020 data not available due to COVID-19 testing waiver"
#> 
```
