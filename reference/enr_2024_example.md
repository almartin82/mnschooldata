# Example 2024 enrollment data

Example 2024 enrollment data

## Format

A data frame with 15 rows and 9 columns

## Examples

``` r
data(enr_2024_example)
head(enr_2024_example)
#> # A tibble: 6 × 12
#>   end_year state district_name school_name level subgroup grade_level n_students
#>      <dbl> <chr> <chr>         <chr>       <chr> <chr>    <chr>            <dbl>
#> 1     2024 MN    Minneapolis … Minneapoli… dist… total_e… TOTAL            28500
#> 2     2024 MN    Minneapolis … Minneapoli… dist… white    TOTAL             9500
#> 3     2024 MN    Minneapolis … Minneapoli… dist… black    TOTAL            12500
#> 4     2024 MN    Minneapolis … Minneapoli… dist… hispanic TOTAL             3200
#> 5     2024 MN    Minneapolis … Minneapoli… dist… asian    TOTAL             2100
#> 6     2024 MN    Saint Paul P… Saint Paul… dist… total_e… TOTAL            33200
#> # ℹ 4 more variables: aggregation_flag <chr>, is_district <lgl>,
#> #   is_state <lgl>, type <chr>
```
