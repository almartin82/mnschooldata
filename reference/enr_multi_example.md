# Example multi-year enrollment data (2022-2024)

Example multi-year enrollment data (2022-2024)

## Format

A data frame with 45 rows and 12 columns

## Examples

``` r
data(enr_multi_example)
head(enr_multi_example)
#> # A tibble: 6 × 12
#>   end_year state district_name school_name level subgroup grade_level n_students
#>      <dbl> <chr> <chr>         <chr>       <chr> <chr>    <chr>            <dbl>
#> 1     2022 MN    Minneapolis … Minneapoli… dist… total_e… TOTAL            27645
#> 2     2022 MN    Minneapolis … Minneapoli… dist… white    TOTAL             9215
#> 3     2022 MN    Minneapolis … Minneapoli… dist… black    TOTAL            12125
#> 4     2022 MN    Minneapolis … Minneapoli… dist… hispanic TOTAL             3104
#> 5     2022 MN    Minneapolis … Minneapoli… dist… asian    TOTAL             2037
#> 6     2022 MN    Saint Paul P… Saint Paul… dist… total_e… TOTAL            32204
#> # ℹ 4 more variables: aggregation_flag <chr>, is_district <lgl>,
#> #   is_state <lgl>, type <chr>
```
