# Example district-level enrollment data

Example district-level enrollment data

## Format

A data frame with 10 rows and 5 columns

## Examples

``` r
data(districts_example)
head(districts_example)
#> # A tibble: 6 × 6
#>   end_year type     district_name              subgroup   grade_level n_students
#>      <dbl> <chr>    <chr>                      <chr>      <chr>            <dbl>
#> 1     2024 District Minneapolis Public Schools total_enr… TOTAL            28500
#> 2     2024 District Minneapolis Public Schools white      TOTAL             9500
#> 3     2024 District Minneapolis Public Schools black      TOTAL            12500
#> 4     2024 District Minneapolis Public Schools hispanic   TOTAL             3200
#> 5     2024 District Minneapolis Public Schools asian      TOTAL             2100
#> 6     2024 District Saint Paul Public Schools  total_enr… TOTAL            33200
```
