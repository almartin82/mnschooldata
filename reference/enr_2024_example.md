# Example 2024 enrollment data

Example 2024 enrollment data

## Format

A data frame with 15 rows and 9 columns

## Examples

``` r
data(enr_2024_example)
head(enr_2024_example)
#> # A tibble: 6 × 15
#>   end_year state district_id district_name school_name level subgroup       
#>      <dbl> <chr> <chr>       <chr>         <chr>       <chr> <chr>          
#> 1     2024 MN    NA          NA            Minnesota   state asian          
#> 2     2024 MN    NA          NA            Minnesota   state black          
#> 3     2024 MN    NA          NA            Minnesota   state econ_disadv    
#> 4     2024 MN    NA          NA            Minnesota   state hispanic       
#> 5     2024 MN    NA          NA            Minnesota   state lep            
#> 6     2024 MN    NA          NA            Minnesota   state native_american
#> # ℹ 8 more variables: grade_level <chr>, n_students <dbl>, pct <dbl>,
#> #   aggregation_flag <chr>, is_district <lgl>, is_state <lgl>, is_school <lgl>,
#> #   type <chr>
```
