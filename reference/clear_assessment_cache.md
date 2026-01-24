# Clear assessment cache

Removes cached assessment data files.

## Usage

``` r
clear_assessment_cache(end_year = NULL, type = NULL, level = NULL)
```

## Arguments

- end_year:

  Optional school year to clear. If NULL, clears all years.

- type:

  Optional data type to clear ("tidy" or "wide"). If NULL, clears all
  types.

- level:

  Optional level to clear. If NULL, clears all levels.

## Value

Invisibly returns the number of files removed

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear all cached assessment data
clear_assessment_cache()

# Clear only 2024 assessment data
clear_assessment_cache(2024)

# Clear only tidy format assessment data
clear_assessment_cache(type = "tidy")
} # }
```
