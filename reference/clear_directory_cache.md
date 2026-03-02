# Clear directory data cache

Removes cached directory data files.

## Usage

``` r
clear_directory_cache(end_year = NULL)
```

## Arguments

- end_year:

  Optional school year to clear. If NULL, clears all directory cache.

## Value

Invisibly returns the number of files removed

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear all directory cache
clear_directory_cache()

# Clear specific year
clear_directory_cache(2026)
} # }
```
