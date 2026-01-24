# Getting Started with mnschooldata

## Installation

Install from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("almartin82/mnschooldata")
```

## Quick Example

Fetch the most recent year of Minnesota enrollment data:

``` r
library(mnschooldata)
library(dplyr)

# Fetch 2024 enrollment data
enr <- fetch_enr(2024, use_cache = TRUE)

head(enr)
```

``` r
# For vignette building, use cached data to avoid network calls
data("enr_2024_example", package = "mnschooldata")
enr <- enr_2024_example
head(enr)
```

    ##   end_year state district_id district_name school_name level        subgroup
    ## 1     2024    MN        <NA>          <NA>   Minnesota state           asian
    ## 2     2024    MN        <NA>          <NA>   Minnesota state           black
    ## 3     2024    MN        <NA>          <NA>   Minnesota state     econ_disadv
    ## 4     2024    MN        <NA>          <NA>   Minnesota state        hispanic
    ## 5     2024    MN        <NA>          <NA>   Minnesota state             lep
    ## 6     2024    MN        <NA>          <NA>   Minnesota state native_american
    ##   grade_level n_students        pct aggregation_flag is_district is_state
    ## 1       TOTAL      13976 0.02798228         official       FALSE     TRUE
    ## 2       TOTAL      27846 0.05575232         official       FALSE     TRUE
    ## 3       TOTAL     101640 0.20350019         official       FALSE     TRUE
    ## 4       TOTAL      18560 0.03716021         official       FALSE     TRUE
    ## 5       TOTAL      43373 0.08683996         official       FALSE     TRUE
    ## 6       TOTAL       6059 0.01213113         official       FALSE     TRUE
    ##   is_school  type
    ## 1     FALSE State
    ## 2     FALSE State
    ## 3     FALSE State
    ## 4     FALSE State
    ## 5     FALSE State
    ## 6     FALSE State

## Understanding the Data

The data is returned in **tidy (long) format** by default:

- Each row is one subgroup for one school/district/state
- `subgroup` identifies the demographic group (e.g., “total_enrollment”,
  “white”, “black”, “hispanic”, “econ_disadv”)
- `grade_level` shows the grade (“TOTAL”, “K”, “01”, “02”, etc.)
- `n_students` is the enrollment count
- `pct` is the percentage of total enrollment

``` r
enr %>%
  filter(is_state) %>%
  select(end_year, type, subgroup, grade_level, n_students) %>%
  head(10)
```

## Filtering by Level

Use the aggregation flags to filter data:

``` r
# State totals
state <- enr %>% filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
state %>% select(end_year, n_students)

# All districts
districts <- enr %>% filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL")
nrow(districts)

# All schools
schools <- enr %>% filter(is_school, subgroup == "total_enrollment", grade_level == "TOTAL")
nrow(schools)
```

## Simple Analysis: Top 10 Districts

``` r
enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  select(district_name, n_students) %>%
  head(10)
```

## Demographic Analysis

``` r
# State demographics
enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(subgroup, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1))
```

## Wide Format

If you prefer wide format (one column per demographic), set
`tidy = FALSE`:

``` r
enr_wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

enr_wide %>%
  filter(type == "State") %>%
  select(end_year, row_total, white, black, hispanic, asian, econ_disadv)
```

## Historical Data

Fetch multiple years to analyze trends:

``` r
# Fetch 5 years of data
enr_multi <- fetch_enr_multi(2020:2024, use_cache = TRUE)

# State enrollment trend
enr_multi %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students)
```

## Minnesota-Specific Features

Minnesota has several unique district types:

``` r
enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  mutate(district_type = case_when(
    grepl("^01", district_id) ~ "Independent School District",
    grepl("^03", district_id) ~ "Special School District (e.g., Minneapolis, St. Paul)",
    grepl("^06", district_id) ~ "Intermediate School District",
    grepl("^07", district_id) ~ "Charter School",
    TRUE ~ "Other"
  )) %>%
  count(district_type) %>%
  arrange(desc(n))
```

## Next Steps

- See
  [`vignette("enrollment-trends")`](https://almartin82.github.io/mnschooldata/articles/enrollment-trends.md)
  for 15 stories Minnesota enrollment data
- Use
  [`?fetch_enr`](https://almartin82.github.io/mnschooldata/reference/fetch_enr.md)
  for full function documentation

## Session Info

``` r
sessionInfo()
```

    ## R version 4.5.2 (2025-10-31)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          codetools_0.2-20 
    ##  [5] fastmap_1.2.0     xfun_0.56         cachem_1.1.0      knitr_1.51       
    ##  [9] htmltools_0.5.9   rmarkdown_2.30    lifecycle_1.0.5   cli_3.6.5        
    ## [13] sass_0.4.10       pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4  
    ## [17] systemfonts_1.3.1 compiler_4.5.2    tools_4.5.2       ragg_1.5.0       
    ## [21] evaluate_1.0.5    bslib_0.9.0       yaml_2.3.12       jsonlite_2.0.0   
    ## [25] rlang_1.1.7       fs_1.6.6
