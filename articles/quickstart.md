# Getting Started with mnschooldata

## Installation

Install from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("almartin82/mnschooldata")
```

## Quick Example

Fetch Minnesota enrollment data:

``` r
library(mnschooldata)
library(dplyr)

# Fetch 2023 enrollment data
enr <- fetch_enr(2023, use_cache = TRUE)

head(enr)
```

``` r
# For vignette building, use bundled example data
library(mnschooldata)
library(dplyr)

data("enr_2023_example", package = "mnschooldata")
enr <- enr_2023_example
head(enr)
```

    ##   end_year     type district_id campus_id                     district_name
    ## 1     2023    State        <NA>      <NA>                              <NA>
    ## 2     2023 District 10001000000      <NA>     AITKIN PUBLIC SCHOOL DISTRICT
    ## 3     2023 District 10002000000      <NA>  HILL CITY PUBLIC SCHOOL DISTRICT
    ## 4     2023 District 10004000000      <NA>   MCGREGOR PUBLIC SCHOOL DISTRICT
    ## 5     2023 District 10011000000      <NA>    ANOKA-HENNEPIN SCHOOL DISTRICT
    ## 6     2023 District 10012000000      <NA> CENTENNIAL PUBLIC SCHOOL DISTRICT
    ##   campus_name grade_level         subgroup n_students pct is_state is_district
    ## 1        <NA>       TOTAL total_enrollment     873175   1     TRUE       FALSE
    ## 2        <NA>       TOTAL total_enrollment        977   1    FALSE        TRUE
    ## 3        <NA>       TOTAL total_enrollment        270   1    FALSE        TRUE
    ## 4        <NA>       TOTAL total_enrollment        409   1    FALSE        TRUE
    ## 5        <NA>       TOTAL total_enrollment      38336   1    FALSE        TRUE
    ## 6        <NA>       TOTAL total_enrollment       6842   1    FALSE        TRUE
    ##   is_campus aggregation_flag district_type is_charter
    ## 1     FALSE            state          <NA>      FALSE
    ## 2     FALSE         district            01      FALSE
    ## 3     FALSE         district            01      FALSE
    ## 4     FALSE         district            01      FALSE
    ## 5     FALSE         district            01      FALSE
    ## 6     FALSE         district            01      FALSE

## Understanding the Data

The data is returned in **tidy (long) format** by default:

- Each row is one subgroup for one school/district/state
- `subgroup` identifies the demographic group (e.g., “total_enrollment”,
  “white”, “black”, “hispanic”)
- `grade_level` shows the grade (“TOTAL”, “K”, “01”, “02”, etc.)
- `n_students` is the enrollment count
- `pct` is the percentage of total enrollment

``` r
enr %>%
  filter(is_state) %>%
  select(end_year, type, subgroup, grade_level, n_students) %>%
  head(10)
```

    ##   end_year  type         subgroup grade_level n_students
    ## 1     2023 State total_enrollment       TOTAL     873175
    ## 2     2023 State            white       TOTAL     518783
    ## 3     2023 State            black       TOTAL     110312
    ## 4     2023 State         hispanic       TOTAL     105538
    ## 5     2023 State            asian       TOTAL      62538
    ## 6     2023 State  native_american       TOTAL      15372
    ## 7     2023 State pacific_islander       TOTAL       1116
    ## 8     2023 State      multiracial       TOTAL      59516

## Filtering by Level

Use the aggregation flags to filter data:

``` r
# State totals
state <- enr %>% filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
state %>% select(end_year, n_students)
```

    ##   end_year n_students
    ## 1     2023     873175

``` r
# All districts
districts <- enr %>% filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL")
cat("Number of districts:", nrow(districts), "\n")
```

    ## Number of districts: 389

## Simple Analysis: Top 10 Districts

``` r
enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  select(district_name, n_students) %>%
  head(10)
```

    ##                         district_name n_students
    ## 1      ANOKA-HENNEPIN SCHOOL DISTRICT      38336
    ## 2     ST. PAUL PUBLIC SCHOOL DISTRICT      32750
    ## 3  MINNEAPOLIS PUBLIC SCHOOL DISTRICT      30079
    ## 4        ROSEMOUNT-APPLE VALLEY-EAGAN      29229
    ## 5        OSSEO PUBLIC SCHOOL DISTRICT      21385
    ## 6     SOUTH WASHINGTON COUNTY SCHOOLS      19705
    ## 7    ROCHESTER PUBLIC SCHOOL DISTRICT      17320
    ## 8           ELK RIVER SCHOOL DISTRICT      14785
    ## 9      WAYZATA PUBLIC SCHOOL DISTRICT      13217
    ## 10   LAKEVILLE PUBLIC SCHOOL DISTRICT      12105

## Demographic Analysis

``` r
# State demographics
enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian", "multiracial", "native_american")) %>%
  select(subgroup, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1))
```

    ##          subgroup n_students  pct
    ## 1           white     518783 59.4
    ## 2           black     110312 12.6
    ## 3        hispanic     105538 12.1
    ## 4           asian      62538  7.2
    ## 5 native_american      15372  1.8
    ## 6     multiracial      59516  6.8

## Wide Format

If you prefer wide format (one column per demographic), set
`tidy = FALSE`:

``` r
enr_wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)

enr_wide %>%
  filter(type == "State") %>%
  select(end_year, row_total, white, black, hispanic, asian)
```

## Minnesota-Specific Features

Minnesota has several unique district types:

``` r
enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  mutate(district_type_label = case_when(
    district_type == "01" ~ "Independent School District",
    district_type == "03" ~ "Special School District",
    district_type == "07" ~ "Charter School",
    TRUE ~ "Other"
  )) %>%
  count(district_type_label) %>%
  arrange(desc(n))
```

    ##           district_type_label   n
    ## 1 Independent School District 300
    ## 2              Charter School  68
    ## 3                       Other  19
    ## 4     Special School District   2

## Next Steps

- See
  [`vignette("enrollment_hooks")`](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks.md)
  for 15 stories from Minnesota enrollment data
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
    ## other attached packages:
    ## [1] dplyr_1.2.0        mnschooldata_0.1.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] vctrs_0.7.1       cli_3.6.5         knitr_1.51        rlang_1.1.7      
    ##  [5] xfun_0.56         generics_0.1.4    textshaping_1.0.4 jsonlite_2.0.0   
    ##  [9] glue_1.8.0        htmltools_0.5.9   ragg_1.5.0        sass_0.4.10      
    ## [13] rmarkdown_2.30    tibble_3.3.1      evaluate_1.0.5    jquerylib_0.1.4  
    ## [17] fastmap_1.2.0     yaml_2.3.12       lifecycle_1.0.5   compiler_4.5.2   
    ## [21] codetools_0.2-20  fs_1.6.6          pkgconfig_2.0.3   systemfonts_1.3.1
    ## [25] digest_0.6.39     R6_2.6.1          tidyselect_1.2.1  pillar_1.11.1    
    ## [29] magrittr_2.0.4    bslib_0.10.0      withr_3.0.2       tools_4.5.2      
    ## [33] pkgdown_2.2.0     cachem_1.1.0      desc_1.4.3
