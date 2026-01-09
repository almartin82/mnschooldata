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

    ##   end_year state              district_name                school_name    level
    ## 1     2024    MN Minneapolis Public Schools Minneapolis Public Schools district
    ## 2     2024    MN Minneapolis Public Schools Minneapolis Public Schools district
    ## 3     2024    MN Minneapolis Public Schools Minneapolis Public Schools district
    ## 4     2024    MN Minneapolis Public Schools Minneapolis Public Schools district
    ## 5     2024    MN Minneapolis Public Schools Minneapolis Public Schools district
    ## 6     2024    MN  Saint Paul Public Schools  Saint Paul Public Schools district
    ##           subgroup grade_level n_students aggregation_flag is_district is_state
    ## 1 total_enrollment       TOTAL      28500         official        TRUE    FALSE
    ## 2            white       TOTAL       9500         official        TRUE    FALSE
    ## 3            black       TOTAL      12500         official        TRUE    FALSE
    ## 4         hispanic       TOTAL       3200         official        TRUE    FALSE
    ## 5            asian       TOTAL       2100         official        TRUE    FALSE
    ## 6 total_enrollment       TOTAL      33200         official        TRUE    FALSE
    ##       type
    ## 1 District
    ## 2 District
    ## 3 District
    ## 4 District
    ## 5 District
    ## 6 District

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
