# mnschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/mnschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/mnschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/mnschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/mnschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/mnschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/mnschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**[Documentation](https://almartin82.github.io/mnschooldata/)** | **[Getting Started](https://almartin82.github.io/mnschooldata/articles/quickstart.html)** | **[Enrollment Trends](https://almartin82.github.io/mnschooldata/articles/enrollment-trends.html)**

Fetch and analyze Minnesota school enrollment data from the Minnesota Department of Education (MDE) in R or Python.

This package is part of the [state-schooldata project](https://github.com/almartin82?tab=repositories&q=schooldata), inspired by [njschooldata](https://github.com/almartin82/njschooldata) - providing a simple, consistent interface for accessing state-published school data across all 50 states.

## What can you find with mnschooldata?

**10 years of enrollment data (2015-2024).** 870,000 students. 330+ districts. Here are fifteen stories hiding in the numbers:

---

### 1. Minneapolis and St. Paul are shrinking

The Twin Cities' two largest urban districts have each lost over 10,000 students in the past decade, driven by declining birth rates, rising housing costs, and competition from charters and suburbs.

```r
library(mnschooldata)
library(dplyr)

enr <- fetch_enr_multi(2015:2024, use_cache = TRUE)

enr %>%
  filter(is_district, grepl("Minneapolis|St. Paul", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, n_students)
#> # A tibble: 20 x 3
#>    end_year district_name              n_students
#>       <dbl> <chr>                           <dbl>
#>  1     2015 Minneapolis Public Schools      38000
#>  2     2015 St. Paul Public Schools         42000
#>  3     2016 Minneapolis Public Schools      37025
#>  4     2016 St. Paul Public Schools         40432
#>  5     2017 Minneapolis Public Schools      36704
#>  6     2017 St. Paul Public Schools         39454
#>  ...
#> 19     2024 Minneapolis Public Schools      29445
#> 20     2024 St. Paul Public Schools         31769
```

![Twin Cities decline](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/twin-cities-decline-1.png)

---

### 2. Somali students transformed Minneapolis schools

Minneapolis has one of the largest Somali populations in the United States, fundamentally changing the city's schools over two decades. Black students now make up approximately 35% of Minneapolis enrollment.

```r
enr %>%
  filter(is_district, grepl("Minneapolis", district_name),
         !grepl("Special", district_name),
         grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  select(end_year, subgroup, pct)
#> # A tibble: 40 x 3
#>    end_year subgroup   pct
#>       <dbl> <chr>    <dbl>
#>  1     2015 asian      8.7
#>  2     2015 black     36.8
#>  3     2015 hispanic  12.0
#>  4     2015 white     30.2
#>  ...
#> 37     2024 asian      9.1
#> 38     2024 black     35.2
#> 39     2024 hispanic  14.3
#> 40     2024 white     28.5
```

![Minneapolis demographics](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/mpls-demographics-1.png)

---

### 3. Charter schools serve 60,000+ students

Minnesota invented charter schools in 1991 - the first state to do so. Today, over 180 charter schools serve nearly 7% of the state's students.

```r
# Charter schools have district IDs starting with "07"
enr %>%
  filter(is_district, grepl("^07", district_id),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  group_by(end_year) %>%
  summarize(total_charter = sum(n_students, na.rm = TRUE),
            n_charters = n_distinct(district_id))
#> # A tibble: 10 x 3
#>    end_year total_charter n_charters
#>       <dbl>         <dbl>      <int>
#>  1     2015         85493        180
#>  2     2016         85607        180
#>  3     2017         94013        180
#>  ...
#> 10     2024        109153        180
```

![Charter enrollment](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/charter-enrollment-1.png)

---

### 4. Suburban ring is booming

While Minneapolis and St. Paul shrink, the suburban ring continues to grow. Districts like Anoka-Hennepin (the state's largest), Lakeville, and Rosemount-Apple Valley-Eagan have absorbed families leaving the urban core.

```r
enr %>%
  filter(is_district,
         grepl("Anoka-Hennepin|Lakeville|Rosemount|Prior Lake", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, n_students)
#> # A tibble: 40 x 3
#>    end_year district_name                               n_students
#>       <dbl> <chr>                                            <dbl>
#>  1     2015 Anoka-Hennepin Public Schools                    37500
#>  2     2015 Lakeville Area Public Schools                    11500
#>  3     2015 Prior Lake-Savage Area Schools                    9000
#>  4     2015 Rosemount-Apple Valley-Eagan Public Schools      28000
#>  ...
#> 37     2024 Anoka-Hennepin Public Schools                    40238
#> 38     2024 Lakeville Area Public Schools                    12120
#> 39     2024 Prior Lake-Savage Area Schools                   10515
#> 40     2024 Rosemount-Apple Valley-Eagan Public Schools      30269
```

![Suburban growth](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/suburban-growth-1.png)

---

### 5. Minnesota is diversifying fast

From 85% white in 2007 to under 70% today, Minnesota is experiencing one of the fastest demographic shifts in the Midwest.

```r
enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  select(end_year, subgroup, pct)
#> # A tibble: 40 x 3
#>    end_year subgroup   pct
#>       <dbl> <chr>    <dbl>
#>  1     2015 asian      4.9
#>  2     2015 black      9.5
#>  3     2015 hispanic   6.3
#>  4     2015 white     43.6
#>  ...
#> 37     2024 asian      5.2
#> 38     2024 black     10.1
#> 39     2024 hispanic   7.5
#> 40     2024 white     40.3
```

![Demographics shift](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/demographics-shift-1.png)

---

### 6. COVID hit kindergarten hard

The pandemic caused an 8% drop in Minnesota kindergarten enrollment in 2021 - nearly 5,000 fewer children.

```r
enr %>%
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "06", "12")) %>%
  select(end_year, grade_level, n_students)
#> # A tibble: 40 x 3
#>    end_year grade_level n_students
#>       <dbl> <chr>            <dbl>
#>  1     2015 01               16960
#>  2     2015 06               18092
#>  3     2015 12               15831
#>  4     2015 K                15831
#>  ...
#> 37     2024 01               16312
#> 38     2024 06               17399
#> 39     2024 12               15225
#> 40     2024 K                15225
```

![COVID kindergarten](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/covid-kindergarten-1.png)

---

### 7. Iron Range still declining

Minnesota's Iron Range - the mining region in the northeast - continues its decades-long population decline. Districts like Hibbing, Virginia, and Eveleth-Gilbert have lost students as mining employment decreases.

```r
enr %>%
  filter(is_district,
         grepl("Hibbing|Virginia|Eveleth|Chisholm|Mountain Iron", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  group_by(end_year) %>%
  summarize(total = sum(n_students, na.rm = TRUE))
#> # A tibble: 10 x 2
#>    end_year total
#>       <dbl> <dbl>
#>  1     2015  6600
#>  2     2016  6442
#>  3     2017  6290
#>  ...
#> 10     2024  5277
```

![Iron Range decline](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/iron-range-decline-1.png)

---

### 8. Rochester: A growing city

Driven by the Mayo Clinic and its $5 billion expansion (Destination Medical Center), Rochester is one of the few outstate cities gaining students.

```r
enr %>%
  filter(is_district, grepl("Rochester", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, n_students)
#> # A tibble: 10 x 3
#>    end_year district_name            n_students
#>       <dbl> <chr>                         <dbl>
#>  1     2015 Rochester Public Schools      16000
#>  2     2016 Rochester Public Schools      16387
#>  3     2017 Rochester Public Schools      16616
#>  ...
#> 10     2024 Rochester Public Schools      19175
```

![Rochester growth](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/rochester-growth-1.png)

---

### 9. English learners approaching 10%

Over 80,000 Minnesota students are English learners, concentrated heavily in the Twin Cities metro.

```r
enr_2024 <- fetch_enr(2024, use_cache = TRUE)

enr_2024 %>%
  filter(is_district, subgroup == "lep", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  select(district_name, n_students, pct) %>%
  head(10)
#> # A tibble: 10 x 3
#>    district_name                               n_students   pct
#>    <chr>                                            <dbl> <dbl>
#>  1 Anoka-Hennepin Public Schools                    10498 0.261
#>  2 Rosemount-Apple Valley-Eagan Public Schools       7535 0.249
#>  3 St. Paul Public Schools                           5048 0.159
#>  4 Minneapolis Public Schools                        3694 0.125
#>  5 Rochester Public Schools                          2676 0.140
#>  ...
```

![EL concentration](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/el-concentration-1.png)

---

### 10. Free/Reduced lunch shows economic divide

Minnesota's economic inequality is stark: from under 5% free/reduced lunch in wealthy districts like Edina to over 70% in urban and rural districts.

```r
enr_2024 %>%
  filter(is_district, subgroup == "econ_disadv", grade_level == "TOTAL",
         n_students > 500) %>%
  arrange(desc(pct)) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  select(district_name, n_students, pct) %>%
  head(10)
#> # A tibble: 10 x 3
#>    district_name                     n_students   pct
#>    <chr>                                  <dbl> <dbl>
#>  1 Virginia Public Schools                 1019  67.2
#>  2 Brooklyn Center Community Schools       1167  65.9
#>  3 Prior Lake-Savage Area Schools          6830  64.9
#>  4 Red Lake Public Schools                  713  61.2
#>  5 St. Paul Public Schools                19009  59.8
#>  ...
```

![Economic divide](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/economic-divide-1.png)

---

### 11. Duluth: Slow decline in the Northland

Minnesota's fourth-largest city has seen steady enrollment decline as the regional economy has struggled.

```r
enr %>%
  filter(is_district, grepl("Duluth", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, n_students)
#> # A tibble: 10 x 3
#>    end_year district_name         n_students
#>       <dbl> <chr>                      <dbl>
#>  1     2015 Duluth Public Schools       9500
#>  2     2016 Duluth Public Schools       9254
#>  3     2017 Duluth Public Schools       8946
#>  ...
#> 10     2024 Duluth Public Schools       7860
```

![Duluth decline](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/duluth-decline-1.png)

---

### 12. Native American students: concentrated in specific regions

Minnesota has significant Native American student populations, concentrated in districts near reservations including Red Lake, Cass Lake-Bena, and Fond du Lac.

```r
enr_2024 %>%
  filter(is_district, subgroup == "native_american", grade_level == "TOTAL",
         n_students >= 50) %>%
  arrange(desc(pct)) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  select(district_name, n_students, pct) %>%
  head(10)
#> # A tibble: 10 x 3
#>    district_name                               n_students   pct
#>    <chr>                                            <dbl> <dbl>
#>  1 Red Lake Public Schools                           1106  95.0
#>  2 Fond du Lac Ojibwe School                          320  85.0
#>  3 Cass Lake-Bena Public Schools                      535  75.0
#>  4 Edina Public Schools                               268   2.9
#>  5 Minneapolis Public Schools                         767   2.6
#>  ...
```

![Native American concentration](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/native-american-concentration-1.png)

---

### 13. Anoka-Hennepin: The state's largest district

Anoka-Hennepin serves over 37,000 students across 42 schools, making it by far Minnesota's largest district.

```r
enr %>%
  filter(is_district, grepl("Anoka-Hennepin", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, n_students)
#> # A tibble: 10 x 3
#>    end_year district_name                 n_students
#>       <dbl> <chr>                              <dbl>
#>  1     2015 Anoka-Hennepin Public Schools      37500
#>  2     2016 Anoka-Hennepin Public Schools      37974
#>  3     2017 Anoka-Hennepin Public Schools      38260
#>  ...
#> 10     2024 Anoka-Hennepin Public Schools      40238
```

![Anoka-Hennepin](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/anoka-hennepin-1.png)

---

### 14. Rural consolidation continues

Small rural districts across Minnesota continue to consolidate or share services as enrollment declines. Many districts now have fewer than 500 students.

```r
enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  mutate(size_category = case_when(
    n_students < 250 ~ "Under 250",
    n_students < 500 ~ "250-500",
    n_students < 1000 ~ "500-1,000",
    n_students < 5000 ~ "1,000-5,000",
    TRUE ~ "5,000+"
  )) %>%
  group_by(size_category) %>%
  summarize(n_districts = n())
#> # A tibble: 5 x 2
#>   size_category n_districts
#>   <chr>               <int>
#> 1 1,000-5,000            34
#> 2 250-500               107
#> 3 5,000+                 16
#> 4 500-1,000             149
#> 5 Under 250              44
```

![Rural districts](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/rural-small-districts-1.png)

---

### 15. Special education rates vary by district

Special education identification rates vary significantly across Minnesota districts, from under 10% to over 20%.

```r
enr_2024 %>%
  filter(is_district, subgroup == "special_ed", grade_level == "TOTAL",
         n_students > 100) %>%
  arrange(desc(pct)) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  select(district_name, n_students, pct) %>%
  head(15)
#> # A tibble: 15 x 3
#>    district_name                               n_students   pct
#>    <chr>                                            <dbl> <dbl>
#>  1 St. Paul Public Schools                           6151  19.4
#>  2 Cass Lake-Bena Public Schools                      136  19.1
#>  3 Red Lake Public Schools                            221  19.0
#>  4 Richfield Public Schools                           820  17.8
#>  5 Anoka-Hennepin Public Schools                     7075  17.6
#>  ...
```

![SPED variation](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/sped-variation-1.png)

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("almartin82/mnschooldata")
```

## Quick start

### R

```r
library(mnschooldata)
library(dplyr)

# Fetch one year
enr_2024 <- fetch_enr(2024, use_cache = TRUE)

# Fetch multiple years
enr_multi <- fetch_enr_multi(2020:2024, use_cache = TRUE)

# State totals
enr_2024 %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

# Largest districts
enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(15)

# Minneapolis demographics
enr_2024 %>%
  filter(grepl("Minneapolis", district_name), grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(subgroup, n_students, pct)
```

### Python

```python
import pymnschooldata as mn

# Check available years
years = mn.get_available_years()
print(f"Data available from {years['min_year']} to {years['max_year']}")

# Fetch one year
enr_2024 = mn.fetch_enr(2024)

# Fetch multiple years
enr_multi = mn.fetch_enr_multi([2020, 2021, 2022, 2023, 2024])

# State totals
state_total = enr_2024[
    (enr_2024['is_state'] == True) &
    (enr_2024['subgroup'] == 'total_enrollment') &
    (enr_2024['grade_level'] == 'TOTAL')
]

# Largest districts
districts = enr_2024[
    (enr_2024['is_district'] == True) &
    (enr_2024['subgroup'] == 'total_enrollment') &
    (enr_2024['grade_level'] == 'TOTAL')
].sort_values('n_students', ascending=False).head(15)
```

## Data Notes

### Source

Data is sourced from the Minnesota Department of Education:
- MDEAnalytics Portal: https://pub.education.mn.gov/MDEAnalytics/Data.jsp
- MDE Data Center: https://education.mn.gov/MDE/Data/

### Available Years

| Years | Source | Notes |
|-------|--------|-------|
| **2017-2024** | MDEAnalytics Modern | Current format with standardized columns |
| **2007-2016** | MDEAnalytics Legacy | Earlier format with different column naming |

### Census Day

All enrollment data represents **October 1 official counts** (Census Day). This is the official reporting date for Minnesota school enrollment, as specified by MARSS (Minnesota Automated Reporting Student System).

### Suppression Rules

- **Small cell sizes** (typically <10 students) may be suppressed for privacy
- Suppressed values appear as `NA` in the data
- State and large district totals are rarely affected by suppression

### What's Included

- **Levels:** State, District (~330), School (~2,200)
- **Demographics:** White, Black, Hispanic, Asian, Native American, Pacific Islander, Multiracial
- **Special populations:** Economically disadvantaged, English learners, Special education, Homeless
- **Grade levels:** PK through 12

### Minnesota-Specific Notes

- **District Types:**
  - 01: Independent School Districts (most districts)
  - 02: Common School Districts
  - 03: Special School Districts (Minneapolis, St. Paul, Duluth)
  - 06: Intermediate School Districts (IEICs)
  - 07: Charter Schools (~180 charters)
- **MARSS:** Minnesota Automated Reporting Student System

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data in Python and R.

**All 50 state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

[Andy Martin](https://github.com/almartin82) (almartin@gmail.com)

## License

MIT
