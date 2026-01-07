# mnschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/mnschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/mnschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/mnschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/mnschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/mnschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/mnschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**[Documentation](https://almartin82.github.io/mnschooldata/)** | **[Getting Started](https://almartin82.github.io/mnschooldata/articles/quickstart.html)** | **[Enrollment Trends](https://almartin82.github.io/mnschooldata/articles/enrollment-trends.html)**

Fetch and analyze Minnesota school enrollment data from the Minnesota Department of Education (MDE) in R or Python.

## What can you find with mnschooldata?

**18 years of enrollment data (2007-2024).** 870,000 students. 330+ districts. Here are fifteen stories hiding in the numbers:

---

### 1. Minneapolis and St. Paul are shrinking

The Twin Cities' two largest urban districts have each lost over 10,000 students in the past decade, driven by declining birth rates, rising housing costs, and competition from charters and suburbs.

```r
library(mnschooldata)
library(dplyr)

enr <- fetch_enr_multi(2015:2024)

enr %>%
  filter(is_district, grepl("Minneapolis|St. Paul", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, n_students)
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
```

![Rochester growth](https://almartin82.github.io/mnschooldata/articles/enrollment-trends_files/figure-html/rochester-growth-1.png)

---

### 9. English learners approaching 10%

Over 80,000 Minnesota students are English learners, concentrated heavily in the Twin Cities metro.

```r
enr_2024 <- fetch_enr(2024)

enr_2024 %>%
  filter(is_district, subgroup == "lep", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  select(district_name, n_students, pct) %>%
  head(10)
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
enr_2024 <- fetch_enr(2024)

# Fetch multiple years
enr_multi <- fetch_enr_multi(2020:2024)

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

## Data availability

| Years | Source | Notes |
|-------|--------|-------|
| **2017-2024** | MDEAnalytics Modern | Current format with standardized columns |
| **2007-2016** | MDEAnalytics Legacy | Earlier format with different column naming |

Data is sourced from the Minnesota Department of Education:
- MDEAnalytics Portal: https://pub.education.mn.gov/MDEAnalytics/Data.jsp
- MDE Data Center: https://education.mn.gov/MDE/Data/

### What's included

- **Levels:** State, District (~330), School (~2,200)
- **Demographics:** White, Black, Hispanic, Asian, Native American, Pacific Islander, Multiracial
- **Special populations:** Economically disadvantaged, English learners, Special education, Homeless
- **Grade levels:** PK through 12

### Minnesota-specific notes

- **District Types:**
  - 01: Independent School Districts (most districts)
  - 02: Common School Districts
  - 03: Special School Districts (Minneapolis, St. Paul, Duluth)
  - 06: Intermediate School Districts (IEICs)
  - 07: Charter Schools (~180 charters)
- **MARSS:** Minnesota Automated Reporting Student System
- **October 1 counts:** All enrollment data represents October 1 official counts
- **Suppression:** Small cell sizes may be suppressed for privacy

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data in Python and R.

**All 50 state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

[Andy Martin](https://github.com/almartin82) (almartin@gmail.com)

## License

MIT
