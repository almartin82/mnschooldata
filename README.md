# mnschooldata

An R package for fetching and processing school enrollment data from the Minnesota Department of Education (MDE).

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("almartin82/mnschooldata")
```
## Quick Start

```r
library(mnschooldata)

# Get 2024 enrollment data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# Get wide format (one row per school/district)
enr_wide <- fetch_enr(2024, tidy = FALSE)

# Get multiple years
enr_multi <- fetch_enr_multi(2020:2024)

# Filter to a specific district
mpls <- enr_2024 %>%
  dplyr::filter(grepl("Minneapolis", district_name, ignore.case = TRUE),
                is_district,
                subgroup == "total_enrollment",
                grade_level == "TOTAL")
```

## Data Availability

### Available Years

| Format Era | Years | Notes |
|------------|-------|-------|
| MDEAnalytics Modern | 2017-2025 | Current format with standardized columns |
| MDEAnalytics Legacy | 2007-2016 | Earlier format with different column naming |

**Earliest available year**: 2007
**Most recent available year**: 2025
**Total years of data**: 19 years

### Data Levels

- **State**: Statewide totals
- **District**: All ~330 operating school districts
- **School (Campus)**: Individual school buildings (~2,200 public schools)

### Demographics Available

| Category | Fields | Notes |
|----------|--------|-------|
| **Race/Ethnicity** | White, Black, Hispanic, Asian, Native American, Pacific Islander, Multiracial | |
| **Gender** | Male, Female | |
| **Special Populations** | Economically Disadvantaged (FRPL), English Learners (LEP), Special Education, Homeless | Availability varies by year |
| **Grade Levels** | Pre-K, K, Grades 1-12 | |

### Known Caveats

1. **October 1 Counts**: All enrollment data represents October 1 enrollment as reported to MDE via MARSS.

2. **Charter Schools**: Minnesota has ~180 charter schools. These are included in the data but may have different district type codes (07).

3. **Suppression**: Small cell sizes may be suppressed to protect student privacy. These appear as NA values.

4. **District Types**: Minnesota uses a district type + number system:
   - 01: Independent School Districts
   - 02: Common School Districts
   - 03: Special School Districts
   - 06: Intermediate School Districts (IEIC)
   - 07: Charter Schools

5. **Historical Comparisons**: Column names and available fields may vary across years. The package standardizes column names but some fields may only be available in certain year ranges.

## Data Source

Data comes from the Minnesota Department of Education's MDEAnalytics portal:
- **Portal**: https://pub.education.mn.gov/MDEAnalytics/Data.jsp
- **Data Center**: https://education.mn.gov/MDE/Data/
- **Data System**: MARSS (Minnesota Automated Reporting Student System)

For more information about Minnesota education data:
- [MDE Data Center](https://education.mn.gov/MDE/Data/)
- [Minnesota Report Card](https://rc.education.mn.gov/)
- [MARSS Documentation](https://education.mn.gov/MDE/dse/datasub/MARSSWES/)

## Functions

### Main Functions

| Function | Description |
|----------|-------------|
| `fetch_enr(end_year)` | Download enrollment data for a single year |
| `fetch_enr_multi(end_years)` | Download enrollment data for multiple years |
| `tidy_enr(df)` | Convert wide format to long (tidy) format |
| `get_available_years()` | List available data years |

### Utility Functions

| Function | Description |
|----------|-------------|
| `id_enr_aggs(df)` | Add aggregation level flags (is_state, is_district, is_campus) |
| `enr_grade_aggs(df)` | Create K-8, 9-12, K-12 grade aggregations |
| `cache_status()` | Show cached data files |
| `clear_cache()` | Remove cached data |

## Output Schema

### Wide Format (`tidy = FALSE`)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end (2024 = 2023-24) |
| type | character | "State", "District", or "Campus" |
| district_id | character | District identifier |
| campus_id | character | School identifier (NA for district/state rows) |
| district_name | character | District name |
| campus_name | character | School name (NA for district/state rows) |
| county | character | County name |
| row_total | integer | Total enrollment |
| white, black, hispanic, asian, native_american, pacific_islander, multiracial | integer | Race/ethnicity counts |
| male, female | integer | Gender counts |
| econ_disadv, lep, special_ed, homeless | integer | Special population counts |
| grade_pk, grade_k, grade_01 ... grade_12 | integer | Grade-level counts |

### Tidy Format (`tidy = TRUE`, default)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end |
| type | character | Aggregation level |
| district_id | character | District identifier |
| campus_id | character | School identifier |
| district_name | character | District name |
| campus_name | character | School name |
| county | character | County name |
| grade_level | character | "TOTAL", "PK", "K", "01"-"12" |
| subgroup | character | "total_enrollment", "white", "black", etc. |
| n_students | integer | Student count |
| pct | numeric | Percentage of total (0-1 scale) |
| is_state | logical | TRUE if state-level row |
| is_district | logical | TRUE if district-level row |
| is_campus | logical | TRUE if school-level row |

## Examples

### Statewide Enrollment Trends

```r
library(mnschooldata)
library(dplyr)
library(ggplot2)

# Get 10 years of data
enr <- fetch_enr_multi(2015:2024)

# State total enrollment over time
state_totals <- enr %>%
  filter(is_state,
         subgroup == "total_enrollment",
         grade_level == "TOTAL") %>%
  select(end_year, n_students)

ggplot(state_totals, aes(x = end_year, y = n_students)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Minnesota K-12 Public School Enrollment",
       x = "School Year End",
       y = "Total Students")
```

### Demographic Breakdown by District

```r
# Get 2024 data
enr_2024 <- fetch_enr(2024)

# Largest districts by enrollment
large_districts <- enr_2024 %>%
  filter(is_district,
         subgroup == "total_enrollment",
         grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(10)

# Demographic percentages for top districts
demo_pcts <- enr_2024 %>%
  filter(is_district,
         district_id %in% large_districts$district_id,
         grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(district_name, subgroup, pct)
```

### Grade-Level Analysis

```r
# Get grade aggregations
enr_2024 <- fetch_enr(2024)
grade_aggs <- enr_grade_aggs(enr_2024)

# Compare elementary (K-8) vs high school (9-12) enrollment
grade_aggs %>%
  filter(is_state) %>%
  select(grade_level, n_students)
```

## Caching

The package caches downloaded data locally to avoid repeated downloads:

```r
# Check cache status
cache_status()

# Clear all cached data
clear_cache()

# Clear only 2024 data
clear_cache(end_year = 2024)

# Force re-download (bypass cache)
enr <- fetch_enr(2024, use_cache = FALSE)
```

Cache files are stored in:
- macOS: `~/Library/Caches/mnschooldata/data/`
- Windows: `%LOCALAPPDATA%/mnschooldata/data/`
- Linux: `~/.cache/mnschooldata/data/`

## Related Packages

This package is part of a suite of state school data packages:
- [txschooldata](https://github.com/almartin82/txschooldata) - Texas
- [ilschooldata](https://github.com/almartin82/ilschooldata) - Illinois
- [nyschooldata](https://github.com/almartin82/nyschooldata) - New York
- [ohschooldata](https://github.com/almartin82/ohschooldata) - Ohio
- [paschooldata](https://github.com/almartin82/paschooldata) - Pennsylvania
- [caschooldata](https://github.com/almartin82/caschooldata) - California

## License
MIT
