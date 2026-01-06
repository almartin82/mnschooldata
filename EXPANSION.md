# Minnesota School Data Expansion Research

**Last Updated:** 2026-01-04
**Theme Researched:** Graduation Rates

## Executive Summary

Minnesota graduation rate data is available through the Minnesota Department of Education (MDE). The data is accessible through the MDEAnalytics portal and the Minnesota Report Card, which use WebFOCUS reporting infrastructure. Unlike enrollment data which has direct file downloads, graduation data appears to be primarily accessed through interactive reporting tools that generate exports on demand.

**Key Finding:** The graduation data download mechanism uses WebFOCUS servlets (ibi_apps) rather than direct file URLs, making programmatic access more complex than the enrollment data. The Minnesota Report Card also provides graduation data but does not expose a public API.

---

## Current Package Status

- **R-CMD-check:** PASSING
- **Python tests:** PASSING
- **pkgdown:** PASSING
- **Current Data Types:** Enrollment only (2007-2024)
- **Data Source Pattern:** MDEAnalytics portal with CGI fallback

---

## Data Sources Found

### Source 1: MDEAnalytics Data Reports - Graduation Rate Topic

- **URL:** https://pub.education.mn.gov/MDEAnalytics/DataTopic.jsp?TOPICID=242
- **HTTP Status:** 200 (page loads, but content in iframe)
- **Format:** Interactive reporting interface with Excel/Tab export
- **Years:** 2003-2024 (adjusted cohort rates calculated back to 2003)
- **Access Method:** WebFOCUS servlet (ibi_apps), requires browser interaction or servlet mimicking
- **Update Frequency:** Annual, released each February/March

**Technical Details:**
- Page uses iframe: `src="/ibi_apps/WFServlet?IBIF_ex=mdea_ddl_topic_select_PAGELAUNCH&TOPICID=242"`
- Export functions: `flexDownload()` and `flexPrint()` JavaScript functions
- Requires JavaScript execution to access file list

### Source 2: Minnesota Report Card

- **URL:** https://rc.education.mn.gov/#graduation/
- **Example:** https://rc.education.mn.gov/#graduation/orgId--999999000000__groupType--state__year--2024__graduationYearRate--4__p--1
- **HTTP Status:** 200
- **Format:** Interactive Backbone.js application
- **Years:** 2012-2024 (adjusted cohort methodology started 2012)
- **Access Method:** JavaScript application, no public API
- **Export Options:** PDF export only (per interface documentation)

**Technical Details:**
- Uses Backbone.js, RequireJS, Highcharts
- Data loaded dynamically via JavaScript
- No documented JSON API for programmatic access
- Contact: mde.analytics@state.mn.us

### Source 3: Graduation Indicators Files (via Data Center)

- **URL:** Navigation path: MDE Homepage > Data Center > Data Reports and Analytics > Student Data > Category: Graduation Rate
- **HTTP Status:** Unknown (requires interactive navigation)
- **Format:** Excel (.xlsx) and Tab-delimited (.tab)
- **Years:** 2003-2024
- **Access Method:** Interactive file selection and download
- **File Types:**
  - Graduation Indicators (rates by cohort)
  - Graduation Counts (student counts by graduation year)

### Source 4: Assessment Files User Guide (Reference)

- **URL:** https://education.mn.gov/mdeprod/idcplg?IdcService=GET_FILE&dDocName=005342&RevisionSelectionMethod=latestReleased&Rendition=primary
- **Notes:** Documents file structure and column definitions for assessment files; similar patterns likely apply to graduation files

---

## Schema Analysis

### Graduation Rate Metrics Available

| Metric | Description |
|--------|-------------|
| 4-Year Rate | Percentage graduating within 4 years of entering 9th grade |
| 5-Year Rate | Percentage graduating within 5 years |
| 6-Year Rate | Percentage graduating within 6 years |
| 7-Year Rate | Percentage graduating within 7 years (used for accountability) |

### Cohort Methodology (since 2012)

Minnesota uses the federally-required Adjusted Cohort Graduation Rate (ACGR):
- Students assigned to cohort when entering 9th grade
- Example: 9th graders in Fall 2020 = 2024 four-year cohort
- Cohorts adjusted for transfers in/out
- Rates recalculated back to 2003 using this methodology

### Student Groups Available

Based on Minnesota Report Card and MDE documentation:

| Group Type | Categories |
|------------|------------|
| Race/Ethnicity | 8 state-defined groups including American Indian, Other Indigenous |
| Gender | Male, Female |
| Special Populations | English Learners, Special Education, Free/Reduced Lunch |
| Other | Foster Care, Homeless, Military Connected |

### Expected Column Names (based on enrollment patterns)

| Standard Field | Possible MDE Names |
|----------------|-------------------|
| district_id | DistrictNumber, District Number, District_Number |
| school_id | SchoolNumber, School Number, School_Number |
| district_name | DistrictName, District Name |
| school_name | SchoolName, School Name |
| grad_rate_4yr | Four Year Rate, 4YrRate, GradRate4 |
| grad_rate_7yr | Seven Year Rate, 7YrRate, GradRate7 |
| cohort_count | Cohort Count, Adjusted Cohort, CohortN |
| graduate_count | Graduate Count, Graduates, GradN |

### Known Data Suppression

- Data suppressed for small cell sizes to protect student privacy
- Uses "primary and secondary data suppression techniques"
- Common markers: `*`, `<10`, `-`, `N/A`
- Foster care data cannot be cross-filtered with other demographics

---

## Time Series Heuristics

Based on 2024 public reporting:

| Metric | Expected Value | Source |
|--------|---------------|--------|
| State 4-year rate | 82-86% | Historical trend |
| 2024 4-year rate | 84.2% | Press release |
| 2023 4-year rate | 83.3% | Press release |
| Total 4-year graduates 2024 | ~60,000 | 59,720 reported |
| YoY change | < 2 percentage points | Historical |

### Major Districts to Track (for validation)

| District | 2024 4-Year Rate | Notes |
|----------|------------------|-------|
| Minneapolis | ~70-75% | Major urban |
| St. Paul | 77% | +8 points from 2023 |
| Anoka-Hennepin | ~85-90% | Largest suburban |
| Duluth | ~80% | Northern Minnesota |

### Achievement Gap Metrics

White students: ~89% (4-year)
Other groups: Various, gaps documented in ESSA reporting

---

## URL Discovery Attempts

### URLs That Return 404

| URL Pattern | Status | Notes |
|-------------|--------|-------|
| pub.education.mn.gov/MDEAnalytics/DataDownload/Graduation_2024_State.xlsx | 404 | Direct file download not available |
| rc.education.mn.gov/js/settings.js | 404 | No exposed config |
| rc.education.mn.gov/js/app/config.js | 404 | No exposed config |

### URLs That Work But Require JavaScript

| URL | Status | Notes |
|-----|--------|-------|
| pub.education.mn.gov/MDEAnalytics/DataTopic.jsp?TOPICID=242 | 200 | Graduation topic page, content in iframe |
| rc.education.mn.gov | 200 | Report Card, JavaScript app |
| pub.education.mn.gov/ibi_apps/WFServlet?... | 200 | WebFOCUS, requires proper parameters |

### WebFOCUS Servlet Pattern (from enrollment)

The enrollment download uses this CGI pattern:
```
https://pub.education.mn.gov/MDEAnalytics/DataDownload.do
POST parameters:
- category = "Enrollment"
- year = end_year
- level = "School" | "District" | "State"
- format = "xlsx"
```

**For graduation, likely similar pattern:**
```
POST parameters:
- category = "Graduation Rate" or "Graduation"
- year = end_year
- level = "School" | "District" | "State"
- format = "xlsx"
```

---

## Implementation Complexity Assessment

### Priority: HIGH
- Graduation rates are a core educational metric
- High user demand for cohort analysis
- Complements existing enrollment data

### Complexity: HARD

**Challenges:**
1. No direct file download URLs discovered
2. Data accessed through WebFOCUS servlet requiring session management
3. May need to mimic browser interaction or use POST requests
4. Multiple rate types (4yr, 5yr, 6yr, 7yr) in same or different files
5. Student group breakdowns add complexity

### Estimated Files to Create/Modify

| File | Action | Complexity |
|------|--------|------------|
| R/get_raw_graduation.R | CREATE | High - WebFOCUS integration |
| R/process_graduation.R | CREATE | Medium - column mapping |
| R/tidy_graduation.R | CREATE | Medium - pivot logic |
| R/fetch_graduation.R | CREATE | Low - wrapper function |
| R/utils.R | MODIFY | Low - add graduation years |
| tests/testthat/test-pipeline-live-graduation.R | CREATE | Medium |

---

## Recommended Implementation Approach

### Option A: WebFOCUS POST Mimicking (Preferred)

1. Analyze existing enrollment CGI code in `download_mde_via_cgi()`
2. Modify POST parameters for graduation category
3. Test with different year/level combinations
4. Handle different file structures for indicators vs counts

```r
download_mde_graduation <- function(end_year, level = "school", type = "indicators") {
  cgi_url <- "https://pub.education.mn.gov/MDEAnalytics/DataDownload.do"

  post_params <- list(
    category = "Graduation Rate",
    year = as.character(end_year),
    level = switch(level, "school" = "School", "district" = "District", "state" = "State"),
    format = "xlsx",
    type = type  # "indicators" or "counts"
  )

  # ... similar to enrollment CGI code
}
```

### Option B: Minnesota Report Card Scraping (Fallback)

If POST doesn't work, could potentially:
1. Use RSelenium or chromote for browser automation
2. Navigate Report Card interface programmatically
3. Export PDF and parse (not ideal)

### Option C: Contact MDE

- Email mde.analytics@state.mn.us for API access
- Request bulk data file location
- May provide alternative access method

---

## Test Requirements

### Raw Data Fidelity Tests Needed

Based on publicly reported values:

```r
test_that("2024: State 4-year rate matches public data", {
  skip_if_offline()
  data <- fetch_grad(2024, tidy = TRUE)
  state_4yr <- data |>
    filter(is_state, grad_type == "4-year", subgroup == "all_students") |>
    pull(rate)
  expect_equal(state_4yr, 84.2, tolerance = 0.1)
})

test_that("2024: Total 4-year graduates matches public data", {
  skip_if_offline()
  data <- fetch_grad(2024, tidy = FALSE)
  total_grads <- sum(data$graduate_count_4yr[data$level == "State"], na.rm = TRUE)
  expect_equal(total_grads, 59720, tolerance = 100)
})

test_that("2023: State 4-year rate matches public data", {
  skip_if_offline()
  data <- fetch_grad(2023, tidy = TRUE)
  state_4yr <- data |>
    filter(is_state, grad_type == "4-year", subgroup == "all_students") |>
    pull(rate)
  expect_equal(state_4yr, 83.3, tolerance = 0.1)
})
```

### Data Quality Checks

```r
test_that("Graduation rates are valid percentages", {
  data <- fetch_grad(2024, tidy = TRUE)
  expect_true(all(data$rate >= 0, na.rm = TRUE))
  expect_true(all(data$rate <= 100, na.rm = TRUE))
})

test_that("7-year rate >= 4-year rate for same cohort", {
  data <- fetch_grad(2024, tidy = FALSE)
  expect_true(all(data$rate_7yr >= data$rate_4yr, na.rm = TRUE))
})

test_that("Major districts exist in all years", {
  major_districts <- c("0001", "0625", "0011")  # Minneapolis, St Paul, Anoka-Hennepin (IDs TBD)
  for (yr in 2020:2024) {
    data <- fetch_grad(yr, tidy = FALSE)
    expect_true(all(major_districts %in% data$district_id))
  }
})
```

---

## Open Questions

1. **Exact file URL pattern:** Need to test POST parameters for graduation category
2. **File structure:** Are indicators and counts in same file or separate?
3. **Historical years:** What's the earliest year with downloadable data?
4. **Student group files:** Separate download or same file with group columns?
5. **County-level data:** Available? Same download mechanism?

---

## Next Steps

1. **Test WebFOCUS POST:** Modify enrollment CGI code to request graduation category
2. **Examine downloaded file:** Document actual column names and structure
3. **Verify multiple years:** Confirm URL pattern works for 2020-2024
4. **Document schema changes:** Note any year-over-year column differences
5. **Implement TDD:** Write fidelity tests with known values before coding

---

## References

- [MDE Data Center](https://education.mn.gov/MDE/Data/)
- [Minnesota Report Card](https://rc.education.mn.gov/)
- [Graduation Rates Accountability Info](https://education.mn.gov/MDE/dse/ESSA/meet/acc/MDE075309)
- [MDEAnalytics Graduation Topic](https://pub.education.mn.gov/MDEAnalytics/DataTopic.jsp?TOPICID=242)
- [2024 Graduation Rate Press Release](https://content.govdelivery.com/accounts/MNMDE/bulletins/3986df4)
- [2023 Graduation Rate Press Release](https://content.govdelivery.com/accounts/MNMDE/bulletins/392e7b7)
