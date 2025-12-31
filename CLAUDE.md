# Claude Code Instructions for mnschooldata

## Commit and PR Guidelines

- Do NOT include "Generated with Claude Code" in commit messages
- Do NOT include "Co-Authored-By: Claude" in commit messages
- Do NOT mention Claude or AI assistance in PR descriptions
- Keep commit messages clean and professional

## Project Context

This is an R package for fetching and processing Minnesota school enrollment data from MDE (Minnesota Department of Education).

### Key Data Characteristics

- **Data Source**: Minnesota Department of Education (MDE) at https://education.mn.gov/MDE/Data/
- **Data Portal**: MDEAnalytics at https://pub.education.mn.gov/MDEAnalytics/Data.jsp
- **ID System**:
  - District Type: 2 digits (01=Independent, 02=Common, 03=Special, 06=IEIC, 07=Charter)
  - District Number: 4 digits
  - School Number: Additional digits for schools within districts
- **Primary Data System**: MARSS (Minnesota Automated Reporting Student System)
- **Number of Districts**: ~330 operating districts
- **Number of Schools**: ~2,200 public schools

### Data Availability

- **Years**: 2007-2025 (19 years)
- **Format Eras**:
  - Era 1 (2007-2016): Legacy MDEAnalytics format
  - Era 2 (2017-present): Modern MDEAnalytics format with standardized columns
- **Levels**: State, District, School (Campus)
- **Collection Date**: October 1 enrollment

### Column Mapping Notes

MDE uses various column naming conventions across years:
- District identifiers: "DistrictNumber", "District Number", "DISTRICT_NUMBER"
- School identifiers: "SchoolNumber", "School Number", "SCHOOL_NUMBER"
- Demographics use full names: "American Indian", "Two or More Races"
- Special populations: "Free/Reduced Price Lunch", "English Learner"

## Package Structure

The package follows the same patterns as txschooldata/ilschooldata:
- `fetch_enrollment.R` - Main user-facing function
- `get_raw_enrollment.R` - Download raw data from MDE
- `process_enrollment.R` - Process raw data into standard schema
- `tidy_enrollment.R` - Transform to long format
- `cache.R` - Local caching functions
- `utils.R` - Utility functions including safe_numeric

## Testing

Run tests with:
```r
devtools::test()
```

Integration tests require network access to MDE servers. They will skip gracefully if:
- Running on CRAN
- No network connection
- MDE servers are unavailable

## Common Issues

1. **URL Changes**: MDE occasionally changes their data portal URLs. Check https://education.mn.gov/MDE/Data/ for current access patterns.

2. **Column Name Variations**: MDE has inconsistent column naming across years. The column mapping in `get_mde_column_map()` handles common variations but may need updates for older years.

3. **Data Availability**: Not all fields are available for all years. The package returns NA for unavailable fields.
