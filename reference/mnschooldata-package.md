# mnschooldata: Minnesota School Data

A simple, consistent interface for accessing Minnesota school data in
Python and R.

Downloads and processes school enrollment data from the Minnesota
Department of Education (MDE). Provides functions for fetching data from
MARSS (Minnesota Automated Reporting Student System) and transforming it
into tidy format for analysis.

## Main functions

- fetch_enr:

  Download enrollment data for a single year

- fetch_enr_multi:

  Download enrollment data for multiple years

- tidy_enr:

  Convert wide format to tidy long format

- get_available_years:

  List available data years

## Data source

Data comes from the Minnesota Department of Education's MDEAnalytics
portal. Enrollment counts are based on October 1 enrollment reported via
MARSS (Minnesota Automated Reporting Student System).

Available data:

- Years: 2007-2024

- Levels: State, District, School (Campus)

- Demographics: Race/ethnicity, gender, special populations

- Grades: Pre-K through 12

## Identifier system

Minnesota uses a district type + number system:

- District Type: 2-digit code (01=Independent, 02=Common, 03=Special,
  06=IEIC, 07=Charter)

- District Number: 4-digit number

- School Number: Additional digits for schools within districts

## See also

Useful links:

- <https://almartin82.github.io/mnschooldata>

- <https://github.com/almartin82/mnschooldata>

- Report bugs at <https://github.com/almartin82/mnschooldata/issues>

Useful links:

- <https://almartin82.github.io/mnschooldata>

- <https://github.com/almartin82/mnschooldata>

- Report bugs at <https://github.com/almartin82/mnschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
