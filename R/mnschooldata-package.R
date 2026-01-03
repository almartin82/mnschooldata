#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' mnschooldata: Fetch and Process Minnesota School Data
#'
#' Downloads and processes school enrollment data from the Minnesota
#' Department of Education (MDE). Provides functions for fetching data
#' from MARSS (Minnesota Automated Reporting Student System) and
#' transforming it into tidy format for analysis.
#'
#' @section Main functions:
#' \describe{
#'   \item{fetch_enr}{Download enrollment data for a single year}
#'   \item{fetch_enr_multi}{Download enrollment data for multiple years}
#'   \item{tidy_enr}{Convert wide format to tidy long format}
#'   \item{get_available_years}{List available data years}
#' }
#'
#' @section Data source:
#' Data comes from the Minnesota Department of Education's MDEAnalytics portal.
#' Enrollment counts are based on October 1 enrollment reported via MARSS
#' (Minnesota Automated Reporting Student System).
#'
#' Available data:
#' \itemize{
#'   \item Years: 2007-2024
#'   \item Levels: State, District, School (Campus)
#'   \item Demographics: Race/ethnicity, gender, special populations
#'   \item Grades: Pre-K through 12
#' }
#'
#' @section Identifier system:
#' Minnesota uses a district type + number system:
#' \itemize{
#'   \item District Type: 2-digit code (01=Independent, 02=Common, 03=Special, 06=IEIC, 07=Charter)
#'   \item District Number: 4-digit number
#'   \item School Number: Additional digits for schools within districts
#' }
#'
#' @docType package
#' @name mnschooldata-package
NULL
