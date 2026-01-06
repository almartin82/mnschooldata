#' Example multi-year enrollment data (2022-2024)
#'
#' A minimal example dataset for vignette illustration.
#' Contains enrollment data for Minneapolis and Saint Paul public schools
#' across three school years (2022, 2023, 2024), plus Minnesota state totals.
#'
#' @format A data frame with 45 rows and 12 columns:
#' \describe{
#'   \item{end_year}{School year end (2022, 2023, or 2024)}
#'   \item{state}{State abbreviation ("MN")}
#'   \item{district_name}{District name (NA for state-level data)}
#'   \item{school_name}{School name (NA for district/state-level data)}
#'   \item{level}{Geographic level: "state", "district", or "school"}
#'   \item{subgroup}{Demographic subgroup}
#'   \item{grade_level}{Grade level ("TOTAL" for all grades combined)}
#'   \item{n_students}{Enrollment count}
#'   \item{aggregation_flag}{Source of data ("official" for MDE-reported)}
#'   \item{is_district}{Logical indicating if row is district-level}
#'   \item{is_state}{Logical indicating if row is state-level}
#'   \item{type}{Character vector: "State", "District", or "School"}
#' }
#' @examples
#' \dontrun{
#' data(enr_multi_example)
#' head(enr_multi_example)
#' }
#' @keywords internal
"enr_multi_example"
