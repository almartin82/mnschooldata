#' Example 2024 enrollment data
#'
#' A minimal example dataset for vignette illustration.
#' Contains enrollment data for Minneapolis and Saint Paul public schools,
#' plus Minnesota state totals.
#'
#' @format A data frame with 15 rows and 9 columns:
#' \describe{
#'   \item{end_year}{School year end (e.g., 2024 for 2023-24 school year)}
#'   \item{state}{State abbreviation ("MN")}
#'   \item{district_name}{District name (NA for state-level data)}
#'   \item{school_name}{School name (NA for district/state-level data)}
#'   \item{level}{Geographic level: "state", "district", or "school"}
#'   \item{subgroup}{Demographic subgroup (e.g., "total_enrollment", "white", "black")}
#'   \item{grade_level}{Grade level ("TOTAL" for all grades combined)}
#'   \item{n_students}{Enrollment count}
#'   \item{aggregation_flag}{Source of data ("official" for MDE-reported)}
#' }
#' @examples
#' \dontrun{
#' data(enr_2024_example)
#' head(enr_2024_example)
#' }
"enr_2024_example"
