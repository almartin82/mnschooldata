#' Example district-level enrollment data
#'
#' A minimal example dataset for vignette illustration.
#' Contains district-level enrollment data for Minneapolis and Saint Paul.
#'
#' @format A data frame with 10 rows and 5 columns:
#' \describe{
#'   \item{end_year}{School year end (e.g., 2024 for 2023-24 school year)}
#'   \item{type}{Geographic type ("District")}
#'   \item{district_name}{District name}
#'   \item{subgroup}{Demographic subgroup}
#'   \item{grade_level}{Grade level ("TOTAL" for all grades combined)}
#'   \item{n_students}{Enrollment count}
#' }
#' @examples
#' \dontrun{
#' data(districts_example)
#' head(districts_example)
#' }
#' @keywords internal
"districts_example"
