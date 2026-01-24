# ==============================================================================
# Assessment Data Tidying Functions
# ==============================================================================
#
# This file contains functions for transforming assessment data from wide
# format (with pct_does_not_meet, pct_partially_meets, etc. columns) to long
# (tidy) format with a proficiency_level column.
#
# ==============================================================================


#' Tidy assessment data
#'
#' Transforms wide assessment data to long format with proficiency_level column.
#' The wide format has separate columns for each proficiency level. The tidy
#' format pivots these into a single proficiency_level column with corresponding
#' n_students and pct values.
#'
#' @param df A wide data.frame of processed assessment data
#' @return A long data.frame of tidied assessment data
#' @export
#' @examples
#' \dontrun{
#' wide_data <- fetch_assessment(2024, tidy = FALSE)
#' tidy_data <- tidy_assessment(wide_data)
#' }
tidy_assessment <- function(df) {

  if (nrow(df) == 0) {
    return(create_empty_tidy_assessment())
  }

  # Invariant columns (identifiers that stay the same)
  invariants <- c(
    "end_year", "type",
    "district_id", "district_name",
    "school_id", "school_name",
    "test", "subject", "grade", "subgroup",
    "n_tested"
  )
  invariants <- invariants[invariants %in% names(df)]

  # Proficiency level mappings
  # MDE uses: Does Not Meet, Partially Meets, Meets, Exceeds
  # For MCA-IV (2025): Beginning, Intermediate, Meets, Advanced
  level_mappings <- list(
    does_not_meet = list(
      n_col = "n_does_not_meet",
      pct_col = "pct_does_not_meet",
      level_name = "does_not_meet"
    ),
    partially_meets = list(
      n_col = "n_partially_meets",
      pct_col = "pct_partially_meets",
      level_name = "partially_meets"
    ),
    meets = list(
      n_col = "n_meets",
      pct_col = "pct_meets",
      level_name = "meets"
    ),
    exceeds = list(
      n_col = "n_exceeds",
      pct_col = "pct_exceeds",
      level_name = "exceeds"
    )
  )

  # Build long format
  tidy_list <- lapply(level_mappings, function(mapping) {
    n_col <- mapping$n_col
    pct_col <- mapping$pct_col
    level_name <- mapping$level_name

    # Check if columns exist
    has_n <- n_col %in% names(df)
    has_pct <- pct_col %in% names(df)

    if (!has_n && !has_pct) {
      return(NULL)
    }

    # Build the subset
    result <- df[, invariants, drop = FALSE]
    result$proficiency_level <- level_name

    # Add n_students
    if (has_n) {
      result$n_students <- df[[n_col]]
    } else if (has_pct) {
      # Calculate from percentage and n_tested
      # pct values are typically 0-100
      result$n_students <- ifelse(
        !is.na(df[[pct_col]]) & !is.na(df$n_tested) & df$n_tested > 0,
        round(df[[pct_col]] / 100 * df$n_tested),
        NA_real_
      )
    } else {
      result$n_students <- NA_real_
    }

    # Add pct (normalized to 0-1 range)
    if (has_pct) {
      result$pct <- ifelse(
        !is.na(df[[pct_col]]),
        pmin(df[[pct_col]] / 100, 1.0),
        NA_real_
      )
    } else if (has_n) {
      # Calculate from count and n_tested
      result$pct <- ifelse(
        !is.na(df[[n_col]]) & !is.na(df$n_tested) & df$n_tested > 0,
        df[[n_col]] / df$n_tested,
        NA_real_
      )
    } else {
      result$pct <- NA_real_
    }

    result
  })

  # Remove NULL entries and combine
  tidy_list <- tidy_list[!sapply(tidy_list, is.null)]

  if (length(tidy_list) == 0) {
    return(id_assessment_aggs(df))
  }

  # Combine all levels
  result <- dplyr::bind_rows(tidy_list)

  # Reorder columns and add aggregation flags
  result <- result |>
    dplyr::select(
      dplyr::all_of(invariants),
      proficiency_level,
      n_students,
      pct
    ) |>
    id_assessment_aggs()

  result
}


#' Identify assessment aggregation levels
#'
#' Adds boolean flags to identify state, district, and school level records.
#'
#' @param df Assessment dataframe, output of tidy_assessment or process_assessment
#' @return data.frame with boolean aggregation flags
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_assessment(2024)
#' # Data already has aggregation flags via id_assessment_aggs
#' table(tidy_data$is_state, tidy_data$is_district, tidy_data$is_school)
#' }
id_assessment_aggs <- function(df) {
  df |>
    dplyr::mutate(
      # State level: Type == "State"
      is_state = type == "State",

      # District level: Type == "District"
      is_district = type == "District",

      # School level: Type == "School"
      is_school = type == "School"
    )
}


#' Calculate proficiency summary statistics
#'
#' Creates a summary table with proficiency rates aggregated by specified grouping.
#'
#' @param df Tidy assessment data frame
#' @param ... Grouping variables (unquoted)
#' @return Summary data frame with proficiency statistics
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_assessment(2024)
#' # Proficiency by subject and grade
#' assessment_summary(tidy_data, subject, grade)
#'
#' # Proficiency by subgroup
#' assessment_summary(tidy_data, subgroup)
#' }
assessment_summary <- function(df, ...) {

  # Check if tidy format (has proficiency_level column)
  if (!"proficiency_level" %in% names(df)) {
    stop("Input must be tidy assessment data (use tidy = TRUE)")
  }

  df |>
    dplyr::group_by(..., proficiency_level) |>
    dplyr::summarize(
      n_students = sum(n_students, na.rm = TRUE),
      n_tested = sum(n_tested, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct = dplyr::case_when(
        n_tested > 0 ~ n_students / n_tested,
        TRUE ~ NA_real_
      )
    )
}


#' Calculate proficiency rates (meets + exceeds)
#'
#' Calculates the combined proficiency rate (students scoring meets or exceeds)
#' for each row or group in the assessment data.
#'
#' @param df Tidy assessment data frame (must have proficiency_level column)
#' @param ... Optional grouping variables (unquoted)
#' @return Data frame with proficiency rates
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_assessment(2024)
#' # Overall proficiency by state/district/school
#' calc_proficiency(tidy_data)
#'
#' # Proficiency by subject
#' calc_proficiency(tidy_data, subject)
#' }
calc_proficiency <- function(df, ...) {

  if (!"proficiency_level" %in% names(df)) {
    stop("Input must be tidy assessment data (use tidy = TRUE)")
  }

  group_vars <- rlang::enquos(...)

  # If grouping vars provided, group by them plus base identifiers
  if (length(group_vars) > 0) {
    result <- df |>
      dplyr::filter(proficiency_level %in% c("meets", "exceeds")) |>
      dplyr::group_by(!!!group_vars) |>
      dplyr::summarize(
        n_proficient = sum(n_students, na.rm = TRUE),
        n_tested = dplyr::first(n_tested),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_proficient = dplyr::case_when(
          n_tested > 0 ~ n_proficient / n_tested,
          TRUE ~ NA_real_
        )
      )
  } else {
    # Group by row identifiers
    id_cols <- c("end_year", "type", "district_id", "school_id",
                 "test", "subject", "grade", "subgroup")
    id_cols <- id_cols[id_cols %in% names(df)]

    result <- df |>
      dplyr::filter(proficiency_level %in% c("meets", "exceeds")) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::summarize(
        n_proficient = sum(n_students, na.rm = TRUE),
        n_tested = dplyr::first(n_tested),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_proficient = dplyr::case_when(
          n_tested > 0 ~ n_proficient / n_tested,
          TRUE ~ NA_real_
        )
      )
  }

  result
}


#' Create empty tidy assessment data frame
#'
#' @return Empty data frame with tidy assessment columns
#' @keywords internal
create_empty_tidy_assessment <- function() {
  data.frame(
    end_year = integer(0),
    type = character(0),
    district_id = character(0),
    district_name = character(0),
    school_id = character(0),
    school_name = character(0),
    test = character(0),
    subject = character(0),
    grade = character(0),
    subgroup = character(0),
    n_tested = integer(0),
    proficiency_level = character(0),
    n_students = numeric(0),
    pct = numeric(0),
    is_state = logical(0),
    is_district = logical(0),
    is_school = logical(0),
    stringsAsFactors = FALSE
  )
}
