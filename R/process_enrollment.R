# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw MDE enrollment data into a
# clean, standardized format.
#
# ==============================================================================

#' Process raw MDE enrollment data
#'
#' Transforms raw MDE data into a standardized schema combining school
#' and district data.
#'
#' @param raw_data List containing school and district data frames from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process school data
  school_processed <- process_school_enr(raw_data$school, end_year)

  # Process district data
  district_processed <- process_district_enr(raw_data$district, end_year)

  # Create state aggregate
  state_processed <- create_state_aggregate(district_processed, end_year)

  # Combine all levels
  result <- dplyr::bind_rows(state_processed, district_processed, school_processed)

  result
}


#' Process school-level enrollment data
#'
#' @param df Raw school data frame
#' @param end_year School year end
#' @return Processed school data frame
#' @keywords internal
process_school_enr <- function(df, end_year) {

  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }

  cols <- names(df)
  n_rows <- nrow(df)
  col_map <- get_mde_column_map(end_year)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", gsub("([\\[\\]()])", "\\\\\\1", pattern), "$"),
                      cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("School", n_rows),
    stringsAsFactors = FALSE
  )

  # School and District IDs
  school_col <- find_col(col_map$school_id)
  if (!is.null(school_col)) {
    # Minnesota uses Type-Number format: e.g., "03" for district type, "0001" for number
    # Full school ID format: district_type + district_number + school_number
    result$school_id <- trimws(as.character(df[[school_col]]))
  }

  district_col <- find_col(col_map$district_id)
  if (!is.null(district_col)) {
    result$district_id <- trimws(as.character(df[[district_col]]))
  }

  # If we have school ID but not district ID, try to extract district from school ID
  if (is.null(district_col) && !is.null(school_col)) {
    # Minnesota school IDs often embed district info
    # Try common patterns
    school_ids <- trimws(as.character(df[[school_col]]))
    # If IDs are long enough, first portion may be district
    if (all(nchar(school_ids) >= 6, na.rm = TRUE)) {
      result$district_id <- substr(school_ids, 1, 6)
    }
  }

  # Names
  school_name_col <- find_col(col_map$school_name)
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(as.character(df[[school_name_col]]))
  }

  district_name_col <- find_col(col_map$district_name)
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(as.character(df[[district_name_col]]))
  }

  # County
  county_col <- find_col(col_map$county)
  if (!is.null(county_col)) {
    result$county <- trimws(as.character(df[[county_col]]))
  }

  # Total enrollment
  total_col <- find_col(col_map$total)
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics - Race/Ethnicity
  demo_fields <- c("white", "black", "hispanic", "asian",
                   "native_american", "pacific_islander", "multiracial")
  for (field in demo_fields) {
    col <- find_col(col_map[[field]])
    if (!is.null(col)) {
      result[[field]] <- safe_numeric(df[[col]])
    }
  }

  # Gender
  gender_fields <- c("male", "female")
  for (field in gender_fields) {
    col <- find_col(col_map[[field]])
    if (!is.null(col)) {
      result[[field]] <- safe_numeric(df[[col]])
    }
  }

  # Special populations
  special_fields <- c("econ_disadv", "lep", "special_ed", "homeless")
  for (field in special_fields) {
    col <- find_col(col_map[[field]])
    if (!is.null(col)) {
      result[[field]] <- safe_numeric(df[[col]])
    }
  }

  # Grade levels
  grade_fields <- c("grade_pk", "grade_k",
                    "grade_01", "grade_02", "grade_03", "grade_04",
                    "grade_05", "grade_06", "grade_07", "grade_08",
                    "grade_09", "grade_10", "grade_11", "grade_12")
  for (field in grade_fields) {
    col <- find_col(col_map[[field]])
    if (!is.null(col)) {
      result[[field]] <- safe_numeric(df[[col]])
    }
  }

  # Standardize column to match txschooldata pattern
  # Minnesota uses "school" terminology; standardize to "campus" for consistency
  if ("school_id" %in% names(result)) {
    result$campus_id <- result$school_id
    result$school_id <- NULL
  }
  if ("school_name" %in% names(result)) {
    result$campus_name <- result$school_name
    result$school_name <- NULL
  }

  result$type <- "Campus"

  result
}


#' Process district-level enrollment data
#'
#' @param df Raw district data frame
#' @param end_year School year end
#' @return Processed district data frame
#' @keywords internal
process_district_enr <- function(df, end_year) {

  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }

  cols <- names(df)
  n_rows <- nrow(df)
  col_map <- get_mde_column_map(end_year)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", gsub("([\\[\\]()])", "\\\\\\1", pattern), "$"),
                      cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("District", n_rows),
    stringsAsFactors = FALSE
  )

  # District ID
  district_col <- find_col(col_map$district_id)
  if (!is.null(district_col)) {
    result$district_id <- trimws(as.character(df[[district_col]]))
  }

  # Campus ID is NA for district rows
  result$campus_id <- rep(NA_character_, n_rows)

  # Names
  district_name_col <- find_col(col_map$district_name)
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(as.character(df[[district_name_col]]))
  }

  result$campus_name <- rep(NA_character_, n_rows)

  # County
  county_col <- find_col(col_map$county)
  if (!is.null(county_col)) {
    result$county <- trimws(as.character(df[[county_col]]))
  }

  # Total enrollment
  total_col <- find_col(col_map$total)
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics - Race/Ethnicity
  demo_fields <- c("white", "black", "hispanic", "asian",
                   "native_american", "pacific_islander", "multiracial")
  for (field in demo_fields) {
    col <- find_col(col_map[[field]])
    if (!is.null(col)) {
      result[[field]] <- safe_numeric(df[[col]])
    }
  }

  # Gender
  gender_fields <- c("male", "female")
  for (field in gender_fields) {
    col <- find_col(col_map[[field]])
    if (!is.null(col)) {
      result[[field]] <- safe_numeric(df[[col]])
    }
  }

  # Special populations
  special_fields <- c("econ_disadv", "lep", "special_ed", "homeless")
  for (field in special_fields) {
    col <- find_col(col_map[[field]])
    if (!is.null(col)) {
      result[[field]] <- safe_numeric(df[[col]])
    }
  }

  # Grade levels
  grade_fields <- c("grade_pk", "grade_k",
                    "grade_01", "grade_02", "grade_03", "grade_04",
                    "grade_05", "grade_06", "grade_07", "grade_08",
                    "grade_09", "grade_10", "grade_11", "grade_12")
  for (field in grade_fields) {
    col <- find_col(col_map[[field]])
    if (!is.null(col)) {
      result[[field]] <- safe_numeric(df[[col]])
    }
  }

  result
}


#' Create state-level aggregate from district data
#'
#' @param district_df Processed district data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(district_df, end_year) {

  if (is.null(district_df) || nrow(district_df) == 0) {
    return(data.frame())
  }

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "male", "female",
    "econ_disadv", "lep", "special_ed", "homeless",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
  }

  state_row
}
