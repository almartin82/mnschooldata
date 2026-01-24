# ==============================================================================
# Assessment Data Fetching Functions
# ==============================================================================
#
# This file contains the main user-facing functions for downloading assessment
# data from the Minnesota Department of Education (MDE).
#
# Available years: 2019, 2021, 2022, 2023, 2024, 2025 (no 2020 due to COVID waiver)
#
# ==============================================================================

#' Fetch Minnesota assessment data
#'
#' Downloads and processes assessment data from the Minnesota Department of
#' Education's MDEAnalytics portal.
#'
#' Data is available from 2019-2025, excluding 2020 (COVID-19 testing waiver).
#' Assessment data includes proficiency levels for MCA (Minnesota Comprehensive
#' Assessment) and MTAS (Minnesota Test of Academic Skills):
#' - Does Not Meet (D)
#' - Partially Meets (P)
#' - Meets (M)
#' - Exceeds (E)
#'
#' For MCA-IV (2025+), the labels change to:
#' - Beginning
#' - Intermediate
#' - Meets
#' - Advanced
#'
#' @param end_year A school year. Year is the end of the academic year - e.g.,
#'   2023-24 school year is year '2024'. Valid values are 2019, 2021-2025.
#' @param level Level of data to fetch: "all" (default), "state", "district", or "school"
#' @param tidy If TRUE (default), returns data in long (tidy) format with proficiency_level
#'   column. If FALSE, returns wide format with separate pct_* columns.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from MDE.
#' @return Data frame with assessment data. Wide format includes columns for
#'   proficiency percentages (pct_does_not_meet, pct_partially_meets, pct_meets,
#'   pct_exceeds). Tidy format pivots these into proficiency_level and pct columns.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 assessment data (2023-24 school year)
#' assess_2024 <- fetch_assessment(2024)
#'
#' # Get only state-level data
#' state_assess <- fetch_assessment(2024, level = "state")
#'
#' # Get wide format (pct columns not pivoted)
#' assess_wide <- fetch_assessment(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' assess_fresh <- fetch_assessment(2024, use_cache = FALSE)
#'
#' # Filter to math results
#' math_results <- assess_2024 |>
#'   dplyr::filter(subject == "Math", is_state)
#' }
fetch_assessment <- function(end_year, level = "all", tidy = TRUE, use_cache = TRUE) {

  # Validate year
  available <- get_available_assessment_years()
  if (!end_year %in% available$years) {
    if (end_year == 2020) {
      stop(available$note)
    }
    stop(paste0(
      "end_year must be one of: ", paste(available$years, collapse = ", "),
      "\nGot: ", end_year
    ))
  }

  # Validate level
  level <- tolower(level)
  if (!level %in% c("all", "state", "district", "school")) {
    stop("level must be one of 'all', 'state', 'district', 'school'")
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && assessment_cache_exists(end_year, cache_type, level)) {
    message(paste("Using cached assessment data for", end_year))
    return(read_assessment_cache(end_year, cache_type, level))
  }

  # Get raw data from MDE
  raw <- get_raw_assessment(end_year, level)

  # Process to standard schema
  processed <- process_assessment(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_assessment(processed)
  } else {
    # Add aggregation flags to wide format too
    processed <- id_assessment_aggs(processed)
  }

  # Cache the result
  if (use_cache) {
    write_assessment_cache(processed, end_year, cache_type, level)
  }

  processed
}


#' Fetch assessment data for multiple years
#'
#' Downloads and combines assessment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#'   Note: 2020 is not available due to COVID-19 testing waiver.
#' @param level Level of data to fetch: "all" (default), "state", "district", or "school"
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with assessment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data (excluding 2020)
#' assess_multi <- fetch_assessment_multi(c(2021, 2022, 2023))
#'
#' # Track proficiency trends at state level
#' assess_multi |>
#'   dplyr::filter(is_state, subject == "Math", grade == "All",
#'                 subgroup == "All Students") |>
#'   dplyr::filter(proficiency_level %in% c("meets", "exceeds")) |>
#'   dplyr::group_by(end_year) |>
#'   dplyr::summarize(pct_proficient = sum(pct, na.rm = TRUE))
#' }
fetch_assessment_multi <- function(end_years, level = "all", tidy = TRUE, use_cache = TRUE) {

  # Validate years
  available <- get_available_assessment_years()

  # Check for 2020
  if (2020 %in% end_years) {
    warning("2020 excluded: ", available$note)
    end_years <- end_years[end_years != 2020]
  }

  invalid_years <- end_years[!end_years %in% available$years]
  if (length(invalid_years) > 0) {
    stop(paste0(
      "Invalid years: ", paste(invalid_years, collapse = ", "),
      "\nend_year must be one of: ", paste(available$years, collapse = ", ")
    ))
  }

  if (length(end_years) == 0) {
    stop("No valid years to fetch")
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_assessment(yr, level = level, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}


#' Get assessment data for a specific district
#'
#' Convenience function to fetch assessment data for a single district.
#'
#' @param end_year School year end
#' @param district_id 4-digit district ID (e.g., "0001" for Minneapolis)
#' @param tidy If TRUE (default), returns tidy format
#' @param use_cache If TRUE (default), uses cached data
#' @return Data frame filtered to specified district
#' @export
#' @examples
#' \dontrun{
#' # Get Minneapolis (district 0001) assessment data
#' mpls_assess <- fetch_district_assessment(2024, "0001")
#'
#' # Get St. Paul (district 0625) data
#' stpaul_assess <- fetch_district_assessment(2024, "0625")
#' }
fetch_district_assessment <- function(end_year, district_id, tidy = TRUE, use_cache = TRUE) {

  # Normalize district_id
  district_id <- sprintf("%04d", as.integer(district_id))

  # Fetch district-level data (faster than fetching all)
  df <- fetch_assessment(end_year, level = "district", tidy = tidy, use_cache = use_cache)

  # Filter to requested district
  df |>
    dplyr::filter(district_id == !!district_id)
}


#' Get assessment data for a specific school
#'
#' Convenience function to fetch assessment data for a single school.
#'
#' @param end_year School year end
#' @param district_id 4-digit district ID
#' @param school_id 3-digit school ID
#' @param tidy If TRUE (default), returns tidy format
#' @param use_cache If TRUE (default), uses cached data
#' @return Data frame filtered to specified school
#' @export
#' @examples
#' \dontrun{
#' # Get a specific school's assessment data
#' school_assess <- fetch_school_assessment(2024, "0001", "001")
#' }
fetch_school_assessment <- function(end_year, district_id, school_id, tidy = TRUE, use_cache = TRUE) {

  # Normalize IDs
  district_id <- sprintf("%04d", as.integer(district_id))
  school_id <- sprintf("%03d", as.integer(school_id))

  # Fetch school-level data
  df <- fetch_assessment(end_year, level = "school", tidy = tidy, use_cache = use_cache)

  # Filter to requested school
  df |>
    dplyr::filter(district_id == !!district_id, school_id == !!school_id)
}


# ==============================================================================
# Assessment Cache Functions
# ==============================================================================

#' Get assessment cache file path
#'
#' @param end_year School year end
#' @param type Data type ("tidy" or "wide")
#' @param level Data level ("all", "state", "district", "school")
#' @return Full path to cache file
#' @keywords internal
get_assessment_cache_path <- function(end_year, type, level) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0("assess_", type, "_", level, "_", end_year, ".rds"))
}


#' Check if assessment cache exists
#'
#' @param end_year School year end
#' @param type Data type ("tidy" or "wide")
#' @param level Data level
#' @param max_age Maximum age in days (default 30)
#' @return TRUE if valid cache exists
#' @keywords internal
assessment_cache_exists <- function(end_year, type, level, max_age = 30) {
  cache_path <- get_assessment_cache_path(end_year, type, level)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read assessment data from cache
#'
#' @param end_year School year end
#' @param type Data type ("tidy" or "wide")
#' @param level Data level
#' @return Cached data frame
#' @keywords internal
read_assessment_cache <- function(end_year, type, level) {
  cache_path <- get_assessment_cache_path(end_year, type, level)
  readRDS(cache_path)
}


#' Write assessment data to cache
#'
#' @param df Data frame to cache
#' @param end_year School year end
#' @param type Data type ("tidy" or "wide")
#' @param level Data level
#' @return Invisibly returns the cache path
#' @keywords internal
write_assessment_cache <- function(df, end_year, type, level) {
  cache_path <- get_assessment_cache_path(end_year, type, level)
  saveRDS(df, cache_path)
  invisible(cache_path)
}


#' Clear assessment cache
#'
#' Removes cached assessment data files.
#'
#' @param end_year Optional school year to clear. If NULL, clears all years.
#' @param type Optional data type to clear ("tidy" or "wide"). If NULL, clears all types.
#' @param level Optional level to clear. If NULL, clears all levels.
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear all cached assessment data
#' clear_assessment_cache()
#'
#' # Clear only 2024 assessment data
#' clear_assessment_cache(2024)
#'
#' # Clear only tidy format assessment data
#' clear_assessment_cache(type = "tidy")
#' }
clear_assessment_cache <- function(end_year = NULL, type = NULL, level = NULL) {
  cache_dir <- get_cache_dir()

  # Build pattern
  pattern <- "^assess_"

  if (!is.null(type)) {
    pattern <- paste0(pattern, type, "_")
  } else {
    pattern <- paste0(pattern, "[^_]+_")
  }

  if (!is.null(level)) {
    pattern <- paste0(pattern, level, "_")
  } else {
    pattern <- paste0(pattern, "[^_]+_")
  }

  if (!is.null(end_year)) {
    pattern <- paste0(pattern, end_year, "\\.rds$")
  } else {
    pattern <- paste0(pattern, "\\d{4}\\.rds$")
  }

  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached assessment file(s)"))
  } else {
    message("No cached assessment files to remove")
  }

  invisible(length(files))
}
