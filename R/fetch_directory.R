# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading and processing school/district
# directory data from MDE-ORG (Minnesota Department of Education Organization
# Reference Glossary).
#
# Data Sources:
#   - Schools: https://pub.education.mn.gov/MdeOrgView/tag/extractContacts/MDEORG_SCHOOL?description=
#   - Districts/LEAs: https://pub.education.mn.gov/MdeOrgView/tag/extractContacts/MDEORG_LEA?description=
#
# The school directory includes:
#   - School and district names and IDs (state org ID format)
#   - Principal name and email
#   - Address (mailing and physical) and phone
#   - Grade span and school classification
#   - County, NCES ID, web URL
#
# The district directory includes:
#   - District name, ID, and type
#   - Superintendent name and email
#   - Address and phone
#   - County and web URL
#
# Note: MDE-ORG is a live directory (not historical). There is no year parameter;
# the extract always returns the current directory snapshot. The `end_year`
# parameter in fetch_directory() reflects the school year of the snapshot.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# Directory-specific cache helpers
# ------------------------------------------------------------------------------

#' Get directory cache file path
#'
#' @param end_year School year end
#' @param type Cache type ("tidy", "raw", "districts")
#' @return Full path to cache file
#' @keywords internal
get_directory_cache_path <- function(end_year, type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0("dir_", type, "_", end_year, ".rds"))
}


#' Check if directory cache exists and is valid
#'
#' @param end_year School year end
#' @param type Cache type
#' @param max_age Maximum age in days (default 30)
#' @return TRUE if valid cache exists
#' @keywords internal
directory_cache_exists <- function(end_year, type, max_age = 30) {
  cache_path <- get_directory_cache_path(end_year, type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param end_year School year end
#' @param type Cache type
#' @return Cached data
#' @keywords internal
read_directory_cache <- function(end_year, type) {
  cache_path <- get_directory_cache_path(end_year, type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param data Data to cache
#' @param end_year School year end
#' @param type Cache type
#' @return Invisibly returns the cache path
#' @keywords internal
write_directory_cache <- function(data, end_year, type) {
  cache_path <- get_directory_cache_path(end_year, type)
  saveRDS(data, cache_path)
  invisible(cache_path)
}


# ------------------------------------------------------------------------------
# URL construction
# ------------------------------------------------------------------------------

#' URLs for MDE-ORG directory extracts
#'
#' Returns the extract URLs for school and district directory data from MDE-ORG.
#'
#' @param type Either "school" or "district"
#' @return URL string for CSV extract
#' @keywords internal
get_directory_url <- function(type = "school") {
  base_url <- "https://pub.education.mn.gov/MdeOrgView/tag/extractContacts"
  switch(
    type,
    "school" = paste0(base_url, "/MDEORG_SCHOOL?description="),
    "district" = paste0(base_url, "/MDEORG_LEA?description="),
    stop("type must be 'school' or 'district'")
  )
}


# ------------------------------------------------------------------------------
# Raw data download
# ------------------------------------------------------------------------------

#' Download raw school directory data from MDE-ORG
#'
#' Downloads the school directory CSV extract from the MDE Organization
#' Reference Glossary (MDE-ORG).
#'
#' @return Data frame with raw school directory data
#' @keywords internal
get_raw_school_directory <- function() {

  url <- get_directory_url("school")
  message("Downloading MDE-ORG school directory...")

  temp_file <- tempfile(
    pattern = "mde_dir_school_",
    tmpdir = tempdir(),
    fileext = ".csv"
  )

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::timeout(120),
      httr::user_agent("mnschooldata R package")
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Verify file size (empty responses are tiny)
    file_info <- file.info(temp_file)
    if (is.na(file_info$size) || file_info$size < 100) {
      stop("Downloaded file is too small - may be an error page")
    }

    # Read CSV - all as character for consistent processing
    df <- utils::read.csv(
      temp_file,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )

    df

  }, error = function(e) {
    stop(paste(
      "Failed to download MN school directory from MDE-ORG.\n",
      "URL:", url, "\n",
      "Error:", e$message, "\n",
      "Check: https://pub.education.mn.gov/MdeOrgView/districts/index"
    ))
  }, finally = {
    if (file.exists(temp_file)) unlink(temp_file)
  })
}


#' Download raw district directory data from MDE-ORG
#'
#' Downloads the district (LEA) directory CSV extract from the MDE Organization
#' Reference Glossary (MDE-ORG).
#'
#' @return Data frame with raw district directory data
#' @keywords internal
get_raw_district_directory <- function() {

  url <- get_directory_url("district")
  message("Downloading MDE-ORG district directory...")

  temp_file <- tempfile(
    pattern = "mde_dir_district_",
    tmpdir = tempdir(),
    fileext = ".csv"
  )

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::timeout(120),
      httr::user_agent("mnschooldata R package")
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Verify file size
    file_info <- file.info(temp_file)
    if (is.na(file_info$size) || file_info$size < 100) {
      stop("Downloaded file is too small - may be an error page")
    }

    # Read CSV - all as character for consistent processing
    df <- utils::read.csv(
      temp_file,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )

    df

  }, error = function(e) {
    stop(paste(
      "Failed to download MN district directory from MDE-ORG.\n",
      "URL:", url, "\n",
      "Error:", e$message, "\n",
      "Check: https://pub.education.mn.gov/MdeOrgView/districts/index"
    ))
  }, finally = {
    if (file.exists(temp_file)) unlink(temp_file)
  })
}


# ------------------------------------------------------------------------------
# Processing functions
# ------------------------------------------------------------------------------

#' Process raw school directory data
#'
#' Transforms raw MDE-ORG school directory data into a standardized schema.
#'
#' @param raw_data Data frame from get_raw_school_directory
#' @return Processed data frame with standardized columns
#' @keywords internal
process_school_directory <- function(raw_data) {

  # Build state_school_id from District Number + District Type + School Number
  # Format: DDDD-TT-SSS (e.g., 0001-01-001)
  state_school_id <- sprintf(
    "%04d-%02d-%03d",
    as.integer(raw_data[["District Number"]]),
    as.integer(raw_data[["District Type"]]),
    as.integer(raw_data[["School Number"]])
  )

  state_district_id <- sprintf(
    "%04d-%02d",
    as.integer(raw_data[["District Number"]]),
    as.integer(raw_data[["District Type"]])
  )

  result <- data.frame(
    entity_type = "school",
    state_district_id = state_district_id,
    state_school_id = state_school_id,
    state_org_id = trimws(raw_data[["StateOrganizationId"]]),
    nces_id = trimws(raw_data[["NCES ID"]]),
    district_name = NA_character_,   # Filled in from district data during combine
    school_name = trimws(raw_data[["Organization"]]),
    school_classification = trimws(raw_data[["School Classification"]]),
    grades_served = trimws(raw_data[["Grades"]]),
    principal_name = trimws(raw_data[["Name"]]),
    principal_first_name = trimws(raw_data[["First Name"]]),
    principal_last_name = trimws(raw_data[["Last Name"]]),
    principal_email = trimws(raw_data[["Email"]]),
    phone = trimws(raw_data[["Phone"]]),
    physical_address = trimws(raw_data[["Physical Line 1"]]),
    physical_address_2 = trimws(raw_data[["Physical Line 2"]]),
    physical_city = trimws(raw_data[["Physical City"]]),
    physical_state = trimws(raw_data[["Physical State"]]),
    physical_zip = trimws(raw_data[["Physical Zip"]]),
    mailing_address = trimws(raw_data[["Mailing Line 1"]]),
    mailing_city = trimws(raw_data[["Mailing City"]]),
    mailing_state = trimws(raw_data[["Mailing State"]]),
    mailing_zip = trimws(raw_data[["Mailing Zip"]]),
    county_name = trimws(raw_data[["County"]]),
    eco_dev_region = trimws(raw_data[["Eco Dev Region"]]),
    website = trimws(raw_data[["Web URL"]]),
    data_extracted = trimws(raw_data[["Data Extracted"]]),
    stringsAsFactors = FALSE
  )

  # Clean up empty strings to NA
  result[] <- lapply(result, function(col) {
    if (is.character(col)) {
      col[col == ""] <- NA_character_
    }
    col
  })

  result
}


#' Process raw district directory data
#'
#' Transforms raw MDE-ORG district (LEA) directory data into a standardized schema.
#'
#' @param raw_data Data frame from get_raw_district_directory
#' @return Processed data frame with standardized columns
#' @keywords internal
process_district_directory <- function(raw_data) {

  state_district_id <- sprintf(
    "%04d-%02d",
    as.integer(raw_data[["District Number"]]),
    as.integer(raw_data[["District Type"]])
  )

  result <- data.frame(
    entity_type = "district",
    state_district_id = state_district_id,
    state_org_id = trimws(raw_data[["StateOrganizationId"]]),
    nces_id = trimws(raw_data[["NCES ID"]]),
    district_name = trimws(raw_data[["Organization"]]),
    superintendent_name = trimws(raw_data[["Name"]]),
    superintendent_title = trimws(raw_data[["Title"]]),
    superintendent_first_name = trimws(raw_data[["First Name"]]),
    superintendent_last_name = trimws(raw_data[["Last Name"]]),
    superintendent_email = trimws(raw_data[["Email"]]),
    phone = trimws(raw_data[["Phone"]]),
    physical_address = trimws(raw_data[["Physical Line 1"]]),
    physical_address_2 = trimws(raw_data[["Physical Line 2"]]),
    physical_city = trimws(raw_data[["Physical City"]]),
    physical_state = trimws(raw_data[["Physical State"]]),
    physical_zip = trimws(raw_data[["Physical Zip"]]),
    mailing_address = trimws(raw_data[["Mailing Line 1"]]),
    mailing_city = trimws(raw_data[["Mailing City"]]),
    mailing_state = trimws(raw_data[["Mailing State"]]),
    mailing_zip = trimws(raw_data[["Mailing Zip"]]),
    county_name = trimws(raw_data[["County"]]),
    eco_dev_region = trimws(raw_data[["Eco Dev Region"]]),
    website = trimws(raw_data[["Web URL"]]),
    data_extracted = trimws(raw_data[["Data Extracted"]]),
    stringsAsFactors = FALSE
  )

  # Clean up empty strings to NA
  result[] <- lapply(result, function(col) {
    if (is.character(col)) {
      col[col == ""] <- NA_character_
    }
    col
  })

  result
}


#' Combine school and district directory data
#'
#' Merges school-level directory data with district-level data to enrich
#' school records with district names and superintendent information.
#'
#' @param schools Processed school directory data frame
#' @param districts Processed district directory data frame
#' @return Combined data frame with school records enriched by district info
#' @keywords internal
combine_directory <- function(schools, districts) {

  # Extract district-level columns for merging into school data
  district_lookup <- districts[, c(
    "state_district_id",
    "district_name",
    "superintendent_name",
    "superintendent_email"
  )]

  # Deduplicate district lookup (some districts have multiple superintendent rows)
  district_lookup <- district_lookup[!duplicated(district_lookup$state_district_id), ]

  # Merge district info into school records
  enriched <- merge(
    schools,
    district_lookup,
    by = "state_district_id",
    all.x = TRUE,
    suffixes = c("", "_from_district")
  )

  # Fill in the district_name from the lookup
  enriched$district_name <- enriched$district_name_from_district
  enriched$district_name_from_district <- NULL

  # Reorder columns for clarity
  col_order <- c(
    "entity_type",
    "state_district_id", "state_school_id", "state_org_id", "nces_id",
    "district_name", "school_name",
    "school_classification", "grades_served",
    "principal_name", "principal_first_name", "principal_last_name",
    "principal_email",
    "superintendent_name", "superintendent_email",
    "phone",
    "physical_address", "physical_address_2",
    "physical_city", "physical_state", "physical_zip",
    "mailing_address", "mailing_city", "mailing_state", "mailing_zip",
    "county_name", "eco_dev_region",
    "website", "data_extracted"
  )
  col_order <- col_order[col_order %in% names(enriched)]
  enriched <- enriched[, col_order]

  enriched
}


# ------------------------------------------------------------------------------
# User-facing functions
# ------------------------------------------------------------------------------

#' Fetch Minnesota school directory data
#'
#' Downloads and processes school and district directory data from the MDE
#' Organization Reference Glossary (MDE-ORG). Returns a combined dataset
#' with school information, including principal and superintendent contacts.
#'
#' MDE-ORG is a live directory -- the extract always returns the current
#' snapshot. There is no historical year parameter. The \code{end_year}
#' parameter is used only for cache key labeling.
#'
#' @param end_year A school year end used as the cache key. Defaults to the
#'   current school year (July-June). For example, in March 2026 the current
#'   school year is 2025-26, so end_year defaults to 2026.
#' @param tidy If TRUE (default), returns combined school+district data with
#'   superintendent info merged into school records. If FALSE, returns a list
#'   with separate \code{$schools} and \code{$districts} data frames.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from MDE-ORG.
#' @return If tidy=TRUE, a data frame with school-level directory data enriched
#'   with district/superintendent info. If tidy=FALSE, a list with:
#'   \itemize{
#'     \item \code{schools}: School directory data with principal contacts
#'     \item \code{districts}: District directory data with superintendent contacts
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Get current directory (combined schools + districts)
#' dir <- fetch_directory()
#'
#' # Get separate school and district data frames
#' dir_raw <- fetch_directory(tidy = FALSE)
#' dir_raw$schools
#' dir_raw$districts
#'
#' # Force fresh download (bypass cache)
#' dir_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Find schools in Minneapolis
#' mpls <- dir |>
#'   dplyr::filter(grepl("Minneapolis", district_name, ignore.case = TRUE))
#' }
fetch_directory <- function(end_year = NULL, tidy = TRUE, use_cache = TRUE) {

  # Default end_year to current school year
  # School year runs July-June: if we're in Jan-June, it's the current year.
  # If Jul-Dec, it's next year.
  if (is.null(end_year)) {
    current_month <- as.integer(format(Sys.Date(), "%m"))
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    end_year <- if (current_month >= 7) current_year + 1L else current_year
  }

  # Determine cache type
  cache_type <- if (tidy) "tidy" else "raw"

  # Check cache first
  if (use_cache && directory_cache_exists(end_year, cache_type)) {
    message(paste("Using cached directory data for", end_year))
    return(read_directory_cache(end_year, cache_type))
  }

  # Download raw data
  raw_schools <- get_raw_school_directory()
  raw_districts <- get_raw_district_directory()

  # Process to standardized schema
  processed_schools <- process_school_directory(raw_schools)
  processed_districts <- process_district_directory(raw_districts)

  if (tidy) {
    result <- combine_directory(processed_schools, processed_districts)
  } else {
    result <- list(
      schools = processed_schools,
      districts = processed_districts
    )
  }

  # Cache the result
  if (use_cache) {
    write_directory_cache(result, end_year, cache_type)
  }

  result
}


#' Fetch Minnesota district directory data
#'
#' Downloads and processes district-level directory data from MDE-ORG.
#' Returns district information with superintendent contacts.
#'
#' @param end_year School year end (cache key). Defaults to current school year.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Data frame with district directory data including superintendent contacts
#' @export
#' @examples
#' \dontrun{
#' # Get district directory
#' districts <- fetch_district_directory()
#'
#' # Find districts in Hennepin County
#' hennepin <- districts |>
#'   dplyr::filter(grepl("Hennepin", county_name))
#' }
fetch_district_directory <- function(end_year = NULL, use_cache = TRUE) {

  # Default end_year
  if (is.null(end_year)) {
    current_month <- as.integer(format(Sys.Date(), "%m"))
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    end_year <- if (current_month >= 7) current_year + 1L else current_year
  }

  cache_type <- "districts"

  # Check cache
  if (use_cache && directory_cache_exists(end_year, cache_type)) {
    message(paste("Using cached district directory data for", end_year))
    return(read_directory_cache(end_year, cache_type))
  }

  # Download and process
  raw <- get_raw_district_directory()
  result <- process_district_directory(raw)

  # Cache
  if (use_cache) {
    write_directory_cache(result, end_year, cache_type)
  }

  result
}


#' Clear directory data cache
#'
#' Removes cached directory data files.
#'
#' @param end_year Optional school year to clear. If NULL, clears all directory cache.
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear all directory cache
#' clear_directory_cache()
#'
#' # Clear specific year
#' clear_directory_cache(2026)
#' }
clear_directory_cache <- function(end_year = NULL) {
  cache_dir <- get_cache_dir()

  if (!is.null(end_year)) {
    # Clear all directory types for this year
    files <- list.files(
      cache_dir,
      pattern = paste0("^dir_.*_", end_year, "\\.rds$"),
      full.names = TRUE
    )
  } else {
    # Clear all directory cache files
    files <- list.files(
      cache_dir,
      pattern = "^dir_",
      full.names = TRUE
    )
  }

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}
