# ==============================================================================
# Raw Assessment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw assessment data from MDE.
#
# Minnesota Department of Education provides assessment data through the
# MDEAnalytics portal at pub.education.mn.gov. Assessment data includes MCA
# (Minnesota Comprehensive Assessment) and MTAS (Minnesota Test of Academic
# Skills) results.
#
# Test Eras:
# - MCA-II (2006-2012): Original comprehensive assessments
# - MCA-III (2013-2023): Standards-aligned revision
# - MCA-IV (2024+): Current version with new proficiency labels
#
# Available Years:
# - 2019, 2021-2025 (no 2020 due to COVID-19 testing waiver)
#
# File Format (2019+):
# - Excel files with tabs: Assessment Information, State, County, District,
#   School, Column Definitions
# - Proficiency levels: Does Not Meet, Partially Meets, Meets, Exceeds
#   (Beginning, Intermediate, Meets, Advanced for MCA-IV in 2025)
#
# ==============================================================================


#' Get available assessment years
#'
#' Returns information about years for which assessment data is available.
#'
#' @return List with 'years' (numeric vector) and 'note' (character)
#' @export
#' @examples
#' get_available_assessment_years()
get_available_assessment_years <- function() {
  list(
    years = c(2019, 2021, 2022, 2023, 2024, 2025),
    note = "2020 data not available due to COVID-19 testing waiver"
  )
}


#' Download raw assessment data from MDE
#'
#' Downloads assessment data from Minnesota Department of Education's
#' MDEAnalytics portal.
#'
#' @param end_year School year end (e.g., 2024 for 2023-24 school year)
#' @param level Data level: "all" (default), "state", "district", or "school"
#' @return List with state, district, and/or school data frames
#' @keywords internal
get_raw_assessment <- function(end_year, level = "all") {

  # Validate year
  available <- get_available_assessment_years()
  if (!end_year %in% available$years) {
    if (end_year == 2020) {
      stop("Assessment data is not available for 2020: ", available$note)
    }
    stop(paste0(
      "end_year must be one of: ", paste(available$years, collapse = ", "),
      "\nGot: ", end_year
    ))
  }

  message(paste("Downloading MDE assessment data for", end_year, "..."))

  # Determine which levels to download
  level <- tolower(level)
  if (level == "all") {
    levels_to_download <- c("state", "district", "school")
  } else if (level %in% c("state", "district", "school")) {
    levels_to_download <- level
  } else {
    stop("level must be one of 'all', 'state', 'district', 'school'")
  }

  # Download each level
  result <- list()

  for (lv in levels_to_download) {
    message(paste("  Downloading", lv, "level data..."))
    df <- download_assessment_file(end_year, lv)
    result[[lv]] <- df
  }

  result
}


#' Get assessment data URL
#'
#' Constructs the URL for downloading assessment data from MDE.
#' MDE uses WebFOCUS for data delivery, with URLs that include
#' year and level parameters.
#'
#' @param end_year School year end
#' @param level One of "state", "district", "school"
#' @return URL string or NULL if not available
#' @keywords internal
get_assessment_url <- function(end_year, level) {

  # Base URL for MDE Analytics data download
  # MDE provides Excel files through the WebFOCUS system
  base_url <- "https://pub.education.mn.gov/ibi_apps/WFServlet"

  # MDE Assessment files follow this pattern:
  # - IBIF_ex: Report execution ID (varies by report type)
  # - Year parameter
  # - Level parameter (State, District, School, County)
  # - Format: XLSX

  # The assessment data files URL patterns:
  # State: MCA_State_YYYY.xlsx
  # District: MCA_District_YYYY.xlsx
  # School: MCA_School_YYYY.xlsx

  # These files are accessed through the portal which generates them dynamically
  # We need to use the portal download approach

  level_cap <- switch(
    level,
    "state" = "State",
    "district" = "District",
    "school" = "School",
    "State"
  )

  # URL patterns observed from MDE portal
  # The files are generated via WebFOCUS and can be accessed through
  # a direct download endpoint after form submission

  # Build URL for direct file download (if available)
  # MDE typically hosts files at patterns like:
  url_patterns <- c(
    # Pattern 1: Direct download from MDE data files
    paste0("https://education.mn.gov/mdeprod/idcplg?IdcService=GET_FILE&",
           "dDocName=MDE_ASSESS_", level_cap, "_", end_year),
    # Pattern 2: MDEAnalytics file server
    paste0("https://pub.education.mn.gov/MDEAnalytics/DataFiles/",
           "Assessment_", end_year, "_", level_cap, ".xlsx"),
    # Pattern 3: Alternative MDEAnalytics path
    paste0("https://pub.education.mn.gov/MDEAnalytics/files/",
           "assessment_", tolower(level_cap), "_", end_year, ".xlsx")
  )

  url_patterns
}


#' Download assessment file
#'
#' Downloads a single assessment file from MDE.
#'
#' @param end_year School year end
#' @param level One of "state", "district", "school"
#' @return Data frame with assessment data
#' @keywords internal
download_assessment_file <- function(end_year, level) {

  # Create temp file
  tname <- tempfile(
    pattern = paste0("mde_assess_", level, "_"),
    tmpdir = tempdir(),
    fileext = ".xlsx"
  )

  # Try the WebFOCUS portal download approach
  download_success <- download_assessment_via_portal(end_year, level, tname)

  if (!download_success) {
    # Try direct URL patterns
    url_patterns <- get_assessment_url(end_year, level)

    for (url in url_patterns) {
      result <- tryCatch({
        response <- httr::GET(
          url,
          httr::write_disk(tname, overwrite = TRUE),
          httr::timeout(300),
          httr::user_agent("mnschooldata R package"),
          httr::config(ssl_verifypeer = 0L)
        )

        if (!httr::http_error(response)) {
          file_info <- file.info(tname)
          if (!is.na(file_info$size) && file_info$size > 1000) {
            # Verify it's a valid Excel file
            test_read <- tryCatch({
              readxl::read_excel(tname, n_max = 1)
              TRUE
            }, error = function(e) FALSE)

            if (test_read) {
              download_success <- TRUE
              break
            }
          }
        }
        FALSE
      }, error = function(e) FALSE)

      if (result) {
        download_success <- TRUE
        break
      }
    }
  }

  if (!download_success) {
    message(paste("  Could not download", level, "data for", end_year))
    unlink(tname)
    return(create_empty_assessment_raw())
  }

  # Read the Excel file
  # MDE files have multiple sheets - we want the data sheet for the specified level
  df <- tryCatch({
    # First, get sheet names
    sheets <- readxl::excel_sheets(tname)

    # Find the appropriate sheet
    # Common patterns: "State", "District", "School", or the level name
    level_cap <- switch(level,
                        "state" = "State",
                        "district" = "District",
                        "school" = "School",
                        level)

    # Look for exact match first, then partial
    sheet_idx <- which(tolower(sheets) == tolower(level_cap))
    if (length(sheet_idx) == 0) {
      sheet_idx <- grep(level_cap, sheets, ignore.case = TRUE)
    }
    if (length(sheet_idx) == 0) {
      # Default to first data sheet (skip info sheets)
      info_sheets <- grep("info|column|definition", sheets, ignore.case = TRUE)
      data_sheets <- setdiff(seq_along(sheets), info_sheets)
      sheet_idx <- data_sheets[1]
    }

    if (length(sheet_idx) > 0) {
      readxl::read_excel(tname, sheet = sheets[sheet_idx[1]], col_types = "text")
    } else {
      readxl::read_excel(tname, col_types = "text")
    }
  }, error = function(e) {
    message(paste("  Error reading Excel file:", e$message))
    create_empty_assessment_raw()
  })

  # Clean up temp file
  unlink(tname)

  # Add metadata
  if (nrow(df) > 0) {
    df$end_year <- end_year
    df$source_level <- level
  }

  df
}


#' Download assessment via MDE portal
#'
#' Downloads assessment data using the MDEAnalytics portal interface.
#' This mimics the web interface download functionality.
#'
#' @param end_year School year end
#' @param level Data level
#' @param dest_file Destination file path
#' @return TRUE if successful, FALSE otherwise
#' @keywords internal
download_assessment_via_portal <- function(end_year, level, dest_file) {

  # MDE uses WebFOCUS for data delivery

  # The portal requires navigating through a series of forms
  # We'll try to access the direct download endpoint

  # Construct the download URL using the WebFOCUS servlet
  level_param <- switch(
    level,
    "state" = "State",
    "district" = "District",
    "school" = "School",
    "State"
  )

  # WebFOCUS download servlet endpoint
  # These parameters are based on observed patterns from the MDE portal
  servlet_url <- "https://pub.education.mn.gov/ibi_apps/WFServlet"

  # Build POST/GET parameters for the download
  # The exact parameters depend on MDE's WebFOCUS configuration
  params <- list(
    IBIF_ex = "mdea_assessment_data_download",
    YEAR = as.character(end_year),
    LEVEL = level_param,
    FORMAT = "XLSX",
    IBIAPP_app = "mdea",
    IBI_NavigatorUser = "public"
  )

  # Try GET request first
  result <- tryCatch({
    url_with_params <- paste0(
      servlet_url, "?",
      paste(names(params), params, sep = "=", collapse = "&")
    )

    response <- httr::GET(
      url_with_params,
      httr::write_disk(dest_file, overwrite = TRUE),
      httr::timeout(300),
      httr::user_agent("mnschooldata R package"),
      httr::config(ssl_verifypeer = 0L)
    )

    if (!httr::http_error(response)) {
      file_info <- file.info(dest_file)
      if (!is.na(file_info$size) && file_info$size > 1000) {
        # Verify it's a valid Excel file
        test <- tryCatch({
          readxl::read_excel(dest_file, n_max = 1)
          TRUE
        }, error = function(e) FALSE)

        return(test)
      }
    }
    FALSE
  }, error = function(e) FALSE)

  if (result) return(TRUE)

  # Try POST request
  tryCatch({
    response <- httr::POST(
      servlet_url,
      body = params,
      encode = "form",
      httr::write_disk(dest_file, overwrite = TRUE),
      httr::timeout(300),
      httr::user_agent("mnschooldata R package"),
      httr::config(ssl_verifypeer = 0L)
    )

    if (!httr::http_error(response)) {
      file_info <- file.info(dest_file)
      if (!is.na(file_info$size) && file_info$size > 1000) {
        test <- tryCatch({
          readxl::read_excel(dest_file, n_max = 1)
          TRUE
        }, error = function(e) FALSE)

        return(test)
      }
    }
    FALSE
  }, error = function(e) FALSE)
}


#' Create empty assessment raw data frame
#'
#' Returns an empty data frame with expected column structure.
#'
#' @return Empty data frame with assessment columns
#' @keywords internal
create_empty_assessment_raw <- function() {
  data.frame(
    district_number = character(0),
    district_name = character(0),
    school_number = character(0),
    school_name = character(0),
    test_name = character(0),
    subject = character(0),
    grade = character(0),
    group_category = character(0),
    student_group = character(0),
    count_tested = character(0),
    count_level_d = character(0),  # Does Not Meet
    count_level_p = character(0),  # Partially Meets
    count_level_m = character(0),  # Meets
    count_level_e = character(0),  # Exceeds
    pct_level_d = character(0),
    pct_level_p = character(0),
    pct_level_m = character(0),
    pct_level_e = character(0),
    end_year = integer(0),
    source_level = character(0),
    stringsAsFactors = FALSE
  )
}


#' Get MDE assessment column mappings
#'
#' Returns mappings from MDE column names to standardized names.
#' MDE uses different column naming conventions across years.
#'
#' @param end_year School year end
#' @return Named list of column mappings
#' @keywords internal
get_assessment_column_map <- function(end_year) {

  list(
    # Identifiers
    district_id = c("DistrictNumber", "District Number", "districtNumber",
                    "DISTRICT_NUMBER", "District_Number", "distNum", "dist_num"),
    school_id = c("SchoolNumber", "School Number", "schoolNumber",
                  "SCHOOL_NUMBER", "School_Number", "schNum", "sch_num"),
    district_name = c("DistrictName", "District Name", "districtName",
                      "DISTRICT_NAME", "District_Name"),
    school_name = c("SchoolName", "School Name", "schoolName",
                    "SCHOOL_NAME", "School_Name"),

    # Test metadata
    test_name = c("TestName", "Test Name", "test_name", "TEST_NAME", "Test"),
    subject = c("Subject", "SUBJECT", "ContentArea", "Content Area",
                "content_area"),
    grade = c("Grade", "GRADE", "GradeLevel", "Grade Level", "grade_level"),

    # Student groups
    group_category = c("GroupCategory", "Group Category", "group_category",
                       "GROUP_CATEGORY", "Category"),
    student_group = c("StudentGroup", "Student Group", "student_group",
                      "STUDENT_GROUP", "Group", "Subgroup"),

    # Counts
    count_tested = c("CountTested", "Count Tested", "count_tested",
                     "COUNT_TESTED", "NumTested", "N Tested", "n_tested",
                     "TotalTested", "Total Tested"),
    count_level_d = c("CountLevelD", "Count Level D", "count_d",
                      "COUNT_D", "Does Not Meet Count", "DoesNotMeetCount",
                      "DNM Count", "CountBeginning", "Count Beginning"),
    count_level_p = c("CountLevelP", "Count Level P", "count_p",
                      "COUNT_P", "Partially Meets Count", "PartiallyMeetsCount",
                      "PM Count", "CountIntermediate", "Count Intermediate"),
    count_level_m = c("CountLevelM", "Count Level M", "count_m",
                      "COUNT_M", "Meets Count", "MeetsCount",
                      "M Count", "CountMeets", "Count Meets"),
    count_level_e = c("CountLevelE", "Count Level E", "count_e",
                      "COUNT_E", "Exceeds Count", "ExceedsCount",
                      "E Count", "CountAdvanced", "Count Advanced"),

    # Percentages
    pct_level_d = c("PercentLevelD", "Percent Level D", "pct_d",
                    "PCT_D", "Does Not Meet Percent", "DoesNotMeetPercent",
                    "DNM Percent", "PctBeginning", "Pct Beginning", "%D"),
    pct_level_p = c("PercentLevelP", "Percent Level P", "pct_p",
                    "PCT_P", "Partially Meets Percent", "PartiallyMeetsPercent",
                    "PM Percent", "PctIntermediate", "Pct Intermediate", "%P"),
    pct_level_m = c("PercentLevelM", "Percent Level M", "pct_m",
                    "PCT_M", "Meets Percent", "MeetsPercent",
                    "M Percent", "PctMeets", "Pct Meets", "%M"),
    pct_level_e = c("PercentLevelE", "Percent Level E", "pct_e",
                    "PCT_E", "Exceeds Percent", "ExceedsPercent",
                    "E Percent", "PctAdvanced", "Pct Advanced", "%E"),

    # Combined proficient (Meets + Exceeds)
    pct_proficient = c("PercentProficient", "Percent Proficient", "pct_proficient",
                       "PCT_PROFICIENT", "Proficient Percent", "ProficientPercent",
                       "Meets or Exceeds Percent", "M+E Percent", "%Proficient",
                       "PctMeetsExceeds", "Pct Meets Exceeds")
  )
}
