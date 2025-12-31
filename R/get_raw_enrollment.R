# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from MDE.
#
# Minnesota Department of Education provides enrollment data through the
# MDEAnalytics portal at pub.education.mn.gov. Data comes from MARSS
# (Minnesota Automated Reporting Student System) and represents October 1
# enrollment counts.
#
# Format Eras:
# - Era 1 (2007-2016): Original MDEAnalytics format with older column names
# - Era 2 (2017-present): Updated MDEAnalytics format with standardized columns
#
# ==============================================================================

#' Download raw enrollment data from MDE
#'
#' Downloads enrollment data from Minnesota Department of Education's
#' MDEAnalytics portal.
#'
#' @param end_year School year end (2023-24 = 2024)
#' @return List with school and district data frames
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year
  available_years <- get_available_years()
  if (!end_year %in% available_years) {
    stop(paste0(
      "end_year must be between ", min(available_years), " and ", max(available_years),
      ". Available years: ", paste(range(available_years), collapse = "-")
    ))
  }

  message(paste("Downloading MDE enrollment data for", end_year, "..."))

  # Download school-level data
  message("  Downloading school-level data...")
  school_data <- download_mde_enrollment(end_year, level = "school")

  # Download district-level data
  message("  Downloading district-level data...")
  district_data <- download_mde_enrollment(end_year, level = "district")

  list(
    school = school_data,
    district = district_data
  )
}


#' Download MDE enrollment data file
#'
#' Downloads enrollment data from the MDEAnalytics portal.
#' MDE provides Excel files for download that can be accessed via direct URLs.
#'
#' @param end_year School year end
#' @param level Data level: "school", "district", or "state"
#' @return Data frame with enrollment data
#' @keywords internal
download_mde_enrollment <- function(end_year, level = "school") {

  # Build URL for MDE data download
  # MDE uses a file naming pattern based on year and level
  # Files are Excel format (.xlsx)

  # Construct the school year string (e.g., "2023-24" for end_year 2024)
  start_year <- end_year - 1
  year_str <- paste0(start_year, "-", substr(end_year, 3, 4))

  # MDE URL patterns discovered through research:
  # School/District level enrollment files are available through the MDEAnalytics portal
  # The files follow patterns like:
  # https://pub.education.mn.gov/MDEAnalytics/DataDownload/Enrollment_{Year}_{Level}.xlsx

  # Primary URL pattern for enrollment data
  base_url <- "https://pub.education.mn.gov/MDEAnalytics/DataDownload"

  # Build filename based on level and year
  level_code <- switch(
    level,
    "school" = "School",
    "district" = "District",
    "state" = "State",
    "School"
  )

  # Try multiple URL patterns as MDE changes formats occasionally
  url_patterns <- c(
    # Pattern 1: Standard format
    paste0(base_url, "/Enrollment_", end_year, "_", level_code, ".xlsx"),
    # Pattern 2: With school year string
    paste0(base_url, "/Enrollment_", year_str, "_", level_code, ".xlsx"),
    # Pattern 3: Underscored year
    paste0(base_url, "/Enrollment_", start_year, "_", substr(end_year, 3, 4), "_", level_code, ".xlsx"),
    # Pattern 4: Simple year suffix
    paste0(base_url, "/", level_code, "Enrollment", end_year, ".xlsx")
  )

  # Create temp file
  tname <- tempfile(
    pattern = paste0("mde_enr_", level, "_"),
    tmpdir = tempdir(),
    fileext = ".xlsx"
  )

  # Try each URL pattern
  download_success <- FALSE

  for (url in url_patterns) {
    tryCatch({
      response <- httr::GET(
        url,
        httr::write_disk(tname, overwrite = TRUE),
        httr::timeout(300),
        httr::user_agent("mnschooldata R package")
      )

      # Check if download was successful
      if (!httr::http_error(response)) {
        # Verify file is actually an Excel file (not an error page)
        file_info <- file.info(tname)
        if (file_info$size > 1000) {
          # Try to read the file to verify it's valid Excel
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
    }, error = function(e) {
      # Continue to next URL pattern
    })
  }

  # If direct download failed, try the MDEAnalytics CGI endpoint
  if (!download_success) {
    download_success <- download_mde_via_cgi(end_year, level, tname)
  }

  if (!download_success) {
    stop(paste(
      "Failed to download", level, "enrollment data for year", end_year, "from MDE.",
      "\nThe Minnesota Department of Education may have changed their data portal.",
      "\nPlease check https://education.mn.gov/MDE/Data/ for current data access."
    ))
  }

  # Read the Excel file
  df <- readxl::read_excel(
    tname,
    col_types = "text"  # Read all as text initially for consistent processing
  )

  # Clean up temp file
  unlink(tname)

  # Add end_year column
  df$end_year <- end_year

  df
}


#' Download MDE data via CGI endpoint
#'
#' Alternative download method using the MDEAnalytics CGI interface.
#' This mimics the web interface's download functionality.
#'
#' @param end_year School year end
#' @param level Data level
#' @param dest_file Destination file path
#' @return TRUE if successful, FALSE otherwise
#' @keywords internal
download_mde_via_cgi <- function(end_year, level, dest_file) {

  # MDEAnalytics uses a JSP/CGI interface for data downloads
  # The interface accepts parameters for category, year, and level

  # Construct the download URL
  # Based on research, the download endpoint uses these parameters:
  # - Category: "Enrollment"
  # - Year: The school year (e.g., 2024)
  # - Level: "School", "District", "State", or "County"

  level_param <- switch(
    level,
    "school" = "School",
    "district" = "District",
    "state" = "State",
    "School"
  )

  # The MDEAnalytics download servlet
  cgi_url <- "https://pub.education.mn.gov/MDEAnalytics/DataDownload.do"

  # Build POST parameters
  post_params <- list(
    category = "Enrollment",
    year = as.character(end_year),
    level = level_param,
    format = "xlsx"
  )

  tryCatch({
    response <- httr::POST(
      cgi_url,
      body = post_params,
      encode = "form",
      httr::write_disk(dest_file, overwrite = TRUE),
      httr::timeout(300),
      httr::user_agent("mnschooldata R package")
    )

    if (!httr::http_error(response)) {
      file_info <- file.info(dest_file)
      if (file_info$size > 1000) {
        # Verify it's a valid Excel file
        test_read <- tryCatch({
          readxl::read_excel(dest_file, n_max = 1)
          TRUE
        }, error = function(e) FALSE)

        return(test_read)
      }
    }

    FALSE
  }, error = function(e) {
    FALSE
  })
}


#' Get column mappings for MDE enrollment data
#'
#' Returns mappings from MDE column names to standardized names.
#' MDE uses different column naming conventions across years.
#'
#' @param end_year School year end
#' @return Named list of column mappings
#' @keywords internal
get_mde_column_map <- function(end_year) {

  # MDE column naming patterns:
  # - District/School identifiers use "DistrictNumber", "SchoolNumber", etc.
  # - Demographic columns often use full names like "American Indian"
  # - Special populations: "Free/Reduced Price Lunch", "LEP", "Special Education"

  list(
    # Identifiers
    district_id = c("DistrictNumber", "District Number", "District_Number", "districtNumber",
                    "DISTRICT_NUMBER", "Dist_Num", "distNum"),
    school_id = c("SchoolNumber", "School Number", "School_Number", "schoolNumber",
                  "SCHOOL_NUMBER", "Sch_Num", "schNum"),
    district_name = c("DistrictName", "District Name", "District_Name", "districtName",
                      "DISTRICT_NAME", "Dist_Name"),
    school_name = c("SchoolName", "School Name", "School_Name", "schoolName",
                    "SCHOOL_NAME", "Sch_Name"),

    # Location
    county = c("CountyName", "County Name", "County", "COUNTY"),

    # Total enrollment
    total = c("Total", "TotalEnrollment", "Total Enrollment", "TOTAL",
              "Total_Enrollment", "Enrollment", "K12Enrollment"),

    # Demographics - Race/Ethnicity
    white = c("White", "WHITE", "White Students", "WhiteStudents"),
    black = c("Black", "BLACK", "Black or African American",
              "African American", "BlackStudents"),
    hispanic = c("Hispanic", "HISPANIC", "Hispanic/Latino",
                 "Hispanic or Latino", "HispanicStudents"),
    asian = c("Asian", "ASIAN", "AsianStudents"),
    native_american = c("American Indian", "AMERICAN_INDIAN",
                        "American Indian or Alaska Native",
                        "American Indian/Alaska Native",
                        "NativeAmericanStudents"),
    pacific_islander = c("Pacific Islander", "PACIFIC_ISLANDER",
                         "Native Hawaiian or Other Pacific Islander",
                         "Native Hawaiian/Pacific Islander",
                         "PacificIslanderStudents"),
    multiracial = c("Two or More Races", "TWO_OR_MORE",
                    "Two or More", "Multiracial",
                    "MultiracialStudents"),

    # Gender
    male = c("Male", "MALE", "MaleStudents", "Boys"),
    female = c("Female", "FEMALE", "FemaleStudents", "Girls"),

    # Special populations
    econ_disadv = c("Free/Reduced Price Lunch", "Free and Reduced Lunch",
                    "FreeReducedLunch", "FRPL", "Free/Reduced",
                    "Economically Disadvantaged", "EconDisadv"),
    lep = c("LEP", "English Learner", "English Learners", "EL",
            "Limited English Proficient", "ELL"),
    special_ed = c("Special Education", "SPED", "SpecialEducation",
                   "Students with Disabilities", "IEP"),
    homeless = c("Homeless", "HOMELESS", "HomelessStudents"),

    # Grade levels
    grade_pk = c("Pre-Kindergarten", "PreK", "PK", "Pre-K", "Prekindergarten"),
    grade_k = c("Kindergarten", "K", "KG", "KNDG"),
    grade_01 = c("Grade 1", "Grade1", "Gr1", "G1", "1"),
    grade_02 = c("Grade 2", "Grade2", "Gr2", "G2", "2"),
    grade_03 = c("Grade 3", "Grade3", "Gr3", "G3", "3"),
    grade_04 = c("Grade 4", "Grade4", "Gr4", "G4", "4"),
    grade_05 = c("Grade 5", "Grade5", "Gr5", "G5", "5"),
    grade_06 = c("Grade 6", "Grade6", "Gr6", "G6", "6"),
    grade_07 = c("Grade 7", "Grade7", "Gr7", "G7", "7"),
    grade_08 = c("Grade 8", "Grade8", "Gr8", "G8", "8"),
    grade_09 = c("Grade 9", "Grade9", "Gr9", "G9", "9"),
    grade_10 = c("Grade 10", "Grade10", "Gr10", "G10", "10"),
    grade_11 = c("Grade 11", "Grade11", "Gr11", "G11", "11"),
    grade_12 = c("Grade 12", "Grade12", "Gr12", "G12", "12")
  )
}
