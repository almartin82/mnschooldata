# ==============================================================================
# Assessment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw MDE assessment data into a
# clean, standardized format.
#
# ==============================================================================


#' Process raw MDE assessment data
#'
#' Transforms raw MDE assessment data into a standardized schema combining
#' state, district, and school data.
#'
#' @param raw_data List containing state, district, and/or school data frames
#'   from get_raw_assessment
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_assessment <- function(raw_data, end_year) {

  result_list <- list()

  # Process each level if present
  if ("state" %in% names(raw_data) && nrow(raw_data$state) > 0) {
    result_list$state <- process_assessment_level(raw_data$state, end_year, "State")
  }

  if ("district" %in% names(raw_data) && nrow(raw_data$district) > 0) {
    result_list$district <- process_assessment_level(raw_data$district, end_year, "District")
  }

  if ("school" %in% names(raw_data) && nrow(raw_data$school) > 0) {
    result_list$school <- process_assessment_level(raw_data$school, end_year, "School")
  }

  # Combine all levels
  if (length(result_list) == 0) {
    return(create_empty_assessment_result(end_year))
  }

  dplyr::bind_rows(result_list)
}


#' Process a single level of assessment data
#'
#' @param df Raw data frame for one level (state/district/school)
#' @param end_year School year end
#' @param type Record type ("State", "District", "School")
#' @return Processed data frame
#' @keywords internal
process_assessment_level <- function(df, end_year, type) {

  if (nrow(df) == 0) {
    return(create_empty_assessment_result(end_year))
  }

  cols <- names(df)
  n_rows <- nrow(df)

  # Get column mappings
  col_map <- get_assessment_column_map(end_year)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    # Try partial match if exact match fails
    for (pattern in patterns) {
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep(type, n_rows),
    stringsAsFactors = FALSE
  )

  # District ID
  district_col <- find_col(col_map$district_id)
  if (!is.null(district_col)) {
    district_vals <- trimws(as.character(df[[district_col]]))
    # Handle state-level records (usually 0 or blank)
    result$district_id <- ifelse(
      district_vals == "0" | district_vals == "" | is.na(district_vals) | district_vals == "9999",
      NA_character_,
      sprintf("%04d", as.integer(district_vals))
    )
  } else {
    result$district_id <- rep(NA_character_, n_rows)
  }

  # District name
  district_name_col <- find_col(col_map$district_name)
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(as.character(df[[district_name_col]]))
  } else {
    result$district_name <- rep(NA_character_, n_rows)
  }

  # School ID (only for school level)
  school_col <- find_col(col_map$school_id)
  if (!is.null(school_col)) {
    school_vals <- trimws(as.character(df[[school_col]]))
    result$school_id <- ifelse(
      school_vals == "0" | school_vals == "" | is.na(school_vals) | school_vals == "9999",
      NA_character_,
      sprintf("%04d", as.integer(school_vals))
    )
  } else {
    result$school_id <- rep(NA_character_, n_rows)
  }

  # School name
  school_name_col <- find_col(col_map$school_name)
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(as.character(df[[school_name_col]]))
  } else {
    result$school_name <- rep(NA_character_, n_rows)
  }

  # Test name
  test_col <- find_col(col_map$test_name)
  if (!is.null(test_col)) {
    result$test <- standardize_test_name(df[[test_col]])
  } else {
    result$test <- rep("MCA", n_rows)
  }

  # Subject
  subject_col <- find_col(col_map$subject)
  if (!is.null(subject_col)) {
    result$subject <- standardize_subject(df[[subject_col]])
  } else {
    result$subject <- rep(NA_character_, n_rows)
  }

  # Grade
  grade_col <- find_col(col_map$grade)
  if (!is.null(grade_col)) {
    result$grade <- standardize_grade(df[[grade_col]])
  } else {
    result$grade <- rep(NA_character_, n_rows)
  }

  # Subgroup - combine group category and student group
  group_cat_col <- find_col(col_map$group_category)
  student_grp_col <- find_col(col_map$student_group)

  if (!is.null(student_grp_col)) {
    result$subgroup <- standardize_subgroup(df[[student_grp_col]])
  } else if (!is.null(group_cat_col)) {
    result$subgroup <- standardize_subgroup(df[[group_cat_col]])
  } else {
    result$subgroup <- rep("All Students", n_rows)
  }

  # Number tested
  n_tested_col <- find_col(col_map$count_tested)
  if (!is.null(n_tested_col)) {
    result$n_tested <- safe_numeric(df[[n_tested_col]])
  } else {
    result$n_tested <- rep(NA_integer_, n_rows)
  }

  # Proficiency counts
  count_d_col <- find_col(col_map$count_level_d)
  count_p_col <- find_col(col_map$count_level_p)
  count_m_col <- find_col(col_map$count_level_m)
  count_e_col <- find_col(col_map$count_level_e)

  result$n_does_not_meet <- if (!is.null(count_d_col)) safe_numeric(df[[count_d_col]]) else rep(NA_integer_, n_rows)
  result$n_partially_meets <- if (!is.null(count_p_col)) safe_numeric(df[[count_p_col]]) else rep(NA_integer_, n_rows)
  result$n_meets <- if (!is.null(count_m_col)) safe_numeric(df[[count_m_col]]) else rep(NA_integer_, n_rows)
  result$n_exceeds <- if (!is.null(count_e_col)) safe_numeric(df[[count_e_col]]) else rep(NA_integer_, n_rows)

  # Proficiency percentages
  pct_d_col <- find_col(col_map$pct_level_d)
  pct_p_col <- find_col(col_map$pct_level_p)
  pct_m_col <- find_col(col_map$pct_level_m)
  pct_e_col <- find_col(col_map$pct_level_e)

  result$pct_does_not_meet <- if (!is.null(pct_d_col)) safe_numeric(df[[pct_d_col]]) else rep(NA_real_, n_rows)
  result$pct_partially_meets <- if (!is.null(pct_p_col)) safe_numeric(df[[pct_p_col]]) else rep(NA_real_, n_rows)
  result$pct_meets <- if (!is.null(pct_m_col)) safe_numeric(df[[pct_m_col]]) else rep(NA_real_, n_rows)
  result$pct_exceeds <- if (!is.null(pct_e_col)) safe_numeric(df[[pct_e_col]]) else rep(NA_real_, n_rows)

  # Combined proficient (Meets + Exceeds)
  pct_prof_col <- find_col(col_map$pct_proficient)
  if (!is.null(pct_prof_col)) {
    result$pct_proficient <- safe_numeric(df[[pct_prof_col]])
  } else {
    # Calculate from components if available
    result$pct_proficient <- ifelse(
      !is.na(result$pct_meets) & !is.na(result$pct_exceeds),
      result$pct_meets + result$pct_exceeds,
      NA_real_
    )
  }

  result
}


#' Standardize test names
#'
#' @param x Vector of test names
#' @return Standardized test names
#' @keywords internal
standardize_test_name <- function(x) {
  x <- toupper(trimws(as.character(x)))

  # MCA variants
  x <- gsub(".*MCA.*IV.*|.*MCA-IV.*|.*MCA 4.*", "MCA-IV", x)
  x <- gsub(".*MCA.*III.*|.*MCA-III.*|.*MCA 3.*", "MCA-III", x)
  x <- gsub(".*MCA.*II.*|.*MCA-II.*|.*MCA 2.*", "MCA-II", x)

  # Generic MCA (when version not specified)
  x <- gsub("^MCA$|^MINNESOTA COMPREHENSIVE ASSESSMENT.*", "MCA", x)

  # MTAS
  x <- gsub(".*MTAS.*|.*MINNESOTA TEST OF ACADEMIC SKILLS.*", "MTAS", x)

  # Alt MCA
  x <- gsub(".*ALT.*MCA.*|.*ALTERNATE.*MCA.*", "Alt MCA", x)

  # Combined labels
  x <- gsub(".*ALL.*ACADEMIC.*|.*ALL.*STANDARDS.*", "All Assessments", x)

  x
}


#' Standardize subject names
#'
#' @param x Vector of subject names
#' @return Standardized subject names
#' @keywords internal
standardize_subject <- function(x) {
  x <- toupper(trimws(as.character(x)))

  # Reading/ELA
  x <- gsub("^READING$|^RLA$|^ELA$|^ENGLISH.*LANGUAGE.*ARTS$", "Reading", x)

  # Math
  x <- gsub("^MATH$|^MATHEMATICS$|^MATH.*EMATICS$", "Math", x)

  # Science
  x <- gsub("^SCIENCE$|^SCI$", "Science", x)

  x
}


#' Standardize grade levels
#'
#' @param x Vector of grade values
#' @return Standardized grade levels
#' @keywords internal
standardize_grade <- function(x) {
  x <- toupper(trimws(as.character(x)))

  # Remove GRADE prefix
  x <- gsub("^GRADE\\s*", "", x)

  # Handle ordinal formats
  x <- gsub("^3RD$", "03", x)
  x <- gsub("^4TH$", "04", x)
  x <- gsub("^5TH$", "05", x)
  x <- gsub("^6TH$", "06", x)
  x <- gsub("^7TH$", "07", x)
  x <- gsub("^8TH$", "08", x)
  x <- gsub("^9TH$", "09", x)
  x <- gsub("^10TH$", "10", x)
  x <- gsub("^11TH$", "11", x)

  # Pad single digits
  x <- gsub("^([3-9])$", "0\\1", x)

  # HS indicators
  x <- gsub("^HS$|^HIGH.*SCHOOL$|^10$", "10", x)
  x <- gsub("^ALL.*GRADES$|^ALL$", "All", x)

  x
}


#' Standardize subgroup names
#'
#' @param x Vector of subgroup names
#' @return Standardized subgroup names
#' @keywords internal
standardize_subgroup <- function(x) {
  x <- trimws(as.character(x))

  # Mapping of common variations to standard names
  subgroup_map <- c(
    # All students
    "All Students" = "All Students",
    "ALL STUDENTS" = "All Students",
    "All" = "All Students",
    "ALL" = "All Students",
    "Total" = "All Students",
    "TOTAL" = "All Students",

    # Race/ethnicity
    "American Indian or Alaska Native" = "Native American",
    "AMERICAN INDIAN OR ALASKA NATIVE" = "Native American",
    "American Indian" = "Native American",
    "AMERICAN INDIAN" = "Native American",
    "Native American" = "Native American",

    "Asian" = "Asian",
    "ASIAN" = "Asian",

    "Black or African American" = "Black",
    "BLACK OR AFRICAN AMERICAN" = "Black",
    "Black" = "Black",
    "BLACK" = "Black",
    "African American" = "Black",

    "Hispanic or Latino" = "Hispanic",
    "HISPANIC OR LATINO" = "Hispanic",
    "Hispanic" = "Hispanic",
    "HISPANIC" = "Hispanic",
    "Latino" = "Hispanic",

    "Native Hawaiian or Other Pacific Islander" = "Pacific Islander",
    "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = "Pacific Islander",
    "Pacific Islander" = "Pacific Islander",
    "Hawaiian/Pacific Islander" = "Pacific Islander",

    "White" = "White",
    "WHITE" = "White",

    "Two or More Races" = "Multiracial",
    "TWO OR MORE RACES" = "Multiracial",
    "Multiracial" = "Multiracial",
    "Multiple Races" = "Multiracial",
    "Two or more races" = "Multiracial",

    # Gender
    "Female" = "Female",
    "FEMALE" = "Female",
    "Male" = "Male",
    "MALE" = "Male",

    # Special populations
    "Eligible for free or reduced-price lunch" = "Economically Disadvantaged",
    "ELIGIBLE FOR FREE OR REDUCED-PRICE LUNCH" = "Economically Disadvantaged",
    "Free/Reduced Lunch" = "Economically Disadvantaged",
    "FREE/REDUCED LUNCH" = "Economically Disadvantaged",
    "Free and Reduced Lunch" = "Economically Disadvantaged",
    "Economically Disadvantaged" = "Economically Disadvantaged",
    "ECONOMICALLY DISADVANTAGED" = "Economically Disadvantaged",
    "FRL" = "Economically Disadvantaged",

    "Not eligible for free or reduced-price lunch" = "Non-Economically Disadvantaged",
    "NOT ELIGIBLE FOR FREE OR REDUCED-PRICE LUNCH" = "Non-Economically Disadvantaged",
    "Non-Economically Disadvantaged" = "Non-Economically Disadvantaged",
    "Not FRL" = "Non-Economically Disadvantaged",

    "Students with disabilities" = "Students with Disabilities",
    "STUDENTS WITH DISABILITIES" = "Students with Disabilities",
    "Special Education" = "Students with Disabilities",
    "SPECIAL EDUCATION" = "Students with Disabilities",
    "SpEd" = "Students with Disabilities",
    "SPED" = "Students with Disabilities",
    "IEP" = "Students with Disabilities",

    "Students without disabilities" = "Non-SWD",
    "STUDENTS WITHOUT DISABILITIES" = "Non-SWD",
    "Non-Special Education" = "Non-SWD",

    "English learner" = "English Learners",
    "ENGLISH LEARNER" = "English Learners",
    "English Learners" = "English Learners",
    "ENGLISH LEARNERS" = "English Learners",
    "EL" = "English Learners",
    "ELL" = "English Learners",
    "LEP" = "English Learners",
    "Limited English Proficient" = "English Learners",

    "Not an English learner" = "Non-EL",
    "NOT AN ENGLISH LEARNER" = "Non-EL",
    "Non-English Learner" = "Non-EL",
    "Non-EL" = "Non-EL",

    # Homeless
    "Homeless" = "Homeless",
    "HOMELESS" = "Homeless",
    "Not Homeless" = "Non-Homeless"
  )

  # Apply mapping
  result <- subgroup_map[x]

  # For values not in the map, keep original
  result[is.na(result)] <- x[is.na(result)]

  # Remove names
  unname(result)
}


#' Create empty assessment result data frame
#'
#' @param end_year School year end
#' @return Empty data frame with expected columns
#' @keywords internal
create_empty_assessment_result <- function(end_year) {
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
    n_does_not_meet = integer(0),
    n_partially_meets = integer(0),
    n_meets = integer(0),
    n_exceeds = integer(0),
    pct_does_not_meet = numeric(0),
    pct_partially_meets = numeric(0),
    pct_meets = numeric(0),
    pct_exceeds = numeric(0),
    pct_proficient = numeric(0),
    stringsAsFactors = FALSE
  )
}
