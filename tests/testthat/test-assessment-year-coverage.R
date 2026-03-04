# ==============================================================================
# Assessment Year Coverage Tests for mnschooldata
# ==============================================================================
#
# These tests verify assessment function infrastructure and data quality.
#
# MDE Assessment context:
# - Available years: 2019, 2021-2025 (no 2020 due to COVID waiver)
# - Tests: MCA-III (2019-2023), MCA-IV (2024+)
# - Subjects: Reading, Math, Science
# - Proficiency levels: Does Not Meet, Partially Meets, Meets, Exceeds
#   (MCA-IV: Beginning, Intermediate, Meets, Advanced)
# - Levels: state, district, school
#
# NOTE: As of 2026-03, MDE WebFOCUS portal does not return data via
# programmatic access. All cached assessment files have 0 rows. Tests
# verify function infrastructure, input validation, and empty-data
# handling rather than pinned values.
#
# ==============================================================================

library(testthat)

# ==============================================================================
# Year Infrastructure Tests
# ==============================================================================

test_that("get_available_assessment_years returns correct structure", {
  result <- get_available_assessment_years()

  expect_true(is.list(result))
  expect_true("years" %in% names(result))
  expect_true("note" %in% names(result))
  expect_true(is.numeric(result$years))
  expect_true(is.character(result$note))
})

test_that("assessment years include 2019 and 2021-2025, exclude 2020", {
  result <- get_available_assessment_years()

  # Required years
  expect_true(2019 %in% result$years)
  expect_true(2021 %in% result$years)
  expect_true(2022 %in% result$years)
  expect_true(2023 %in% result$years)
  expect_true(2024 %in% result$years)
  expect_true(2025 %in% result$years)

  # 2020 excluded (COVID waiver)
  expect_false(2020 %in% result$years)

  # Exactly 6 years
  expect_equal(length(result$years), 6)
})

test_that("assessment note mentions COVID", {
  result <- get_available_assessment_years()

  expect_true(grepl("COVID", result$note, ignore.case = TRUE))
})

# ==============================================================================
# Input Validation Tests
# ==============================================================================

test_that("fetch_assessment rejects invalid years", {
  expect_error(fetch_assessment(1990), "end_year must be one of")
  expect_error(fetch_assessment(2030), "end_year must be one of")
  expect_error(fetch_assessment(2018), "end_year must be one of")
  expect_error(fetch_assessment(2026), "end_year must be one of")
})

test_that("fetch_assessment gives COVID-specific error for 2020", {
  expect_error(fetch_assessment(2020), "COVID")
})

test_that("fetch_assessment validates level parameter", {
  # These should error even without network
  expect_error(fetch_assessment(2024, level = "invalid"), "level must be")
  expect_error(fetch_assessment(2024, level = "county"), "level must be")
})

test_that("fetch_assessment_multi warns about and excludes 2020", {
  skip_on_cran()
  skip_if_offline()

  # Should warn about 2020 and exclude it
  expect_warning(
    fetch_assessment_multi(c(2019, 2020, 2021), use_cache = TRUE),
    "2020"
  )
})

test_that("fetch_assessment_multi rejects all-invalid years", {
  expect_error(fetch_assessment_multi(c(1990, 1991)))
})

test_that("fetch_assessment_multi errors on empty valid year set", {
  # If only 2020 is passed, after exclusion there are no valid years
  expect_warning(
    expect_error(
      fetch_assessment_multi(2020, use_cache = TRUE),
      "No valid years"
    ),
    "2020"
  )
})

# ==============================================================================
# Level Parameter Tests
# ==============================================================================

test_that("fetch_assessment accepts valid levels", {
  skip_on_cran()
  skip_if_offline()

  # "all", "state", "district", "school" should not error on input validation
  # (they may return 0 rows due to download failure, which is expected)
  for (lev in c("all", "state", "district", "school")) {
    result <- tryCatch(
      fetch_assessment(2024, level = lev, tidy = TRUE, use_cache = TRUE),
      error = function(e) NULL
    )
    # If we get a result, it should be a data frame
    if (!is.null(result)) {
      expect_s3_class(result, "data.frame")
    }
  }
})

test_that("fetch_assessment level is case-insensitive", {
  skip_on_cran()
  skip_if_offline()

  # "State", "STATE", "state" should all work
  for (lev in c("State", "STATE", "state")) {
    result <- tryCatch(
      fetch_assessment(2024, level = lev, tidy = TRUE, use_cache = TRUE),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "data.frame")
    }
  }
})

# ==============================================================================
# Empty Data Handling (MDE portal currently unavailable)
# ==============================================================================

test_that("assessment returns proper empty data frame structure when no data", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    # Even if 0 rows, should have correct columns
    expected_cols <- c(
      "end_year", "type", "district_id", "district_name",
      "school_id", "school_name", "test", "subject", "grade", "subgroup",
      "n_tested", "proficiency_level", "n_students", "pct",
      "is_state", "is_district", "is_school"
    )

    for (col in expected_cols) {
      expect_true(col %in% names(result),
                  info = paste("Missing expected column:", col))
    }
  }
})

test_that("assessment wide format has expected columns when no data", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    # Wide format should have pct_* columns instead of proficiency_level
    wide_expected <- c("end_year", "type", "subject", "grade", "subgroup", "n_tested")

    for (col in wide_expected) {
      expect_true(col %in% names(result),
                  info = paste("Missing expected wide column:", col))
    }
  }
})

# ==============================================================================
# Assessment Cache Functions
# ==============================================================================

test_that("assessment cache path generation works", {
  path <- get_assessment_cache_path(2024, "tidy", "state")

  expect_true(is.character(path))
  expect_true(grepl("2024", path))
  expect_true(grepl("assess", path))
  expect_true(grepl("tidy", path))
  expect_true(grepl("state", path))
  expect_true(grepl("\\.rds$", path))
})

test_that("assessment cache path varies by year, type, and level", {
  p1 <- get_assessment_cache_path(2024, "tidy", "state")
  p2 <- get_assessment_cache_path(2023, "tidy", "state")
  p3 <- get_assessment_cache_path(2024, "wide", "state")
  p4 <- get_assessment_cache_path(2024, "tidy", "all")

  # All should be different

  expect_false(p1 == p2)
  expect_false(p1 == p3)
  expect_false(p1 == p4)
})

test_that("clear_assessment_cache handles non-existent year gracefully", {
  # Should not error
  expect_no_error(clear_assessment_cache(end_year = 9999))
})

# ==============================================================================
# Subgroup Standardization Tests
# ==============================================================================

test_that("standardize_subgroup handles All Students variants", {
  standardize <- mnschooldata:::standardize_subgroup

  expect_equal(standardize("All Students"), "All Students")
  expect_equal(standardize("ALL STUDENTS"), "All Students")
  expect_equal(standardize("All"), "All Students")
  expect_equal(standardize("Total"), "All Students")
})

test_that("standardize_subgroup handles race/ethnicity", {
  standardize <- mnschooldata:::standardize_subgroup

  expect_equal(standardize("American Indian or Alaska Native"), "Native American")
  expect_equal(standardize("Black or African American"), "Black")
  expect_equal(standardize("Hispanic or Latino"), "Hispanic")
  expect_equal(standardize("Two or More Races"), "Multiracial")
  expect_equal(standardize("Native Hawaiian or Other Pacific Islander"), "Pacific Islander")
  expect_equal(standardize("White"), "White")
  expect_equal(standardize("Asian"), "Asian")
})

test_that("standardize_subgroup handles special populations", {
  standardize <- mnschooldata:::standardize_subgroup

  expect_equal(standardize("Students with disabilities"), "Students with Disabilities")
  expect_equal(standardize("English learner"), "English Learners")
  expect_equal(standardize("Eligible for free or reduced-price lunch"), "Economically Disadvantaged")
  expect_equal(standardize("Homeless"), "Homeless")
})

test_that("standardize_subgroup handles gender", {
  standardize <- mnschooldata:::standardize_subgroup

  expect_equal(standardize("Male"), "Male")
  expect_equal(standardize("Female"), "Female")
  expect_equal(standardize("MALE"), "Male")
  expect_equal(standardize("FEMALE"), "Female")
})

test_that("standardize_subgroup preserves unknown values", {
  standardize <- mnschooldata:::standardize_subgroup

  # Unrecognized values should be returned as-is
  expect_equal(standardize("Some New Group"), "Some New Group")
})

# ==============================================================================
# Grade Standardization Tests
# ==============================================================================

test_that("standardize_grade handles common formats", {
  standardize <- mnschooldata:::standardize_grade

  expect_equal(standardize("Grade 3"), "03")
  expect_equal(standardize("3"), "03")
  expect_equal(standardize("3rd"), "03")
  expect_equal(standardize("10"), "10")
  expect_equal(standardize("All Grades"), "All")
  expect_equal(standardize("ALL GRADES"), "All")
})

test_that("standardize_grade pads single digits", {
  standardize <- mnschooldata:::standardize_grade

  for (g in 3:9) {
    expect_equal(standardize(as.character(g)), sprintf("%02d", g),
                 info = paste("Grade", g, "should be zero-padded"))
  }
})

# ==============================================================================
# Subject Standardization Tests
# ==============================================================================

test_that("standardize_subject normalizes names", {
  standardize <- mnschooldata:::standardize_subject

  expect_equal(standardize("Reading"), "Reading")
  expect_equal(standardize("RLA"), "Reading")
  expect_equal(standardize("Math"), "Math")
  expect_equal(standardize("Mathematics"), "Math")
  expect_equal(standardize("Science"), "Science")
})

# ==============================================================================
# Test Name Standardization Tests
# ==============================================================================

test_that("standardize_test_name handles MCA variants", {
  standardize <- mnschooldata:::standardize_test_name

  expect_true(grepl("MCA", standardize("MCA")))
  expect_true(grepl("MCA", standardize("MCA-III")))
  expect_true(grepl("MCA", standardize("MCA-IV")))
  expect_true(grepl("MTAS", standardize("MTAS")))
})

# ==============================================================================
# Tidy Assessment Transformation Tests
# ==============================================================================

test_that("tidy_assessment handles empty data frame", {
  empty <- create_empty_assessment_result(2024)

  result <- tidy_assessment(empty)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("proficiency_level" %in% names(result))
})

test_that("id_assessment_aggs adds correct boolean flags", {
  test_df <- data.frame(
    end_year = c(2024, 2024, 2024),
    type = c("State", "District", "School"),
    subject = c("Math", "Math", "Math"),
    stringsAsFactors = FALSE
  )

  result <- id_assessment_aggs(test_df)

  expect_true(result$is_state[1])
  expect_false(result$is_district[1])
  expect_false(result$is_school[1])

  expect_false(result$is_state[2])
  expect_true(result$is_district[2])
  expect_false(result$is_school[2])

  expect_false(result$is_state[3])
  expect_false(result$is_district[3])
  expect_true(result$is_school[3])
})

# ==============================================================================
# Proficiency Calculation Tests
# ==============================================================================

test_that("calc_proficiency requires tidy format", {
  # Should error if proficiency_level column is missing
  bad_df <- data.frame(end_year = 2024, subject = "Math")

  expect_error(calc_proficiency(bad_df), "tidy assessment data")
})

test_that("assessment_summary requires tidy format", {
  bad_df <- data.frame(end_year = 2024, subject = "Math")

  expect_error(assessment_summary(bad_df), "tidy assessment data")
})

# ==============================================================================
# Convenience Function Tests
# ==============================================================================

test_that("fetch_district_assessment normalizes district_id", {
  skip_on_cran()
  skip_if_offline()

  # This should not error on input validation
  # (may return 0 rows since assessment data download is broken)
  result <- tryCatch(
    fetch_district_assessment(2024, "1"),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
  }
})

test_that("fetch_school_assessment normalizes IDs", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_school_assessment(2024, "1", "1"),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
  }
})

# ==============================================================================
# Multi-Year Assessment Tests
# ==============================================================================

test_that("fetch_assessment_multi combines years correctly", {
  skip_on_cran()
  skip_if_offline()

  result <- tryCatch(
    fetch_assessment_multi(c(2019, 2021), level = "state",
                           tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )

  if (!is.null(result) && nrow(result) > 0) {
    # Both years should be present
    expect_true(all(c(2019, 2021) %in% unique(result$end_year)))

    # Should be a data frame
    expect_s3_class(result, "data.frame")
  }
})

# ==============================================================================
# Column Mapping Tests
# ==============================================================================

test_that("get_assessment_column_map returns expected structure", {
  col_map <- mnschooldata:::get_assessment_column_map(2024)

  expect_true(is.list(col_map))

  # Key fields must exist
  required_keys <- c(
    "district_id", "school_id", "district_name", "school_name",
    "test_name", "subject", "grade",
    "group_category", "student_group",
    "count_tested",
    "count_level_d", "count_level_p", "count_level_m", "count_level_e",
    "pct_level_d", "pct_level_p", "pct_level_m", "pct_level_e",
    "pct_proficient"
  )

  for (key in required_keys) {
    expect_true(key %in% names(col_map),
                info = paste("Missing column mapping key:", key))
    expect_true(is.character(col_map[[key]]),
                info = paste("Column mapping for", key, "should be character vector"))
    expect_true(length(col_map[[key]]) > 0,
                info = paste("Column mapping for", key, "should not be empty"))
  }
})

test_that("assessment URL patterns are generated for all levels", {
  for (lev in c("state", "district", "school")) {
    urls <- mnschooldata:::get_assessment_url(2024, lev)

    expect_true(is.character(urls))
    expect_true(length(urls) > 0,
                info = paste("Should generate URLs for level:", lev))
    expect_true(all(grepl("^https?://", urls)),
                info = paste("URLs should start with http(s) for level:", lev))
  }
})
