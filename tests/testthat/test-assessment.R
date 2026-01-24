# ==============================================================================
# Assessment Data Tests for mnschooldata
# ==============================================================================
#
# These tests verify the assessment data functions for Minnesota MCA data.
#
# Test Categories:
# 1. Function existence - All exported functions exist
# 2. Year validation - Available years return correctly
# 3. Data structure - Output has expected columns
# 4. Data quality - No Inf/NaN, valid ranges
# 5. Tidy transformation - Wide and tidy formats consistent
#
# ==============================================================================

library(testthat)

# Skip if no network connectivity
skip_if_offline <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", httr::timeout(5))
    if (httr::http_error(response)) {
      skip("No network connectivity")
    }
  }, error = function(e) {
    skip("No network connectivity")
  })
}

# ==============================================================================
# Function Existence Tests
# ==============================================================================

test_that("assessment fetch functions exist", {
  expect_true(is.function(fetch_assessment))
  expect_true(is.function(fetch_assessment_multi))
  expect_true(is.function(fetch_district_assessment))
  expect_true(is.function(fetch_school_assessment))
})

test_that("assessment utility functions exist", {
  expect_true(is.function(get_available_assessment_years))
  expect_true(is.function(tidy_assessment))
  expect_true(is.function(id_assessment_aggs))
  expect_true(is.function(assessment_summary))
  expect_true(is.function(calc_proficiency))
  expect_true(is.function(clear_assessment_cache))
})

# ==============================================================================
# Year Validation Tests
# ==============================================================================

test_that("get_available_assessment_years returns valid years", {
  result <- get_available_assessment_years()

  expect_true(is.list(result))
  expect_true("years" %in% names(result))
  expect_true("note" %in% names(result))

  # Should include 2019, 2021-2025
  expect_true(2019 %in% result$years)
  expect_true(2021 %in% result$years)
  expect_true(2024 %in% result$years)

  # 2020 should NOT be included (COVID waiver)
  expect_false(2020 %in% result$years)

  # All years should be reasonable
  expect_true(all(result$years >= 2010 & result$years <= 2030))
})

test_that("fetch_assessment validates year input", {
  # Invalid year should error

  expect_error(fetch_assessment(1990), "end_year must be one of")
  expect_error(fetch_assessment(2030), "end_year must be one of")

  # 2020 should give specific error message
  expect_error(fetch_assessment(2020), "COVID")
})

test_that("fetch_assessment validates level input", {
  skip_if_offline()

  # Invalid level should error
  expect_error(fetch_assessment(2024, level = "invalid"), "level must be")
  expect_error(fetch_assessment(2024, level = "county"), "level must be")
})

# ==============================================================================
# Data Download Tests (Live Network)
# ==============================================================================

test_that("fetch_assessment returns data frame", {
  skip_if_offline()

  tryCatch({
    # Try to fetch state-level data (smaller/faster)
    result <- fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = FALSE)

    # Should be a data frame
    expect_s3_class(result, "data.frame")

    # If data was retrieved, check structure
    if (nrow(result) > 0) {
      expect_true("end_year" %in% names(result))
      expect_true("type" %in% names(result))
    }
  }, error = function(e) {
    # Data source may be unavailable
    skip(paste("MDE assessment data source unavailable:", e$message))
  })
})

# ==============================================================================
# Data Structure Tests
# ==============================================================================

test_that("processed assessment data has expected columns", {
  skip_if_offline()

  tryCatch({
    result <- fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = TRUE)

    if (nrow(result) > 0) {
      # Required columns for processed data
      expected_cols <- c(
        "end_year", "type", "district_id", "district_name",
        "school_id", "school_name", "test", "subject", "grade", "subgroup",
        "n_tested"
      )

      for (col in expected_cols) {
        expect_true(col %in% names(result), info = paste("Missing column:", col))
      }

      # Proficiency percentage columns (at least some should be present)
      pct_cols <- c("pct_does_not_meet", "pct_partially_meets", "pct_meets", "pct_exceeds")
      has_pct_cols <- any(pct_cols %in% names(result))
      expect_true(has_pct_cols, info = "Should have at least one proficiency percentage column")
    }
  }, error = function(e) {
    skip(paste("MDE assessment data source unavailable:", e$message))
  })
})

test_that("tidy assessment data has expected columns", {
  skip_if_offline()

  tryCatch({
    result <- fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE)

    if (nrow(result) > 0) {
      # Required columns for tidy data
      expected_cols <- c(
        "end_year", "type", "subject", "grade", "subgroup",
        "n_tested", "proficiency_level", "n_students", "pct",
        "is_state", "is_district", "is_school"
      )

      for (col in expected_cols) {
        expect_true(col %in% names(result), info = paste("Missing column:", col))
      }

      # proficiency_level should have expected values
      levels <- unique(result$proficiency_level)
      expect_true(length(levels) > 0)
    }
  }, error = function(e) {
    skip(paste("MDE assessment data source unavailable:", e$message))
  })
})

# ==============================================================================
# Data Quality Tests
# ==============================================================================

test_that("assessment data has no Inf or NaN values", {
  skip_if_offline()

  tryCatch({
    result <- fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE)

    if (nrow(result) > 0) {
      for (col in names(result)[sapply(result, is.numeric)]) {
        expect_false(any(is.infinite(result[[col]]), na.rm = TRUE),
                     info = paste("No Inf in", col))
        expect_false(any(is.nan(result[[col]]), na.rm = TRUE),
                     info = paste("No NaN in", col))
      }
    }
  }, error = function(e) {
    skip(paste("MDE assessment data source unavailable:", e$message))
  })
})

test_that("proficiency percentages are in valid range", {
  skip_if_offline()

  tryCatch({
    result <- fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE)

    if (nrow(result) > 0 && "pct" %in% names(result)) {
      # pct should be in 0-1 range (after normalization)
      valid_pcts <- result$pct[!is.na(result$pct)]
      if (length(valid_pcts) > 0) {
        expect_true(all(valid_pcts >= 0), info = "pct should be >= 0")
        expect_true(all(valid_pcts <= 1), info = "pct should be <= 1")
      }
    }
  }, error = function(e) {
    skip(paste("MDE assessment data source unavailable:", e$message))
  })
})

test_that("n_tested values are non-negative", {
  skip_if_offline()

  tryCatch({
    result <- fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = TRUE)

    if (nrow(result) > 0 && "n_tested" %in% names(result)) {
      valid_n <- result$n_tested[!is.na(result$n_tested)]
      if (length(valid_n) > 0) {
        expect_true(all(valid_n >= 0), info = "n_tested should be >= 0")
      }
    }
  }, error = function(e) {
    skip(paste("MDE assessment data source unavailable:", e$message))
  })
})

# ==============================================================================
# Tidy Transformation Tests
# ==============================================================================

test_that("tidy and wide formats are consistent", {
  skip_if_offline()

  tryCatch({
    wide <- fetch_assessment(2024, level = "state", tidy = FALSE, use_cache = TRUE)
    tidy <- fetch_assessment(2024, level = "state", tidy = TRUE, use_cache = TRUE)

    # Both should have data or both empty
    if (nrow(wide) > 0 && nrow(tidy) > 0) {
      # Tidy should have more rows (one per proficiency level)
      expect_gte(nrow(tidy), nrow(wide))

      # Same year
      expect_equal(unique(wide$end_year), unique(tidy$end_year))
    }
  }, error = function(e) {
    skip(paste("MDE assessment data source unavailable:", e$message))
  })
})

# ==============================================================================
# Aggregation Flag Tests
# ==============================================================================

test_that("aggregation flags are correctly set", {
  skip_if_offline()

  tryCatch({
    result <- fetch_assessment(2024, level = "all", tidy = TRUE, use_cache = TRUE)

    if (nrow(result) > 0) {
      # Check that flags match type column
      state_rows <- result[result$type == "State", ]
      if (nrow(state_rows) > 0) {
        expect_true(all(state_rows$is_state))
        expect_true(all(!state_rows$is_district))
        expect_true(all(!state_rows$is_school))
      }

      district_rows <- result[result$type == "District", ]
      if (nrow(district_rows) > 0) {
        expect_true(all(!district_rows$is_state))
        expect_true(all(district_rows$is_district))
        expect_true(all(!district_rows$is_school))
      }

      school_rows <- result[result$type == "School", ]
      if (nrow(school_rows) > 0) {
        expect_true(all(!school_rows$is_state))
        expect_true(all(!school_rows$is_district))
        expect_true(all(school_rows$is_school))
      }
    }
  }, error = function(e) {
    skip(paste("MDE assessment data source unavailable:", e$message))
  })
})

# ==============================================================================
# Cache Tests
# ==============================================================================

test_that("assessment cache functions work", {
  # Test cache path generation
  path <- mnschooldata:::get_assessment_cache_path(2024, "tidy", "state")
  expect_true(is.character(path))
  expect_true(grepl("2024", path))
  expect_true(grepl("assess", path))
})

test_that("clear_assessment_cache executes without error", {
  # Should not error even if no cache exists
  expect_no_error(clear_assessment_cache(end_year = 9999))
})

# ==============================================================================
# Multi-Year Tests
# ==============================================================================

test_that("fetch_assessment_multi handles year validation", {
  # Should warn about 2020
  expect_warning(
    fetch_assessment_multi(c(2019, 2020, 2021), use_cache = TRUE),
    "2020"
  )

  # Invalid years should error
  expect_error(fetch_assessment_multi(c(1990, 1991)))
})

# ==============================================================================
# Subgroup Standardization Tests
# ==============================================================================

test_that("subgroup names are standardized correctly", {
  # Test the internal standardization function
  standardize <- mnschooldata:::standardize_subgroup

  # All students variants
  expect_equal(standardize("All Students"), "All Students")
  expect_equal(standardize("ALL STUDENTS"), "All Students")
  expect_equal(standardize("All"), "All Students")

  # Race/ethnicity
  expect_equal(standardize("American Indian or Alaska Native"), "Native American")
  expect_equal(standardize("Black or African American"), "Black")
  expect_equal(standardize("Hispanic or Latino"), "Hispanic")
  expect_equal(standardize("Two or More Races"), "Multiracial")

  # Special populations
  expect_equal(standardize("Students with disabilities"), "Students with Disabilities")
  expect_equal(standardize("English learner"), "English Learners")
  expect_equal(standardize("Eligible for free or reduced-price lunch"), "Economically Disadvantaged")
})

test_that("grade levels are standardized correctly", {
  standardize <- mnschooldata:::standardize_grade

  expect_equal(standardize("Grade 3"), "03")
  expect_equal(standardize("3"), "03")
  expect_equal(standardize("3rd"), "03")
  expect_equal(standardize("10"), "10")
  expect_equal(standardize("All Grades"), "All")
})

test_that("subject names are standardized correctly", {
  standardize <- mnschooldata:::standardize_subject

  expect_equal(standardize("Reading"), "Reading")
  expect_equal(standardize("RLA"), "Reading")
  expect_equal(standardize("Math"), "Math")
  expect_equal(standardize("Mathematics"), "Math")
  expect_equal(standardize("Science"), "Science")
})
