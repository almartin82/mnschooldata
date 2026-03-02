# ==============================================================================
# Tests for fetch_directory() and related functions
# ==============================================================================
#
# These tests verify the directory data pipeline using LIVE network calls.
# No mocks - we verify actual connectivity and data correctness.
#
# Test Categories:
# 1. URL Availability - MDE-ORG endpoints respond
# 2. Raw Download - CSV files download and parse correctly
# 3. Column Structure - Expected columns exist in processed data
# 4. Data Quality - No empty data, valid values
# 5. fetch_directory() - Combined output is correct
# 6. fetch_district_directory() - District-only output is correct
# 7. Cache - Cache read/write works
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
# STEP 1: URL Availability
# ==============================================================================

test_that("MDE-ORG school directory URL is accessible", {
  skip_on_cran()
  skip_if_offline()

  url <- mnschooldata:::get_directory_url("school")
  response <- httr::HEAD(url, httr::timeout(30),
                         httr::user_agent("mnschooldata R package"))
  expect_false(httr::http_error(response))
})

test_that("MDE-ORG district directory URL is accessible", {
  skip_on_cran()
  skip_if_offline()

  url <- mnschooldata:::get_directory_url("district")
  response <- httr::HEAD(url, httr::timeout(30),
                         httr::user_agent("mnschooldata R package"))
  expect_false(httr::http_error(response))
})


# ==============================================================================
# STEP 2: Raw Download Tests
# ==============================================================================

test_that("get_raw_school_directory returns a data frame with rows", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_school_directory()
  expect_s3_class(raw, "data.frame")
  expect_gt(nrow(raw), 100)  # MN has ~2000+ schools
})

test_that("get_raw_school_directory has expected columns", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_school_directory()
  expected_cols <- c(
    "District Number", "District Type", "School Number",
    "Number", "Organization", "Title", "Name",
    "First Name", "Last Name", "Phone", "Email",
    "Physical City", "Physical State", "Physical Zip",
    "County", "Grades", "School Classification",
    "StateOrganizationId", "NCES ID"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(raw), info = paste("Missing column:", col))
  }
})

test_that("get_raw_district_directory returns a data frame with rows", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_district_directory()
  expect_s3_class(raw, "data.frame")
  expect_gt(nrow(raw), 50)  # MN has ~330+ districts
})

test_that("get_raw_district_directory has expected columns", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_district_directory()
  expected_cols <- c(
    "District Number", "District Type",
    "Number", "Organization", "Title", "Name",
    "First Name", "Last Name", "Phone", "Email",
    "Physical City", "Physical State", "Physical Zip",
    "County", "StateOrganizationId"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(raw), info = paste("Missing column:", col))
  }
})


# ==============================================================================
# STEP 3: Processing Tests
# ==============================================================================

test_that("process_school_directory produces standardized output", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_school_directory()
  processed <- mnschooldata:::process_school_directory(raw)

  expect_s3_class(processed, "data.frame")
  expect_gt(nrow(processed), 100)

  # Check required columns exist
  required_cols <- c(
    "entity_type", "state_district_id", "state_school_id",
    "school_name", "principal_name", "principal_email",
    "physical_city", "physical_state", "physical_zip",
    "county_name", "grades_served"
  )
  for (col in required_cols) {
    expect_true(col %in% names(processed), info = paste("Missing column:", col))
  }

  # entity_type should all be "school"
  expect_true(all(processed$entity_type == "school"))

  # state should all be MN
  non_na_states <- processed$physical_state[!is.na(processed$physical_state)]
  expect_true(all(non_na_states == "MN"))
})

test_that("process_district_directory produces standardized output", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_district_directory()
  processed <- mnschooldata:::process_district_directory(raw)

  expect_s3_class(processed, "data.frame")
  expect_gt(nrow(processed), 50)

  # Check required columns
  required_cols <- c(
    "entity_type", "state_district_id",
    "district_name", "superintendent_name", "superintendent_email",
    "physical_city", "physical_state", "physical_zip",
    "county_name"
  )
  for (col in required_cols) {
    expect_true(col %in% names(processed), info = paste("Missing column:", col))
  }

  # entity_type should all be "district"
  expect_true(all(processed$entity_type == "district"))
})


# ==============================================================================
# STEP 4: Data Quality Tests
# ==============================================================================

test_that("School directory IDs are well-formed", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_school_directory()
  processed <- mnschooldata:::process_school_directory(raw)

  # state_district_id should match DDDD-TT pattern
  expect_true(all(grepl("^\\d{4}-\\d{2}$", processed$state_district_id)))

  # state_school_id should match DDDD-TT-SSS pattern
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{3}$", processed$state_school_id)))
})

test_that("District directory IDs are well-formed", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_district_directory()
  processed <- mnschooldata:::process_district_directory(raw)

  # state_district_id should match DDDD-TT pattern
  expect_true(all(grepl("^\\d{4}-\\d{2}$", processed$state_district_id)))
})

test_that("School directory has non-empty principal names", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_school_directory()
  processed <- mnschooldata:::process_school_directory(raw)

  # At least 80% of schools should have a principal name
  pct_has_principal <- mean(!is.na(processed$principal_name))
  expect_gt(pct_has_principal, 0.8)
})

test_that("Known districts exist in directory", {
  skip_on_cran()
  skip_if_offline()

  raw <- mnschooldata:::get_raw_district_directory()
  processed <- mnschooldata:::process_district_directory(raw)

  # Minneapolis is district 0001-03
  expect_true("0001-03" %in% processed$state_district_id)

  # Anoka-Hennepin is district 0011-01
  expect_true("0011-01" %in% processed$state_district_id)
})


# ==============================================================================
# STEP 5: fetch_directory() Integration Tests
# ==============================================================================

test_that("fetch_directory returns combined data with expected columns", {
  skip_on_cran()
  skip_if_offline()

  dir <- mnschooldata::fetch_directory(use_cache = FALSE)

  expect_s3_class(dir, "data.frame")
  expect_gt(nrow(dir), 100)

  # Should have both principal and superintendent info
  expect_true("principal_name" %in% names(dir))
  expect_true("superintendent_name" %in% names(dir))
  expect_true("district_name" %in% names(dir))
  expect_true("school_name" %in% names(dir))
})

test_that("fetch_directory tidy=FALSE returns list with schools and districts", {
  skip_on_cran()
  skip_if_offline()

  dir_raw <- mnschooldata::fetch_directory(tidy = FALSE, use_cache = FALSE)

  expect_type(dir_raw, "list")
  expect_true("schools" %in% names(dir_raw))
  expect_true("districts" %in% names(dir_raw))
  expect_s3_class(dir_raw$schools, "data.frame")
  expect_s3_class(dir_raw$districts, "data.frame")
})

test_that("fetch_directory combined data has district names filled in", {
  skip_on_cran()
  skip_if_offline()

  dir <- mnschooldata::fetch_directory(use_cache = FALSE)

  # Most schools should have a district_name after the merge
  pct_has_district <- mean(!is.na(dir$district_name))
  expect_gt(pct_has_district, 0.9)
})


# ==============================================================================
# STEP 6: fetch_district_directory() Tests
# ==============================================================================

test_that("fetch_district_directory returns district data", {
  skip_on_cran()
  skip_if_offline()

  districts <- mnschooldata::fetch_district_directory(use_cache = FALSE)

  expect_s3_class(districts, "data.frame")
  expect_gt(nrow(districts), 50)
  expect_true(all(districts$entity_type == "district"))
  expect_true("superintendent_name" %in% names(districts))
})


# ==============================================================================
# STEP 7: Cache Tests
# ==============================================================================

test_that("directory cache round-trip works", {
  skip_on_cran()
  skip_if_offline()

  # Clear any existing cache
  mnschooldata::clear_directory_cache(9999)

  # Fetch with caching
  dir1 <- mnschooldata::fetch_directory(end_year = 9999, use_cache = TRUE)

  # Fetch again - should use cache
  dir2 <- mnschooldata::fetch_directory(end_year = 9999, use_cache = TRUE)

  expect_equal(nrow(dir1), nrow(dir2))
  expect_equal(names(dir1), names(dir2))


  # Clean up
  mnschooldata::clear_directory_cache(9999)
})
