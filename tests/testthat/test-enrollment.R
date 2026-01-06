# Tests for enrollment functions
# Note: Most tests are marked as skip_on_cran since they require network access

test_that("safe_numeric handles various inputs", {
  # Normal numbers
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)

  # Suppressed values
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("<10")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("NULL")))

  # Whitespace handling
  expect_equal(safe_numeric("  100  "), 100)
})


test_that("get_available_years returns valid range", {
  years <- get_available_years()

  expect_true(is.numeric(years))
  expect_true(length(years) > 10)
  expect_true(min(years) <= 2010)
  expect_true(max(years) >= 2024)
})


test_that("fetch_enr validates year parameter", {
  expect_error(fetch_enr(2000), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
})


test_that("get_cache_dir returns valid path", {
  cache_dir <- get_cache_dir()
  expect_true(is.character(cache_dir))
  expect_true(grepl("mnschooldata", cache_dir))
})


test_that("cache functions work correctly", {
  # Test cache path generation
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024.rds", path))

  # Test cache_exists returns FALSE for non-existent cache
  expect_false(cache_exists(9999, "tidy"))
})


test_that("get_mde_column_map returns expected structure", {
  col_map <- get_mde_column_map(2024)

  expect_true(is.list(col_map))
  expect_true("district_id" %in% names(col_map))
  expect_true("school_id" %in% names(col_map))
  expect_true("total" %in% names(col_map))
  expect_true("white" %in% names(col_map))
  expect_true("hispanic" %in% names(col_map))
  expect_true("econ_disadv" %in% names(col_map))
  expect_true("grade_k" %in% names(col_map))
})


# Integration tests (require network access)
test_that("fetch_enr downloads and processes data", {
  skip_on_cran()
  skip_if_offline()

  # Use a recent year - this test may fail if MDE changes URLs
  result <- tryCatch(
    fetch_enr(2023, tidy = FALSE, use_cache = FALSE),
    error = function(e) NULL
  )

  # Skip test if download fails (MDE may be unavailable)
  skip_if(is.null(result), "MDE data download failed - server may be unavailable")

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("district_id" %in% names(result))
  expect_true("row_total" %in% names(result))
  expect_true("type" %in% names(result))

  # Check we have expected levels
  expect_true("State" %in% result$type || "District" %in% result$type)
})


test_that("tidy_enr produces correct long format", {
  skip_on_cran()
  skip_if_offline()

  # Get wide data
  wide <- tryCatch(
    fetch_enr(2023, tidy = FALSE, use_cache = TRUE),
    error = function(e) NULL
  )

  skip_if(is.null(wide), "MDE data download failed - server may be unavailable")

  # Tidy it
  tidy_result <- tidy_enr(wide)

  # Check structure
  expect_true("grade_level" %in% names(tidy_result))
  expect_true("subgroup" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))

  # Check subgroups include expected values
  subgroups <- unique(tidy_result$subgroup)
  expect_true("total_enrollment" %in% subgroups)
})


test_that("id_enr_aggs adds correct flags", {
  skip_on_cran()
  skip_if_offline()

  # Get tidy data with aggregation flags
  result <- tryCatch(
    fetch_enr(2023, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "MDE data download failed - server may be unavailable")

  # Check flags exist
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))

  # Check flags are boolean
  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))
  expect_true(is.logical(result$is_campus))

  # Check mutual exclusivity (each row is only one type)
  type_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(type_sums == 1))
})


test_that("fetch_enr_multi validates years", {
  expect_error(fetch_enr_multi(c(2023, 2000)), "Invalid years")
  expect_error(fetch_enr_multi(c(2030, 2024)), "Invalid years")
})


test_that("enr_grade_aggs creates correct aggregations", {
  # Create minimal test data
  test_data <- data.frame(
    end_year = rep(2024, 4),
    type = rep("District", 4),
    district_id = rep("0001-01", 4),
    district_name = rep("Test District", 4),
    campus_id = rep(NA_character_, 4),
    campus_name = rep(NA_character_, 4),
    county = rep("Test County", 4),
    subgroup = rep("total_enrollment", 4),
    grade_level = c("K", "01", "09", "10"),
    n_students = c(100, 100, 50, 50),
    pct = c(0.25, 0.25, 0.25, 0.25),
    is_state = rep(FALSE, 4),
    is_district = rep(TRUE, 4),
    is_campus = rep(FALSE, 4)
  )

  aggs <- enr_grade_aggs(test_data)

  expect_true(is.data.frame(aggs))
  expect_true("grade_level" %in% names(aggs))
  expect_true(all(c("K8", "HS", "K12") %in% aggs$grade_level))

  # Check K8 sum (K + 01 = 200)
  k8_row <- aggs[aggs$grade_level == "K8", ]
  expect_equal(k8_row$n_students, 200)

  # Check HS sum (09 + 10 = 100)
  hs_row <- aggs[aggs$grade_level == "HS", ]
  expect_equal(hs_row$n_students, 100)

  # Check K12 sum (all grades = 300)
  k12_row <- aggs[aggs$grade_level == "K12", ]
  expect_equal(k12_row$n_students, 300)
})


test_that("tidy_enr produces all required columns", {
  # Create comprehensive test data
  test_wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "000001",
    campus_id = "000001001",
    district_name = "Test District",
    campus_name = "Test School",
    county = "Test County",
    row_total = 500,
    white = 300,
    black = 100,
    hispanic = 50,
    asian = 25,
    native_american = 10,
    pacific_islander = 5,
    multiracial = 10,
    male = 260,
    female = 240,
    special_ed = 50,
    lep = 75,
    econ_disadv = 200,
    homeless = 10,
    grade_pk = 40,
    grade_k = 45,
    grade_01 = 42,
    grade_02 = 40,
    grade_03 = 41,
    grade_04 = 39,
    grade_05 = 40,
    grade_06 = 38,
    grade_07 = 37,
    grade_08 = 36,
    grade_09 = 35,
    grade_10 = 34,
    grade_11 = 33,
    grade_12 = 30
  )

  tidy_result <- tidy_enr(test_wide)

  # Check all required columns are present
  required_cols <- c(
    "end_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "grade_level", "subgroup",
    "n_students", "pct"
  )
  for (col in required_cols) {
    expect_true(col %in% names(tidy_result),
                info = paste("Missing required column:", col))
  }

  # Check bonus columns
  expect_true("county" %in% names(tidy_result))
})


test_that("tidy_enr produces all expected subgroups", {
  test_wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "000001",
    campus_id = "000001001",
    district_name = "Test District",
    campus_name = "Test School",
    county = "Test County",
    row_total = 500,
    white = 300,
    black = 100,
    hispanic = 50,
    asian = 25,
    native_american = 10,
    pacific_islander = 5,
    multiracial = 10,
    male = 260,
    female = 240,
    special_ed = 50,
    lep = 75,
    econ_disadv = 200,
    homeless = 10,
    grade_k = 45,
    grade_01 = 42
  )

  tidy_result <- tidy_enr(test_wide)

  # Check all expected subgroups are present
  expected_subgroups <- c(
    "total_enrollment",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female",
    "special_ed", "lep", "econ_disadv", "homeless"
  )

  actual_subgroups <- unique(tidy_result$subgroup)

  for (subgrp in expected_subgroups) {
    expect_true(subgrp %in% actual_subgroups,
                info = paste("Missing expected subgroup:", subgrp))
  }
})


test_that("tidy_enr produces all expected grade levels", {
  test_wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "000001",
    campus_id = "000001001",
    district_name = "Test District",
    campus_name = "Test School",
    county = "Test County",
    row_total = 500,
    white = 300,
    grade_pk = 40,
    grade_k = 45,
    grade_01 = 42,
    grade_02 = 40,
    grade_03 = 41,
    grade_04 = 39,
    grade_05 = 40,
    grade_06 = 38,
    grade_07 = 37,
    grade_08 = 36,
    grade_09 = 35,
    grade_10 = 34,
    grade_11 = 33,
    grade_12 = 30
  )

  tidy_result <- tidy_enr(test_wide)

  # Check that we have TOTAL grade level
  expect_true("TOTAL" %in% unique(tidy_result$grade_level))

  # Check that we have individual grade levels
  expected_grades <- c("PK", "K", "01", "02", "03", "04", "05",
                       "06", "07", "08", "09", "10", "11", "12")

  # Filter to only total_enrollment subgroup for grade-level checks
  grade_rows <- tidy_result[tidy_result$subgroup == "total_enrollment", ]
  actual_grades <- unique(grade_rows$grade_level)

  for (grd in expected_grades) {
    expect_true(grd %in% actual_grades,
                info = paste("Missing expected grade level:", grd))
  }
})


test_that("tidy_enr maintains long format structure", {
  test_wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "000001",
    campus_id = "000001001",
    district_name = "Test District",
    campus_name = "Test School",
    county = "Test County",
    row_total = 100,
    white = 50,
    black = 20,
    grade_k = 20,
    grade_01 = 18
  )

  tidy_result <- tidy_enr(test_wide)

  # Check that we have multiple rows (one per grade_level-subgroup combination)
  expect_true(nrow(tidy_result) > 1)

  # Check that each row represents a unique combination
  # Total rows should equal: (demographic + gender subgroups) + grade levels
  # With 2 demographic (white, black) + 2 grade levels (K, 01) + total = 5+ rows
  expect_true(nrow(tidy_result) >= 5)

  # Verify grade_level = "TOTAL" for demographic subgroups
  demo_rows <- tidy_result[tidy_result$subgroup %in% c("white", "black"), ]
  expect_true(all(demo_rows$grade_level == "TOTAL"),
              info = "Demographic subgroups should have grade_level = TOTAL")

  # Verify subgroup = "total_enrollment" for grade levels
  grade_rows <- tidy_result[tidy_result$grade_level %in% c("K", "01"), ]
  expect_true(all(grade_rows$subgroup == "total_enrollment"),
              info = "Grade-level rows should have subgroup = total_enrollment")
})


test_that("tidy_enr produces valid percentages", {
  test_wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "000001",
    campus_id = "000001001",
    district_name = "Test District",
    campus_name = "Test School",
    county = "Test County",
    row_total = 100,
    white = 50,
    black = 30,
    male = 55,
    female = 45,
    grade_k = 25,
    grade_01 = 20
  )

  tidy_result <- tidy_enr(test_wide)

  # All percentages should be between 0 and 1
  expect_true(all(tidy_result$pct >= 0, na.rm = TRUE),
              info = "All percentages should be >= 0")
  expect_true(all(tidy_result$pct <= 1, na.rm = TRUE),
              info = "All percentages should be <= 1")

  # Check specific percentages
  # white = 50/100 = 0.5
  white_row <- tidy_result[tidy_result$subgroup == "white" & tidy_result$grade_level == "TOTAL", ]
  expect_equal(white_row$pct, 0.5, tolerance = 0.01)

  # black = 30/100 = 0.3
  black_row <- tidy_result[tidy_result$subgroup == "black" & tidy_result$grade_level == "TOTAL", ]
  expect_equal(black_row$pct, 0.3, tolerance = 0.01)

  # grade_k = 25/100 = 0.25
  grade_k_row <- tidy_result[tidy_result$subgroup == "total_enrollment" & tidy_result$grade_level == "K", ]
  expect_equal(grade_k_row$pct, 0.25, tolerance = 0.01)
})


test_that("tidy_enr has no Inf or NaN values", {
  test_wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "000001",
    campus_id = "000001001",
    district_name = "Test District",
    campus_name = "Test School",
    county = "Test County",
    row_total = 100,
    white = 50,
    grade_k = 25
  )

  tidy_result <- tidy_enr(test_wide)

  # Check for Inf values in n_students
  expect_false(any(is.infinite(tidy_result$n_students)),
               info = "n_students should not contain Inf values")

  # Check for NaN values in n_students
  expect_false(any(is.nan(tidy_result$n_students)),
               info = "n_students should not contain NaN values")

  # Check for Inf values in pct
  expect_false(any(is.infinite(tidy_result$pct)),
               info = "pct should not contain Inf values")

  # Check for NaN values in pct
  expect_false(any(is.nan(tidy_result$pct)),
               info = "pct should not contain NaN values")
})
