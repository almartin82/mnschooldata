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
