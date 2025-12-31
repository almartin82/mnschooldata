# Tests for cache functions

test_that("cache directory is created correctly", {
  cache_dir <- get_cache_dir()

  expect_true(is.character(cache_dir))
  expect_true(dir.exists(cache_dir))
  expect_true(grepl("mnschooldata", cache_dir))
})


test_that("cache path generation works", {
  path_2024_tidy <- get_cache_path(2024, "tidy")
  path_2024_wide <- get_cache_path(2024, "wide")
  path_2023_tidy <- get_cache_path(2023, "tidy")

  expect_true(grepl("enr_tidy_2024.rds", path_2024_tidy))
  expect_true(grepl("enr_wide_2024.rds", path_2024_wide))
  expect_true(grepl("enr_tidy_2023.rds", path_2023_tidy))

  # Different years should have different paths

  expect_false(path_2024_tidy == path_2023_tidy)

  # Different types should have different paths
  expect_false(path_2024_tidy == path_2024_wide)
})


test_that("cache_exists returns FALSE for non-existent cache", {
  # Use a year that definitely won't exist
  expect_false(cache_exists(9999, "tidy"))
  expect_false(cache_exists(9999, "wide"))
})


test_that("cache write and read roundtrip works", {
  # Create test data
  test_df <- data.frame(
    end_year = 9998,
    district_id = "TEST001",
    value = 12345,
    stringsAsFactors = FALSE
  )

  # Write to cache
  cache_path <- write_cache(test_df, 9998, "test")
  expect_true(file.exists(cache_path))

  # Read from cache
  read_df <- read_cache(9998, "test")
  expect_equal(read_df$end_year, 9998)
  expect_equal(read_df$district_id, "TEST001")
  expect_equal(read_df$value, 12345)

  # Clean up
  unlink(cache_path)
})


test_that("cache_exists respects max_age", {
  # Create test data and cache it
  test_df <- data.frame(end_year = 9997, value = 1)
  cache_path <- write_cache(test_df, 9997, "agetest")

  # Should exist with default max_age (30 days)
  expect_true(cache_exists(9997, "agetest", max_age = 30))

  # Should exist with very short max_age (file was just created)
  expect_true(cache_exists(9997, "agetest", max_age = 0.01))

  # Clean up
  unlink(cache_path)
})


test_that("clear_cache removes files", {
  # Create test cache files
  test_df1 <- data.frame(end_year = 9996, value = 1)
  test_df2 <- data.frame(end_year = 9996, value = 2)
  test_df3 <- data.frame(end_year = 9995, value = 3)

  path1 <- write_cache(test_df1, 9996, "tidy")
  path2 <- write_cache(test_df2, 9996, "wide")
  path3 <- write_cache(test_df3, 9995, "tidy")

  # All should exist
  expect_true(file.exists(path1))
  expect_true(file.exists(path2))
  expect_true(file.exists(path3))

  # Clear specific year
  clear_cache(end_year = 9996)
  expect_false(file.exists(path1))
  expect_false(file.exists(path2))
  expect_true(file.exists(path3))

  # Clean up remaining
  unlink(path3)
})


test_that("cache_status returns correct structure", {
  # Create test cache file
  test_df <- data.frame(end_year = 9994, value = 1)
  path <- write_cache(test_df, 9994, "statustest")

  # Get status (should return a data frame)
  result <- cache_status()

  expect_true(is.data.frame(result) || nrow(result) == 0)

  # Clean up
  unlink(path)
})
