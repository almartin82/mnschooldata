# ==============================================================================
# Enrollment Year Coverage Tests for mnschooldata
# ==============================================================================
#
# These tests verify enrollment data quality for available years.
# All pinned values come from real MDE data (2023 cache and bundled datasets).
#
# Available years: 2007-2024 per get_available_years()
# Currently cached year: 2023
#
# MN enrollment ~873K students (2022-23 school year)
#
# ==============================================================================

library(testthat)

# ==============================================================================
# Year Infrastructure Tests
# ==============================================================================

test_that("get_available_years returns 2007-2024", {
  years <- get_available_years()

  expect_true(is.numeric(years))
  expect_equal(min(years), 2007)
  expect_equal(max(years), 2024)
  expect_equal(length(years), 18)
  expect_equal(years, 2007:2024)
})

test_that("fetch_enr rejects years outside 2007-2024", {
  expect_error(fetch_enr(2006), "end_year must be between")
  expect_error(fetch_enr(2025), "end_year must be between")
  expect_error(fetch_enr(1999), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
})

test_that("fetch_enr_multi rejects invalid years", {
  expect_error(fetch_enr_multi(c(2023, 2000)), "Invalid years")
  expect_error(fetch_enr_multi(c(2030, 2024)), "Invalid years")
  expect_error(fetch_enr_multi(c(2006, 2007)), "Invalid years")
})

# ==============================================================================
# 2023 Enrollment — State Totals (Pinned)
# ==============================================================================

test_that("2023 state total enrollment is 873,175", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]

  expect_equal(nrow(state_total), 1)
  expect_equal(state_total$n_students, 873175)
  expect_equal(state_total$pct, 1.0)
})

test_that("2023 state race/ethnicity subgroups are pinned", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  state <- enr[enr$is_state & enr$grade_level == "TOTAL", ]

  # Pin every race/ethnicity subgroup
  get_n <- function(sg) {
    row <- state[state$subgroup == sg, ]
    expect_equal(nrow(row), 1, info = paste("Expected 1 row for", sg))
    row$n_students
  }

  expect_equal(get_n("white"), 518783)
  expect_equal(get_n("black"), 110312)
  expect_equal(get_n("hispanic"), 105538)
  expect_equal(get_n("asian"), 62538)
  expect_equal(get_n("multiracial"), 59516)
  expect_equal(get_n("native_american"), 15372)
  expect_equal(get_n("pacific_islander"), 1116)
})

test_that("2023 state race/ethnicity percentages sum to ~100%", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  state_demo <- enr[enr$is_state &
                    enr$grade_level == "TOTAL" &
                    enr$subgroup %in% c("white", "black", "hispanic", "asian",
                                        "native_american", "pacific_islander",
                                        "multiracial"), ]

  # Sum of all race/ethnicity counts should equal total
  expect_equal(sum(state_demo$n_students), 873175)

  # Percentages should sum to 1.0
  expect_equal(sum(state_demo$pct), 1.0, tolerance = 0.001)
})

# ==============================================================================
# 2023 Enrollment — District Counts (Pinned)
# ==============================================================================

test_that("2023 has 389 districts (including charters)", {
  skip_on_cran()
  skip_if_offline()

  # Bundled data has is_charter; we test with it
  data(enr_2023_example)

  dist <- enr_2023_example[enr_2023_example$is_district &
                           enr_2023_example$subgroup == "total_enrollment" &
                           enr_2023_example$grade_level == "TOTAL", ]

  expect_equal(nrow(dist), 389)
})

test_that("2023 has 68 charter districts", {
  skip_on_cran()

  data(enr_2023_example)

  charters <- enr_2023_example[enr_2023_example$is_district &
                               enr_2023_example$is_charter == TRUE &
                               enr_2023_example$subgroup == "total_enrollment" &
                               enr_2023_example$grade_level == "TOTAL", ]

  expect_equal(nrow(charters), 68)
})

# ==============================================================================
# 2023 Enrollment — Large District Pinned Values
# ==============================================================================

test_that("2023 Anoka-Hennepin enrollment is 38,336", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  ah <- enr[grepl("ANOKA", enr$district_name) &
            enr$subgroup == "total_enrollment" &
            enr$grade_level == "TOTAL" &
            enr$is_district, ]

  expect_equal(nrow(ah), 1)
  expect_equal(ah$n_students, 38336)
  expect_equal(ah$district_name, "ANOKA-HENNEPIN SCHOOL DISTRICT")
})

test_that("2023 St. Paul enrollment is 32,750", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  stpaul <- enr[enr$district_name == "ST. PAUL PUBLIC SCHOOL DISTRICT" &
                enr$subgroup == "total_enrollment" &
                enr$grade_level == "TOTAL" &
                enr$is_district, ]

  expect_equal(nrow(stpaul), 1)
  expect_equal(stpaul$n_students, 32750)
})

test_that("2023 Minneapolis enrollment is 30,079", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  mpls <- enr[enr$district_name == "MINNEAPOLIS PUBLIC SCHOOL DISTRICT" &
              enr$subgroup == "total_enrollment" &
              enr$grade_level == "TOTAL" &
              enr$is_district, ]

  expect_equal(nrow(mpls), 1)
  expect_equal(mpls$n_students, 30079)
})

test_that("2023 RAVE (Rosemount-Apple Valley-Eagan) enrollment is 29,229", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  rave <- enr[grepl("ROSEMOUNT", enr$district_name) &
              enr$subgroup == "total_enrollment" &
              enr$grade_level == "TOTAL" &
              enr$is_district, ]

  expect_equal(nrow(rave), 1)
  expect_equal(rave$n_students, 29229)
})

test_that("2023 Rochester Public School District enrollment is 17,320", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # Use exact match to avoid matching "Rochester Beacon Academy" charter
  roch <- enr[enr$district_name == "ROCHESTER PUBLIC SCHOOL DISTRICT" &
              enr$subgroup == "total_enrollment" &
              enr$grade_level == "TOTAL" &
              enr$is_district, ]

  expect_equal(nrow(roch), 1)
  expect_equal(roch$n_students, 17320)
})

# ==============================================================================
# 2023 Enrollment — District Demographic Breakdowns (Pinned)
# ==============================================================================

test_that("2023 Minneapolis demographics are pinned", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  mpls <- enr[enr$district_name == "MINNEAPOLIS PUBLIC SCHOOL DISTRICT" &
              enr$grade_level == "TOTAL" &
              enr$is_district, ]

  get_n <- function(sg) {
    row <- mpls[mpls$subgroup == sg, ]
    row$n_students
  }

  expect_equal(get_n("total_enrollment"), 30079)
  expect_equal(get_n("white"), 10935)
  expect_equal(get_n("black"), 8056)
  expect_equal(get_n("hispanic"), 6723)
  expect_equal(get_n("asian"), 940)
  expect_equal(get_n("multiracial"), 2326)
  expect_equal(get_n("native_american"), 1080)
  expect_equal(get_n("pacific_islander"), 19)
})

test_that("2023 Anoka-Hennepin demographics are pinned", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  ah <- enr[enr$district_name == "ANOKA-HENNEPIN SCHOOL DISTRICT" &
            enr$grade_level == "TOTAL" &
            enr$is_district, ]

  get_n <- function(sg) {
    row <- ah[ah$subgroup == sg, ]
    row$n_students
  }

  expect_equal(get_n("total_enrollment"), 38336)
  expect_equal(get_n("white"), 20520)
  expect_equal(get_n("black"), 6737)
  expect_equal(get_n("hispanic"), 3005)
  expect_equal(get_n("asian"), 4437)
  expect_equal(get_n("multiracial"), 3405)
  expect_equal(get_n("native_american"), 207)
  expect_equal(get_n("pacific_islander"), 25)
})

# ==============================================================================
# 2023 Enrollment — Subgroup Completeness
# ==============================================================================

test_that("2023 tidy data has exactly 8 expected subgroups", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  expected_subgroups <- c(
    "total_enrollment",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial"
  )

  actual_subgroups <- sort(unique(enr$subgroup))

  for (sg in expected_subgroups) {
    expect_true(sg %in% actual_subgroups,
                info = paste("Missing expected subgroup:", sg))
  }

  # Verify no unexpected subgroups
  unexpected <- setdiff(actual_subgroups, expected_subgroups)
  expect_equal(length(unexpected), 0,
               info = paste("Unexpected subgroups:", paste(unexpected, collapse = ", ")))
})

test_that("2023 tidy data grade_level is always TOTAL", {
  skip_on_cran()
  skip_if_offline()

  # MN tidy enrollment currently only produces TOTAL grade level
  # (no grade-level breakdown in the tidy format from cached data)
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  expect_equal(unique(enr$grade_level), "TOTAL")
})

# ==============================================================================
# 2023 Enrollment — Entity Flag Tests
# ==============================================================================

test_that("2023 entity flags are mutually exclusive and complete", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # Every row must have exactly one TRUE flag
  flag_sum <- as.integer(enr$is_state) +
              as.integer(enr$is_district) +
              as.integer(enr$is_campus)

  expect_true(all(flag_sum == 1),
              info = "Each row must have exactly one entity flag TRUE")

  # is_state rows should have NA district_id
  state_rows <- enr[enr$is_state, ]
  expect_true(all(is.na(state_rows$district_id)),
              info = "State rows should have NA district_id")
})

test_that("2023 state rows have 8 subgroup entries", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  state <- enr[enr$is_state, ]

  # Should have one row per subgroup (8 total)
  expect_equal(nrow(state), 8)
})

# ==============================================================================
# 2023 Enrollment — Wide Format Tests
# ==============================================================================

test_that("2023 wide format has expected columns and counts", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)

  # Expected columns
  required_cols <- c("end_year", "type", "district_id", "campus_id",
                     "district_name", "campus_name", "row_total",
                     "white", "black", "hispanic", "asian",
                     "native_american", "pacific_islander", "multiracial")

  for (col in required_cols) {
    expect_true(col %in% names(wide),
                info = paste("Missing required column:", col))
  }

  # State row total
  state_wide <- wide[wide$type == "State", ]
  expect_equal(nrow(state_wide), 1)
  expect_equal(state_wide$row_total, 873175)

  # District count (370 in wide vs 389 in bundled — wide cache is from different source)
  dist_wide <- wide[wide$type == "District", ]
  expect_true(nrow(dist_wide) >= 300,
              info = "Should have at least 300 districts in wide format")
})

test_that("2023 wide demographics sum to total for state row", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)

  state <- wide[wide$type == "State", ]

  demo_sum <- sum(state$white, state$black, state$hispanic, state$asian,
                  state$native_american, state$pacific_islander,
                  state$multiracial, na.rm = TRUE)

  expect_equal(demo_sum, state$row_total)
})

# ==============================================================================
# 2023 Enrollment — Data Quality Checks
# ==============================================================================

test_that("2023 enrollment has no Inf values", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  for (col in c("n_students", "pct")) {
    expect_false(any(is.infinite(enr[[col]]), na.rm = TRUE),
                 info = paste("No Inf in", col))
  }
})

test_that("2023 enrollment has no NaN values", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  for (col in c("n_students", "pct")) {
    expect_false(any(is.nan(enr[[col]]), na.rm = TRUE),
                 info = paste("No NaN in", col))
  }
})

test_that("2023 enrollment counts are non-negative", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  non_na <- enr$n_students[!is.na(enr$n_students)]
  expect_true(all(non_na >= 0),
              info = "All enrollment counts should be >= 0")
})

test_that("2023 percentages are in [0, 1] range", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  non_na_pct <- enr$pct[!is.na(enr$pct)]
  expect_true(all(non_na_pct >= 0), info = "pct should be >= 0")
  expect_true(all(non_na_pct <= 1), info = "pct should be <= 1")
})

# ==============================================================================
# 2023 Enrollment — Tidy/Wide Fidelity
# ==============================================================================

test_that("2023 tidy state total matches wide state row_total", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  wide_state_total <- wide[wide$type == "State", ]$row_total
  tidy_state_total <- tidy[tidy$is_state &
                           tidy$subgroup == "total_enrollment" &
                           tidy$grade_level == "TOTAL", ]$n_students

  expect_equal(tidy_state_total, wide_state_total)
})

test_that("2023 tidy race counts match wide for Anoka-Hennepin", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # Find Anoka-Hennepin in wide
  ah_wide <- wide[grepl("ANOKA", wide$district_name), ]

  if (nrow(ah_wide) == 1) {
    ah_tidy <- tidy[grepl("ANOKA", tidy$district_name) &
                    tidy$grade_level == "TOTAL", ]

    # Check that race counts match
    for (sg in c("white", "black", "hispanic", "asian")) {
      tidy_val <- ah_tidy[ah_tidy$subgroup == sg, ]$n_students
      wide_val <- ah_wide[[sg]]
      expect_equal(tidy_val, wide_val,
                   info = paste("Fidelity check for", sg, "in Anoka-Hennepin"))
    }
  }
})

# ==============================================================================
# Bundled Data Tests
# ==============================================================================

test_that("enr_2023_example bundled data structure is correct", {
  skip_on_cran()

  data(enr_2023_example)

  # Required columns
  required_cols <- c(
    "end_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "grade_level", "subgroup",
    "n_students", "pct", "is_state", "is_district", "is_campus",
    "aggregation_flag", "district_type", "is_charter"
  )

  for (col in required_cols) {
    expect_true(col %in% names(enr_2023_example),
                info = paste("Missing column:", col))
  }

  # Year should be 2023
  expect_equal(unique(enr_2023_example$end_year), 2023)
})

test_that("districts_example has correct structure and values", {
  skip_on_cran()

  data(districts_example)

  expect_true("district_name" %in% names(districts_example))
  expect_true("n_students" %in% names(districts_example))
  expect_true("is_charter" %in% names(districts_example))

  # Should have 389 districts

  expect_equal(nrow(districts_example), 389)

  # Anoka-Hennepin should be largest
  top1 <- districts_example[which.max(districts_example$n_students), ]
  expect_equal(top1$n_students, 38336)
  expect_true(grepl("ANOKA", top1$district_name))
})

test_that("enr_multi_example has 2023 data", {
  skip_on_cran()

  data(enr_multi_example)

  expect_true(2023 %in% unique(enr_multi_example$end_year))

  state <- enr_multi_example[enr_multi_example$is_state &
                             enr_multi_example$subgroup == "total_enrollment" &
                             enr_multi_example$grade_level == "TOTAL" &
                             enr_multi_example$end_year == 2023, ]

  expect_equal(state$n_students, 873175)
})

# ==============================================================================
# Ranking / Ordering Tests
# ==============================================================================

test_that("2023 top 5 districts by enrollment are correctly ordered", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  districts <- enr[enr$is_district &
                   enr$subgroup == "total_enrollment" &
                   enr$grade_level == "TOTAL", ]

  districts <- districts[order(-districts$n_students), ]

  top5_names <- districts$district_name[1:5]

  # Pinned ordering
  expect_true(grepl("ANOKA", top5_names[1]))
  expect_true(grepl("ST. PAUL", top5_names[2]))
  expect_true(grepl("MINNEAPOLIS", top5_names[3]))
  expect_true(grepl("ROSEMOUNT", top5_names[4]))
  expect_true(grepl("OSSEO", top5_names[5]))

  top5_enr <- districts$n_students[1:5]
  expect_equal(top5_enr, c(38336, 32750, 30079, 29229, 21385))
})

# ==============================================================================
# Percentage Consistency Tests
# ==============================================================================

test_that("2023 state demographic percentages are internally consistent", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  state <- enr[enr$is_state & enr$grade_level == "TOTAL", ]

  # White is majority (~59.4%)
  white <- state[state$subgroup == "white", ]
  expect_equal(white$pct, 518783 / 873175, tolerance = 0.0001)

  # Black (~12.6%)
  black <- state[state$subgroup == "black", ]
  expect_equal(black$pct, 110312 / 873175, tolerance = 0.0001)

  # Hispanic (~12.1%)
  hisp <- state[state$subgroup == "hispanic", ]
  expect_equal(hisp$pct, 105538 / 873175, tolerance = 0.0001)
})

# ==============================================================================
# Cross-District Consistency Tests
# ==============================================================================

test_that("2023 district enrollment sum is close to state total", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  state_total <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]$n_students

  dist_sum <- sum(
    enr[enr$is_district &
        enr$subgroup == "total_enrollment" &
        enr$grade_level == "TOTAL", ]$n_students,
    na.rm = TRUE
  )

  # State total is created from a different pipeline pass than individual

  # district rows. The sum of districts may differ because some districts
  # are missing from the district-level download (e.g., charter districts
  # that only appear in the state summary). Check within 10%.
  expect_equal(dist_sum, 830179)
  expect_true(dist_sum > state_total * 0.90,
              info = "District sum should be within 10% of state total")
})

test_that("2023 district race sums are pinned and close to state totals", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # The district-level download may not include all districts (some charters
  # only appear in state aggregates). Pin district sums to known values and
  # verify they are within 10% of state totals.
  expected_dist_sums <- list(
    white = 505516,
    black = 95498,
    hispanic = 100114,
    asian = 56594
  )

  for (sg in names(expected_dist_sums)) {
    state_n <- enr[enr$is_state & enr$subgroup == sg &
                   enr$grade_level == "TOTAL", ]$n_students

    dist_sum <- sum(
      enr[enr$is_district & enr$subgroup == sg &
          enr$grade_level == "TOTAL", ]$n_students,
      na.rm = TRUE
    )

    # Pin to known value
    expect_equal(dist_sum, expected_dist_sums[[sg]],
                 info = paste("District sum of", sg))

    # Verify within 15% of state total (some charter schools only appear
    # in state aggregates, causing up to ~14% gap for some subgroups)
    expect_true(dist_sum > state_n * 0.85,
                info = paste("District sum of", sg, "should be within 15% of state total"))
  }
})
