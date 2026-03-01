# ==============================================================================
# Typology Guard Tests for mnschooldata
# ==============================================================================
#
# These tests prevent regressions in naming standards, column types,
# entity classification, and cross-function consistency.
#
# Guard categories:
# 1. Naming Standards — subgroup, grade_level, entity flags match project-wide
# 2. Column Type Guards — numeric stays numeric, character stays character
# 3. Entity Classification — State/District/Campus mutually exclusive
# 4. No Data Fabrication — no random/hardcoded data
# 5. Tidy/Wide Fidelity — tidy transformation preserves raw counts
# 6. Filter Safety — standard filter patterns return non-empty results
# 7. Boundary Conditions — suppressed data, zero enrollment, edge cases
#
# ==============================================================================

library(testthat)

# ==============================================================================
# 1. Naming Standards Guards
# ==============================================================================

test_that("enrollment subgroup names follow project standard", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # Allowed subgroup names per project standard
  allowed_subgroups <- c(
    "total_enrollment",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female",
    "special_ed", "lep", "econ_disadv", "homeless",
    "free_reduced_lunch"
  )

  actual_subgroups <- unique(enr$subgroup)

  # Every actual subgroup must be in the allowed list
 for (sg in actual_subgroups) {
    expect_true(sg %in% allowed_subgroups,
                info = paste("Non-standard subgroup name:", sg,
                             "- should be one of:", paste(allowed_subgroups, collapse = ", ")))
  }
})

test_that("enrollment subgroups never use banned variants", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  actual <- unique(enr$subgroup)

  # Banned names per CLAUDE.md naming standards
  banned <- c(
    "total",               # should be total_enrollment
    "low_income",          # should be econ_disadv
    "economically_disadvantaged",  # should be econ_disadv
    "socioeconomically_disadvantaged",
    "frl",                 # should be econ_disadv or free_reduced_lunch
    "iep",                 # should be special_ed
    "disability",
    "students_with_disabilities",
    "el", "ell", "english_learner",  # should be lep
    "american_indian",     # should be native_american
    "two_or_more"          # should be multiracial
  )

  for (b in banned) {
    expect_false(b %in% actual,
                 info = paste("Banned subgroup name found:", b))
  }
})

test_that("grade_level values follow UPPERCASE standard", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  grades <- unique(enr$grade_level)

  # All should be uppercase
  for (g in grades) {
    expect_equal(g, toupper(g),
                 info = paste("Grade level should be uppercase:", g))
  }

  # Allowed grade levels
  allowed_grades <- c(
    "PK", "K",
    "01", "02", "03", "04", "05", "06", "07", "08",
    "09", "10", "11", "12",
    "TOTAL",
    # Grade aggregates from enr_grade_aggs
    "K8", "HS", "K12"
  )

  for (g in grades) {
    expect_true(g %in% allowed_grades,
                info = paste("Non-standard grade level:", g))
  }
})

test_that("entity flag columns use standard names", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # Required entity flags per CLAUDE.md
  expect_true("is_state" %in% names(enr), info = "Missing is_state column")
  expect_true("is_district" %in% names(enr), info = "Missing is_district column")
  expect_true("is_campus" %in% names(enr), info = "Missing is_campus column")
})

test_that("type column uses standard values", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  allowed_types <- c("State", "District", "Campus")
  actual_types <- unique(enr$type)

  for (t in actual_types) {
    expect_true(t %in% allowed_types,
                info = paste("Non-standard type value:", t))
  }
})

# ==============================================================================
# 2. Column Type Guards
# ==============================================================================

test_that("n_students is numeric", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  expect_true(is.numeric(enr$n_students),
              info = "n_students must be numeric, not character")
})

test_that("pct is numeric in [0, 1] range", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  expect_true(is.numeric(enr$pct), info = "pct must be numeric")

  # Check range
  non_na <- enr$pct[!is.na(enr$pct)]
  expect_true(all(non_na >= 0), info = "pct should be >= 0")
  expect_true(all(non_na <= 1), info = "pct should be <= 1 (not 0-100)")
})

test_that("end_year is numeric", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  expect_true(is.numeric(enr$end_year), info = "end_year must be numeric")
})

test_that("entity flags are logical", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  expect_true(is.logical(enr$is_state), info = "is_state must be logical")
  expect_true(is.logical(enr$is_district), info = "is_district must be logical")
  expect_true(is.logical(enr$is_campus), info = "is_campus must be logical")
})

test_that("district_id and campus_id are character", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  expect_true(is.character(enr$district_id), info = "district_id must be character")
  expect_true(is.character(enr$campus_id), info = "campus_id must be character")
})

test_that("wide format row_total is numeric", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)

  expect_true(is.numeric(wide$row_total),
              info = "row_total must be numeric, not character")
})

# ==============================================================================
# 3. Entity Classification Guards
# ==============================================================================

test_that("entity flags are mutually exclusive", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  flag_sum <- as.integer(enr$is_state) +
              as.integer(enr$is_district) +
              as.integer(enr$is_campus)

  expect_true(all(flag_sum == 1),
              info = "Each row must have exactly one entity flag TRUE")
})

test_that("is_state TRUE rows match type == State", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  state_by_flag <- enr[enr$is_state, ]
  state_by_type <- enr[enr$type == "State", ]

  expect_equal(nrow(state_by_flag), nrow(state_by_type))
})

test_that("is_district TRUE rows match type == District", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  dist_by_flag <- enr[enr$is_district, ]
  dist_by_type <- enr[enr$type == "District", ]

  expect_equal(nrow(dist_by_flag), nrow(dist_by_type))
})

test_that("state rows have NA district_id and campus_id", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  state <- enr[enr$is_state, ]

  expect_true(all(is.na(state$district_id)),
              info = "State rows should have NA district_id")
  expect_true(all(is.na(state$campus_id)),
              info = "State rows should have NA campus_id")
})

test_that("district rows have non-NA district_id and NA campus_id", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  dist <- enr[enr$is_district, ]

  expect_true(all(!is.na(dist$district_id)),
              info = "District rows should have non-NA district_id")
  expect_true(all(is.na(dist$campus_id)),
              info = "District rows should have NA campus_id")
})

# ==============================================================================
# 4. No Data Fabrication Guards
# ==============================================================================

test_that("no set.seed, rnorm, or random generation in R source files", {
  r_files <- list.files(
    system.file("R", package = "mnschooldata"),
    pattern = "\\.R$",
    full.names = TRUE
  )

  # If installed R files not found, use local R/ directory
  if (length(r_files) == 0) {
    pkg_dir <- system.file(package = "mnschooldata")
    r_files <- list.files(file.path(pkg_dir, "R"), pattern = "\\.R$", full.names = TRUE)
  }

  if (length(r_files) > 0) {
    for (f in r_files) {
      content <- readLines(f, warn = FALSE)
      content_str <- paste(content, collapse = "\n")

      # Check for random generation functions
      expect_false(grepl("set\\.seed", content_str),
                   info = paste("set.seed found in", basename(f)))
      expect_false(grepl("\\brnorm\\b", content_str),
                   info = paste("rnorm found in", basename(f)))
      expect_false(grepl("\\brunif\\b", content_str),
                   info = paste("runif found in", basename(f)))
      expect_false(grepl("\\bsample\\(", content_str),
                   info = paste("sample() found in", basename(f)))
    }
  }
})

test_that("no create_example_data function exists", {
  expect_false(
    exists("create_example_data", where = asNamespace("mnschooldata")),
    info = "create_example_data function should not exist (fabrication risk)"
  )
})

# ==============================================================================
# 5. Tidy/Wide Fidelity Guards
# ==============================================================================

test_that("tidy total matches wide row_total for state", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  wide_state <- wide[wide$type == "State", ]$row_total
  tidy_state <- tidy[tidy$is_state &
                     tidy$subgroup == "total_enrollment" &
                     tidy$grade_level == "TOTAL", ]$n_students

  expect_equal(tidy_state, wide_state,
               info = "Tidy state total must match wide row_total")
})

test_that("tidy race counts match wide for all districts", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # Check a sample of districts
  wide_dists <- wide[wide$type == "District" & !is.na(wide$district_id), ]

  if (nrow(wide_dists) > 0) {
    # Sample up to 10 districts for checking
    sample_ids <- head(wide_dists$district_id, 10)

    for (did in sample_ids) {
      wd <- wide_dists[wide_dists$district_id == did, ]
      # Use !is.na() to avoid NA row leakage from == operator
      td <- tidy[!is.na(tidy$district_id) & tidy$district_id == did &
                 !is.na(tidy$grade_level) & tidy$grade_level == "TOTAL", ]

      if (nrow(wd) == 1 && nrow(td) > 0) {
        # Check total_enrollment
        tidy_total <- td[!is.na(td$subgroup) & td$subgroup == "total_enrollment", ]
        if (nrow(tidy_total) == 1) {
          expect_equal(tidy_total$n_students, wd$row_total,
                       info = paste("Total fidelity check for district", did))
        }

        # Check white
        tidy_white <- td[!is.na(td$subgroup) & td$subgroup == "white", ]
        if (nrow(tidy_white) == 1 && "white" %in% names(wd)) {
          expect_equal(tidy_white$n_students, wd$white,
                       info = paste("White fidelity check for district", did))
        }
      }
    }
  }
})

test_that("no rounding occurs during tidy transformation", {
  # Use synthetic data to verify no rounding
  test_wide <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "000001",
    campus_id = NA_character_,
    district_name = "Test District",
    campus_name = NA_character_,
    county = "Test County",
    row_total = 777,
    white = 333,
    black = 222,
    hispanic = 111,
    asian = 55,
    native_american = 33,
    pacific_islander = 12,
    multiracial = 11,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(test_wide)

  # Verify counts are preserved exactly (no rounding)
  white_row <- tidy_result[tidy_result$subgroup == "white", ]
  expect_equal(white_row$n_students, 333)

  asian_row <- tidy_result[tidy_result$subgroup == "asian", ]
  expect_equal(asian_row$n_students, 55)

  pi_row <- tidy_result[tidy_result$subgroup == "pacific_islander", ]
  expect_equal(pi_row$n_students, 12)
})

# ==============================================================================
# 6. Filter Safety Guards
# ==============================================================================

test_that("standard enrollment filters return non-empty results", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # Filter: state total
  result <- enr[enr$is_state &
                enr$subgroup == "total_enrollment" &
                enr$grade_level == "TOTAL", ]
  expect_gt(nrow(result), 0, label = "state total filter should return rows")

  # Filter: district totals
  result <- enr[enr$is_district &
                enr$subgroup == "total_enrollment" &
                enr$grade_level == "TOTAL", ]
  expect_gt(nrow(result), 0, label = "district total filter should return rows")

  # Filter: specific demographic
  result <- enr[enr$is_state &
                enr$subgroup == "hispanic" &
                enr$grade_level == "TOTAL", ]
  expect_gt(nrow(result), 0, label = "state hispanic filter should return rows")

  # Filter: by district name
  result <- enr[grepl("MINNEAPOLIS", enr$district_name) &
                enr$subgroup == "total_enrollment", ]
  expect_gt(nrow(result), 0, label = "Minneapolis filter should return rows")
})

test_that("filter on nonexistent subgroup returns 0 rows", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # These should return 0 rows (values that don't exist)
  result <- enr[enr$subgroup == "el", ]
  expect_equal(nrow(result), 0, info = "el is not a valid subgroup name (use lep)")

  result <- enr[enr$subgroup == "ell", ]
  expect_equal(nrow(result), 0, info = "ell is not a valid subgroup name (use lep)")

  result <- enr[enr$subgroup == "two_or_more", ]
  expect_equal(nrow(result), 0, info = "two_or_more is not valid (use multiracial)")

  result <- enr[enr$subgroup == "american_indian", ]
  expect_equal(nrow(result), 0, info = "american_indian is not valid (use native_american)")
})

# ==============================================================================
# 7. Boundary Condition Guards
# ==============================================================================

test_that("NA counts do not produce Inf percentages", {
  # When row_total is 0, pct should be NaN or NA, not Inf
  test_wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "000001",
    campus_id = "000001001",
    district_name = "Test District",
    campus_name = "Empty School",
    county = "Test County",
    row_total = 0,
    white = 0,
    black = 0,
    stringsAsFactors = FALSE
  )

  # tidy_enr filters out NA n_students, but we check for Inf in what remains
  tidy_result <- tidy_enr(test_wide)

  if (nrow(tidy_result) > 0) {
    expect_false(any(is.infinite(tidy_result$pct), na.rm = TRUE),
                 info = "Zero enrollment should not produce Inf percentages")
    expect_false(any(is.infinite(tidy_result$n_students), na.rm = TRUE),
                 info = "Zero enrollment should not produce Inf counts")
  }
})

test_that("suppressed data (NA) is handled correctly in wide format", {
  # Suppression markers should become NA, not errors
  test_wide <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "000001",
    campus_id = "000001001",
    district_name = "Test District",
    campus_name = "Small School",
    county = "Test County",
    row_total = 100,
    white = 50,
    black = NA_real_,  # Suppressed
    hispanic = NA_real_,
    stringsAsFactors = FALSE
  )

  tidy_result <- tidy_enr(test_wide)

  # NA counts should be filtered out by tidy_enr
  # But total_enrollment and white should remain
  total_row <- tidy_result[tidy_result$subgroup == "total_enrollment", ]
  expect_equal(nrow(total_row), 1)
  expect_equal(total_row$n_students, 100)

  white_row <- tidy_result[tidy_result$subgroup == "white", ]
  expect_equal(nrow(white_row), 1)
  expect_equal(white_row$n_students, 50)

  # black and hispanic should be filtered out (NA counts)
  black_row <- tidy_result[tidy_result$subgroup == "black", ]
  expect_equal(nrow(black_row), 0)
})

test_that("safe_numeric handles MDE suppression markers", {
  # These are common MDE suppression patterns
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("<10")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("NULL")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric(">95")))
})

test_that("safe_numeric preserves valid numbers", {
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("0"), 0)
  expect_equal(safe_numeric("  100  "), 100)
  expect_equal(safe_numeric("873175"), 873175)
})

# ==============================================================================
# 8. Grade Aggregation Guards
# ==============================================================================

test_that("enr_grade_aggs produces K8, HS, K12 only", {
  # Create test data with all grade levels
  test_data <- data.frame(
    end_year = rep(2024, 14),
    type = rep("District", 14),
    district_id = rep("0001-01", 14),
    district_name = rep("Test District", 14),
    campus_id = rep(NA_character_, 14),
    campus_name = rep(NA_character_, 14),
    county = rep("Test County", 14),
    subgroup = rep("total_enrollment", 14),
    grade_level = c("K", "01", "02", "03", "04", "05", "06", "07", "08",
                    "09", "10", "11", "12", "TOTAL"),
    n_students = c(100, 95, 90, 85, 80, 75, 70, 65, 60,
                   55, 50, 45, 40, 1000),
    pct = rep(NA_real_, 14),
    is_state = rep(FALSE, 14),
    is_district = rep(TRUE, 14),
    is_campus = rep(FALSE, 14),
    stringsAsFactors = FALSE
  )

  aggs <- enr_grade_aggs(test_data)

  # Should only have K8, HS, K12
  agg_grades <- unique(aggs$grade_level)
  expect_true(all(c("K8", "HS", "K12") %in% agg_grades))
  expect_equal(length(agg_grades), 3)
})

test_that("enr_grade_aggs K8 sum is correct", {
  test_data <- data.frame(
    end_year = rep(2024, 9),
    type = rep("District", 9),
    district_id = rep("0001-01", 9),
    district_name = rep("Test", 9),
    campus_id = rep(NA_character_, 9),
    campus_name = rep(NA_character_, 9),
    county = rep("Test", 9),
    subgroup = rep("total_enrollment", 9),
    grade_level = c("K", "01", "02", "03", "04", "05", "06", "07", "08"),
    n_students = c(100, 100, 100, 100, 100, 100, 100, 100, 100),
    pct = rep(NA_real_, 9),
    is_state = rep(FALSE, 9),
    is_district = rep(TRUE, 9),
    is_campus = rep(FALSE, 9),
    stringsAsFactors = FALSE
  )

  aggs <- enr_grade_aggs(test_data)

  k8_row <- aggs[aggs$grade_level == "K8", ]
  expect_equal(k8_row$n_students, 900)
})

test_that("enr_grade_aggs excludes PK from K12", {
  test_data <- data.frame(
    end_year = rep(2024, 3),
    type = rep("District", 3),
    district_id = rep("0001-01", 3),
    district_name = rep("Test", 3),
    campus_id = rep(NA_character_, 3),
    campus_name = rep(NA_character_, 3),
    county = rep("Test", 3),
    subgroup = rep("total_enrollment", 3),
    grade_level = c("PK", "K", "01"),
    n_students = c(50, 100, 100),
    pct = rep(NA_real_, 3),
    is_state = rep(FALSE, 3),
    is_district = rep(TRUE, 3),
    is_campus = rep(FALSE, 3),
    stringsAsFactors = FALSE
  )

  aggs <- enr_grade_aggs(test_data)

  k12_row <- aggs[aggs$grade_level == "K12", ]
  # K12 = K + 01 = 200, NOT 250 (PK excluded)
  expect_equal(k12_row$n_students, 200)
})

# ==============================================================================
# 9. Assessment Typology Guards
# ==============================================================================

test_that("assessment proficiency levels use standard names", {
  # Check the level names used in tidy_assessment
  empty <- mnschooldata:::create_empty_assessment_result(2024)

  # Add a minimal test row
  test_df <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    district_name = NA_character_,
    school_id = NA_character_,
    school_name = NA_character_,
    test = "MCA",
    subject = "Math",
    grade = "All",
    subgroup = "All Students",
    n_tested = 100,
    n_does_not_meet = 25,
    n_partially_meets = 25,
    n_meets = 30,
    n_exceeds = 20,
    pct_does_not_meet = 25,
    pct_partially_meets = 25,
    pct_meets = 30,
    pct_exceeds = 20,
    pct_proficient = 50,
    stringsAsFactors = FALSE
  )

  tidy <- tidy_assessment(test_df)

  expected_levels <- c("does_not_meet", "partially_meets", "meets", "exceeds")
  actual_levels <- sort(unique(tidy$proficiency_level))

  expect_equal(actual_levels, sort(expected_levels))
})

test_that("assessment entity flags match enrollment convention", {
  # Assessment uses is_state, is_district, is_school
  test_df <- data.frame(
    end_year = c(2024, 2024, 2024),
    type = c("State", "District", "School"),
    subject = c("Math", "Math", "Math"),
    stringsAsFactors = FALSE
  )

  result <- id_assessment_aggs(test_df)

  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_school" %in% names(result))

  # Verify flags are correct
  expect_true(result$is_state[1])
  expect_true(result$is_district[2])
  expect_true(result$is_school[3])
})

test_that("assessment tidy pct is normalized to 0-1 range", {
  # MDE raw data uses 0-100 range; tidy should normalize to 0-1
  test_df <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    district_name = NA_character_,
    school_id = NA_character_,
    school_name = NA_character_,
    test = "MCA",
    subject = "Math",
    grade = "All",
    subgroup = "All Students",
    n_tested = 100,
    n_does_not_meet = 25,
    n_partially_meets = 25,
    n_meets = 30,
    n_exceeds = 20,
    pct_does_not_meet = 25,
    pct_partially_meets = 25,
    pct_meets = 30,
    pct_exceeds = 20,
    pct_proficient = 50,
    stringsAsFactors = FALSE
  )

  tidy <- tidy_assessment(test_df)

  # All pct values should be in [0, 1]
  non_na_pct <- tidy$pct[!is.na(tidy$pct)]
  expect_true(all(non_na_pct >= 0), info = "Assessment pct should be >= 0")
  expect_true(all(non_na_pct <= 1), info = "Assessment pct should be <= 1 (not 0-100)")

  # Specifically check: 25% should become 0.25
  meets_pct <- tidy[tidy$proficiency_level == "meets", ]$pct
  expect_equal(meets_pct, 0.30, tolerance = 0.01)
})

# ==============================================================================
# 10. Cross-Function Consistency Guards
# ==============================================================================

test_that("fetch_enr tidy=TRUE is equivalent to fetch_enr tidy=FALSE + tidy_enr", {
  skip_on_cran()
  skip_if_offline()

  # Method 1: Direct tidy
  tidy_direct <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)

  # Method 2: Wide then tidy
  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)
  tidy_manual <- tidy_enr(wide) |> id_enr_aggs()

  # Should have same columns (order may differ)
  expect_true(all(names(tidy_manual) %in% names(tidy_direct)),
              info = "Manual tidy columns should be subset of direct tidy columns")

  # Should have same state total
  direct_total <- tidy_direct[tidy_direct$is_state &
                              tidy_direct$subgroup == "total_enrollment" &
                              tidy_direct$grade_level == "TOTAL", ]$n_students

  manual_total <- tidy_manual[tidy_manual$is_state &
                              tidy_manual$subgroup == "total_enrollment" &
                              tidy_manual$grade_level == "TOTAL", ]$n_students

  expect_equal(direct_total, manual_total)
})

test_that("get_available_years range contains all bundled data years", {
  years <- get_available_years()

  data(enr_2023_example)
  data(enr_multi_example)

  bundled_years <- unique(c(
    unique(enr_2023_example$end_year),
    unique(enr_multi_example$end_year)
  ))

  for (yr in bundled_years) {
    expect_true(yr %in% years,
                info = paste("Bundled year", yr, "should be in get_available_years"))
  }
})
