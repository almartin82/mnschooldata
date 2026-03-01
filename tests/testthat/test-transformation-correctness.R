# ==============================================================================
# Transformation Correctness Tests for mnschooldata
# ==============================================================================
#
# These tests verify that every transformation in the pipeline preserves data
# fidelity and produces correct output. Tests use synthetic wide data frames
# (not fabricated state data -- these are test fixtures) to exercise the
# tidy_enr(), id_enr_aggs(), enr_grade_aggs(), process_enr(),
# create_state_aggregate(), tidy_assessment(), id_assessment_aggs(),
# calc_proficiency(), and assessment_summary() transformations.
#
# Bug discoveries are documented inline with "BUG:" markers.
#
# ==============================================================================

library(testthat)


# ==============================================================================
# Helper: Build a canonical wide enrollment row
# ==============================================================================

make_wide_enr <- function(
  end_year = 2024,
  type = "Campus",
  district_id = "000001",
  campus_id = "000001001",
  district_name = "Test District",
  campus_name = "Test School",
  county = "Test County",
  row_total = 500,
  white = 250, black = 100, hispanic = 75, asian = 30,

native_american = 15, pacific_islander = 10, multiracial = 20,
  male = 260, female = 240,
  special_ed = 50, lep = 60, econ_disadv = 200, homeless = 8,
  grade_pk = 30, grade_k = 40, grade_01 = 38, grade_02 = 37,
  grade_03 = 36, grade_04 = 35, grade_05 = 34, grade_06 = 33,
  grade_07 = 32, grade_08 = 31, grade_09 = 36, grade_10 = 35,
  grade_11 = 33, grade_12 = 30
) {
  data.frame(
    end_year = end_year,
    type = type,
    district_id = district_id,
    campus_id = campus_id,
    district_name = district_name,
    campus_name = campus_name,
    county = county,
    row_total = row_total,
    white = white, black = black, hispanic = hispanic, asian = asian,
    native_american = native_american, pacific_islander = pacific_islander,
    multiracial = multiracial,
    male = male, female = female,
    special_ed = special_ed, lep = lep, econ_disadv = econ_disadv,
    homeless = homeless,
    grade_pk = grade_pk, grade_k = grade_k,
    grade_01 = grade_01, grade_02 = grade_02,
    grade_03 = grade_03, grade_04 = grade_04,
    grade_05 = grade_05, grade_06 = grade_06,
    grade_07 = grade_07, grade_08 = grade_08,
    grade_09 = grade_09, grade_10 = grade_10,
    grade_11 = grade_11, grade_12 = grade_12,
    stringsAsFactors = FALSE
  )
}

make_wide_assess <- function(
  end_year = 2024,
  type = "State",
  district_id = NA_character_,
  district_name = NA_character_,
  school_id = NA_character_,
  school_name = NA_character_,
  test = "MCA",
  subject = "Math",
  grade = "03",
  subgroup = "All Students",
  n_tested = 100,
  n_does_not_meet = 20,
  n_partially_meets = 30,
  n_meets = 35,
  n_exceeds = 15,
  pct_does_not_meet = 20.0,
  pct_partially_meets = 30.0,
  pct_meets = 35.0,
  pct_exceeds = 15.0,
  pct_proficient = 50.0
) {
  data.frame(
    end_year = end_year,
    type = type,
    district_id = district_id,
    district_name = district_name,
    school_id = school_id,
    school_name = school_name,
    test = test,
    subject = subject,
    grade = grade,
    subgroup = subgroup,
    n_tested = n_tested,
    n_does_not_meet = n_does_not_meet,
    n_partially_meets = n_partially_meets,
    n_meets = n_meets,
    n_exceeds = n_exceeds,
    pct_does_not_meet = pct_does_not_meet,
    pct_partially_meets = pct_partially_meets,
    pct_meets = pct_meets,
    pct_exceeds = pct_exceeds,
    pct_proficient = pct_proficient,
    stringsAsFactors = FALSE
  )
}


# ==============================================================================
# SECTION 1: safe_numeric() correctness
# ==============================================================================

test_that("safe_numeric converts valid numeric strings", {
  expect_equal(safe_numeric("0"), 0)
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("1,234,567"), 1234567)
  expect_equal(safe_numeric("3.14"), 3.14)
  expect_equal(safe_numeric("0.0"), 0)
})

test_that("safe_numeric handles whitespace", {
  expect_equal(safe_numeric("  100  "), 100)
  expect_equal(safe_numeric("\t200\t"), 200)
})

test_that("safe_numeric converts suppression markers to NA", {
  markers <- c("*", ".", "-", "-1", "<5", "<10", "N/A", "NA", "", "NULL")
  for (m in markers) {
    expect_true(is.na(safe_numeric(m)),
                info = paste("Suppression marker not converted:", m))
  }
})

test_that("safe_numeric handles range markers", {
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("<10")))
  expect_true(is.na(safe_numeric(">95")))
  expect_true(is.na(safe_numeric("<3")))
})

test_that("safe_numeric handles NA inputs", {
  expect_true(is.na(safe_numeric(NA)))
  expect_true(is.na(safe_numeric(NA_character_)))
})

test_that("safe_numeric does not produce Inf or NaN", {
  test_values <- c("0", "1", "1000000", "*", "", "NA", "<5", ">95", ".")
  results <- sapply(test_values, safe_numeric)
  expect_false(any(is.infinite(results), na.rm = TRUE))
  expect_false(any(is.nan(results), na.rm = TRUE))
})


# ==============================================================================
# SECTION 2: tidy_enr() count fidelity
# ==============================================================================

test_that("tidy_enr preserves total_enrollment count exactly", {
  wide <- make_wide_enr(row_total = 12345)
  tidy <- tidy_enr(wide)

  total_row <- tidy[tidy$subgroup == "total_enrollment" &
                    tidy$grade_level == "TOTAL", ]
  expect_equal(nrow(total_row), 1)
  expect_equal(total_row$n_students, 12345)
})

test_that("tidy_enr preserves all demographic counts exactly", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  demo_fields <- c("white", "black", "hispanic", "asian",
                    "native_american", "pacific_islander", "multiracial")
  for (field in demo_fields) {
    tidy_row <- tidy[tidy$subgroup == field & tidy$grade_level == "TOTAL", ]
    expect_equal(nrow(tidy_row), 1,
                 info = paste("Expected exactly 1 row for", field))
    expect_equal(tidy_row$n_students, wide[[field]],
                 info = paste("Count mismatch for", field))
  }
})

test_that("tidy_enr preserves special population counts exactly", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  special_fields <- c("special_ed", "lep", "econ_disadv", "homeless")
  for (field in special_fields) {
    tidy_row <- tidy[tidy$subgroup == field & tidy$grade_level == "TOTAL", ]
    expect_equal(nrow(tidy_row), 1,
                 info = paste("Expected exactly 1 row for", field))
    expect_equal(tidy_row$n_students, wide[[field]],
                 info = paste("Count mismatch for", field))
  }
})

test_that("tidy_enr preserves grade-level counts exactly", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  grade_map <- c(
    "grade_pk" = "PK", "grade_k" = "K",
    "grade_01" = "01", "grade_02" = "02", "grade_03" = "03",
    "grade_04" = "04", "grade_05" = "05", "grade_06" = "06",
    "grade_07" = "07", "grade_08" = "08", "grade_09" = "09",
    "grade_10" = "10", "grade_11" = "11", "grade_12" = "12"
  )

  for (wide_col in names(grade_map)) {
    gl <- grade_map[wide_col]
    tidy_row <- tidy[tidy$grade_level == gl &
                     tidy$subgroup == "total_enrollment", ]
    expect_equal(nrow(tidy_row), 1,
                 info = paste("Expected exactly 1 row for grade", gl))
    expect_equal(tidy_row$n_students, wide[[wide_col]],
                 info = paste("Count mismatch for grade", gl))
  }
})


# ==============================================================================
# SECTION 3: tidy_enr() percentage correctness
# ==============================================================================

test_that("tidy_enr computes correct demographic percentages", {
  wide <- make_wide_enr(row_total = 1000, white = 600, black = 200,
                        hispanic = 100, asian = 50,
                        native_american = 20, pacific_islander = 10,
                        multiracial = 20)
  tidy <- tidy_enr(wide)

  expect_equal(
    tidy$pct[tidy$subgroup == "white" & tidy$grade_level == "TOTAL"],
    0.6
  )
  expect_equal(
    tidy$pct[tidy$subgroup == "black" & tidy$grade_level == "TOTAL"],
    0.2
  )
  expect_equal(
    tidy$pct[tidy$subgroup == "hispanic" & tidy$grade_level == "TOTAL"],
    0.1
  )
})

test_that("tidy_enr total_enrollment has pct = 1.0", {
  wide <- make_wide_enr(row_total = 500)
  tidy <- tidy_enr(wide)

  total_row <- tidy[tidy$subgroup == "total_enrollment" &
                    tidy$grade_level == "TOTAL", ]
  expect_equal(total_row$pct, 1.0)
})

test_that("tidy_enr percentages are in 0-1 range", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  valid_pct <- tidy$pct[!is.na(tidy$pct)]
  expect_true(all(valid_pct >= 0), info = "All pct should be >= 0")
  expect_true(all(valid_pct <= 1), info = "All pct should be <= 1")
})

test_that("tidy_enr grade-level pct computed as grade_count / row_total", {
  wide <- make_wide_enr(row_total = 200, grade_k = 40, grade_01 = 60)
  tidy <- tidy_enr(wide)

  k_row <- tidy[tidy$grade_level == "K" &
                tidy$subgroup == "total_enrollment", ]
  expect_equal(k_row$pct, 40 / 200)

  g01_row <- tidy[tidy$grade_level == "01" &
                  tidy$subgroup == "total_enrollment", ]
  expect_equal(g01_row$pct, 60 / 200)
})


# ==============================================================================
# SECTION 4: tidy_enr() edge cases
# ==============================================================================

test_that("tidy_enr handles row_total = 0 without NaN", {
  # BUG: When row_total is 0, pct = n_students/row_total = 0/0 = NaN
  # for subgroups and grades. This test documents the expected behavior:
  # pct should be NA or 0 when row_total is 0, never NaN.
  wide <- make_wide_enr(row_total = 0, white = 0, black = 0,
                        hispanic = 0, asian = 0,
                        native_american = 0, pacific_islander = 0,
                        multiracial = 0, male = 0, female = 0,
                        special_ed = 0, lep = 0, econ_disadv = 0,
                        homeless = 0, grade_pk = 0, grade_k = 0,
                        grade_01 = 0, grade_02 = 0, grade_03 = 0,
                        grade_04 = 0, grade_05 = 0, grade_06 = 0,
                        grade_07 = 0, grade_08 = 0, grade_09 = 0,
                        grade_10 = 0, grade_11 = 0, grade_12 = 0)
  tidy <- tidy_enr(wide)

  # NaN is never acceptable in output
  expect_false(any(is.nan(tidy$pct)),
               info = "BUG: pct should not be NaN when row_total is 0")
  expect_false(any(is.nan(tidy$n_students)),
               info = "n_students should not be NaN")
})

test_that("tidy_enr handles NA demographic counts", {
  wide <- make_wide_enr()
  wide$white <- NA
  wide$grade_k <- NA
  tidy <- tidy_enr(wide)

  # NA counts should be filtered out (tidy_enr filters !is.na(n_students))
  white_rows <- tidy[tidy$subgroup == "white" & tidy$grade_level == "TOTAL", ]
  expect_equal(nrow(white_rows), 0,
               info = "NA subgroup counts should be filtered out")

  k_rows <- tidy[tidy$grade_level == "K" &
                 tidy$subgroup == "total_enrollment", ]
  expect_equal(nrow(k_rows), 0,
               info = "NA grade counts should be filtered out")
})

test_that("tidy_enr handles single-row input", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  # Should have rows for: total + 7 races + 2 genders + 4 special +
  # 14 grades = 28 rows (for a single wide row with all columns)
  expect_true(nrow(tidy) > 20,
              info = "Single wide row should produce many tidy rows")
})

test_that("tidy_enr handles multi-row input", {
  wide <- dplyr::bind_rows(
    make_wide_enr(district_id = "0001", campus_id = "0001001"),
    make_wide_enr(district_id = "0002", campus_id = "0002001")
  )
  tidy <- tidy_enr(wide)

  # Both entities should be present
  expect_true("0001" %in% tidy$district_id)
  expect_true("0002" %in% tidy$district_id)

  # Each entity should have the same number of rows
  n1 <- sum(tidy$district_id == "0001")
  n2 <- sum(tidy$district_id == "0002")
  expect_equal(n1, n2)
})


# ==============================================================================
# SECTION 5: tidy_enr() structural correctness
# ==============================================================================

test_that("demographic subgroups always have grade_level = TOTAL", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  demo_rows <- tidy[tidy$subgroup %in% c("white", "black", "hispanic",
                                          "asian", "native_american",
                                          "pacific_islander", "multiracial",
                                          "special_ed", "lep", "econ_disadv",
                                          "homeless"), ]
  expect_true(all(demo_rows$grade_level == "TOTAL"),
              info = "All demographic subgroups should have grade_level = TOTAL")
})

test_that("grade-level rows always have subgroup = total_enrollment", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  grade_rows <- tidy[tidy$grade_level %in% c("PK", "K", "01", "02", "03",
                                              "04", "05", "06", "07", "08",
                                              "09", "10", "11", "12"), ]
  expect_true(all(grade_rows$subgroup == "total_enrollment"),
              info = "All grade-level rows should have subgroup = total_enrollment")
})

test_that("tidy_enr output has no duplicate subgroup-grade combinations per entity", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  dup_check <- tidy |>
    dplyr::count(district_id, campus_id, subgroup, grade_level) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dup_check), 0,
               info = "No duplicate subgroup-grade_level combinations per entity")
})

test_that("tidy_enr preserves all invariant columns", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  invariants <- c("end_year", "type", "district_id", "campus_id",
                  "district_name", "campus_name", "county")

  for (col in invariants) {
    if (col %in% names(wide)) {
      expect_true(col %in% names(tidy),
                  info = paste("Invariant column missing from tidy:", col))
    }
  }
})

test_that("tidy_enr output columns include subgroup, grade_level, n_students, pct", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  required_cols <- c("subgroup", "grade_level", "n_students", "pct")
  for (col in required_cols) {
    expect_true(col %in% names(tidy),
                info = paste("Required column missing:", col))
  }
})


# ==============================================================================
# SECTION 6: id_enr_aggs() correctness
# ==============================================================================

test_that("id_enr_aggs correctly flags State records", {
  tidy <- data.frame(
    type = c("State", "District", "Campus"),
    district_id = c(NA, "0001", "0001"),
    campus_id = c(NA, NA, "001"),
    stringsAsFactors = FALSE
  )
  result <- id_enr_aggs(tidy)

  expect_true(result$is_state[1])
  expect_false(result$is_district[1])
  expect_false(result$is_campus[1])
})

test_that("id_enr_aggs correctly flags District records", {
  tidy <- data.frame(
    type = c("State", "District", "Campus"),
    district_id = c(NA, "0001", "0001"),
    campus_id = c(NA, NA, "001"),
    stringsAsFactors = FALSE
  )
  result <- id_enr_aggs(tidy)

  expect_false(result$is_state[2])
  expect_true(result$is_district[2])
  expect_false(result$is_campus[2])
})

test_that("id_enr_aggs correctly flags Campus records", {
  tidy <- data.frame(
    type = c("State", "District", "Campus"),
    district_id = c(NA, "0001", "0001"),
    campus_id = c(NA, NA, "001"),
    stringsAsFactors = FALSE
  )
  result <- id_enr_aggs(tidy)

  expect_false(result$is_state[3])
  expect_false(result$is_district[3])
  expect_true(result$is_campus[3])
})

test_that("id_enr_aggs flags are mutually exclusive", {
  wide <- dplyr::bind_rows(
    make_wide_enr(type = "State", district_id = NA, campus_id = NA),
    make_wide_enr(type = "District", campus_id = NA),
    make_wide_enr(type = "Campus")
  )
  tidy <- tidy_enr(wide) |> id_enr_aggs()

  type_sums <- as.integer(tidy$is_state) +
               as.integer(tidy$is_district) +
               as.integer(tidy$is_campus)
  expect_true(all(type_sums == 1),
              info = "Each row should be exactly one of state/district/campus")
})

test_that("id_enr_aggs adds aggregation_flag column", {
  wide <- dplyr::bind_rows(
    make_wide_enr(type = "State", district_id = NA, campus_id = NA),
    make_wide_enr(type = "District", campus_id = NA),
    make_wide_enr(type = "Campus")
  )
  tidy <- tidy_enr(wide) |> id_enr_aggs()

  expect_true("aggregation_flag" %in% names(tidy))
  expect_true(all(tidy$aggregation_flag %in% c("state", "district", "campus")))
})


# ==============================================================================
# SECTION 7: enr_grade_aggs() correctness
# ==============================================================================

test_that("enr_grade_aggs K8 sum is correct", {
  tidy <- data.frame(
    end_year = rep(2024, 10),
    type = rep("District", 10),
    district_id = rep("0001", 10),
    district_name = rep("Test", 10),
    campus_id = rep(NA_character_, 10),
    campus_name = rep(NA_character_, 10),
    county = rep("Test", 10),
    subgroup = rep("total_enrollment", 10),
    grade_level = c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09"),
    n_students = c(100, 90, 85, 80, 75, 70, 65, 60, 55, 50),
    pct = rep(0.1, 10),
    is_state = rep(FALSE, 10),
    is_district = rep(TRUE, 10),
    is_campus = rep(FALSE, 10)
  )

  aggs <- enr_grade_aggs(tidy)
  k8 <- aggs[aggs$grade_level == "K8", ]

  # K8 = K + 01-08 = 100+90+85+80+75+70+65+60+55 = 680
  expect_equal(k8$n_students, 680)
})

test_that("enr_grade_aggs HS sum is correct", {
  tidy <- data.frame(
    end_year = rep(2024, 4),
    type = rep("District", 4),
    district_id = rep("0001", 4),
    district_name = rep("Test", 4),
    campus_id = rep(NA_character_, 4),
    campus_name = rep(NA_character_, 4),
    county = rep("Test", 4),
    subgroup = rep("total_enrollment", 4),
    grade_level = c("09", "10", "11", "12"),
    n_students = c(50, 45, 40, 35),
    pct = rep(0.1, 4),
    is_state = rep(FALSE, 4),
    is_district = rep(TRUE, 4),
    is_campus = rep(FALSE, 4)
  )

  aggs <- enr_grade_aggs(tidy)
  hs <- aggs[aggs$grade_level == "HS", ]

  expect_equal(hs$n_students, 170)
})

test_that("enr_grade_aggs K12 excludes PK", {
  tidy <- data.frame(
    end_year = rep(2024, 3),
    type = rep("District", 3),
    district_id = rep("0001", 3),
    district_name = rep("Test", 3),
    campus_id = rep(NA_character_, 3),
    campus_name = rep(NA_character_, 3),
    county = rep("Test", 3),
    subgroup = rep("total_enrollment", 3),
    grade_level = c("PK", "K", "01"),
    n_students = c(50, 100, 90),
    pct = rep(0.1, 3),
    is_state = rep(FALSE, 3),
    is_district = rep(TRUE, 3),
    is_campus = rep(FALSE, 3)
  )

  aggs <- enr_grade_aggs(tidy)
  k12 <- aggs[aggs$grade_level == "K12", ]

  # K12 = K + 01 = 190 (PK excluded)
  expect_equal(k12$n_students, 190)
})

test_that("enr_grade_aggs only includes total_enrollment subgroup", {
  tidy <- data.frame(
    end_year = rep(2024, 4),
    type = rep("District", 4),
    district_id = rep("0001", 4),
    district_name = rep("Test", 4),
    campus_id = rep(NA_character_, 4),
    campus_name = rep(NA_character_, 4),
    county = rep("Test", 4),
    subgroup = c("total_enrollment", "total_enrollment", "white", "white"),
    grade_level = c("K", "01", "K", "01"),
    n_students = c(100, 90, 50, 45),
    pct = rep(0.1, 4),
    is_state = rep(FALSE, 4),
    is_district = rep(TRUE, 4),
    is_campus = rep(FALSE, 4)
  )

  aggs <- enr_grade_aggs(tidy)

  # Grade aggregates should only use total_enrollment
  expect_true(all(aggs$subgroup == "total_enrollment"))

  k8 <- aggs[aggs$grade_level == "K8", ]
  expect_equal(k8$n_students, 190)  # 100 + 90, not 100+90+50+45
})

test_that("enr_grade_aggs produces all three aggregation levels", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide) |> id_enr_aggs()
  aggs <- enr_grade_aggs(tidy)

  expect_true("K8" %in% aggs$grade_level)
  expect_true("HS" %in% aggs$grade_level)
  expect_true("K12" %in% aggs$grade_level)
})

test_that("enr_grade_aggs K8 + HS = K12", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide) |> id_enr_aggs()
  aggs <- enr_grade_aggs(tidy)

  k8 <- aggs$n_students[aggs$grade_level == "K8"]
  hs <- aggs$n_students[aggs$grade_level == "HS"]
  k12 <- aggs$n_students[aggs$grade_level == "K12"]

  expect_equal(k8 + hs, k12,
               info = "K8 + HS should equal K12")
})


# ==============================================================================
# SECTION 8: create_state_aggregate() correctness
# ==============================================================================

test_that("create_state_aggregate sums district totals", {
  districts <- dplyr::bind_rows(
    make_wide_enr(type = "District", district_id = "0001", row_total = 1000,
                  white = 500, black = 200, campus_id = NA),
    make_wide_enr(type = "District", district_id = "0002", row_total = 2000,
                  white = 1000, black = 400, campus_id = NA)
  )

  state <- create_state_aggregate(districts, 2024)

  expect_equal(state$row_total, 3000)
  expect_equal(state$white, 1500)
  expect_equal(state$black, 600)
  expect_equal(state$type, "State")
  expect_equal(state$end_year, 2024)
})

test_that("create_state_aggregate handles NA values in sums", {
  districts <- dplyr::bind_rows(
    make_wide_enr(type = "District", district_id = "0001", row_total = 1000,
                  white = 500, campus_id = NA),
    make_wide_enr(type = "District", district_id = "0002", row_total = 2000,
                  white = NA, campus_id = NA)
  )
  districts$white[2] <- NA

  state <- create_state_aggregate(districts, 2024)

  # na.rm = TRUE so should use the non-NA value
  expect_equal(state$row_total, 3000)
  expect_equal(state$white, 500)
})

test_that("create_state_aggregate sets identifiers to NA", {
  districts <- make_wide_enr(type = "District", campus_id = NA)
  state <- create_state_aggregate(districts, 2024)

  expect_true(is.na(state$district_id))
  expect_true(is.na(state$campus_id))
  expect_true(is.na(state$district_name))
  expect_true(is.na(state$campus_name))
  expect_true(is.na(state$county))
})

test_that("create_state_aggregate returns empty for empty input", {
  result <- create_state_aggregate(data.frame(), 2024)
  expect_equal(nrow(result), 0)
})


# ==============================================================================
# SECTION 9: Wide-tidy round-trip fidelity (cached data)
# ==============================================================================

test_that("tidy total_enrollment matches wide row_total for all entities", {
  skip_if(!file.exists(
    file.path(mnschooldata:::get_cache_dir(), "enr_wide_2023.rds")),
    "No cached 2023 wide data available"
  )

  wide <- readRDS(file.path(mnschooldata:::get_cache_dir(), "enr_wide_2023.rds"))
  tidy <- readRDS(file.path(mnschooldata:::get_cache_dir(), "enr_tidy_2023.rds"))

  tidy_totals <- tidy[tidy$subgroup == "total_enrollment" &
                      tidy$grade_level == "TOTAL",
                      c("district_id", "type", "n_students")]
  names(tidy_totals)[3] <- "n_tidy"

  wide_totals <- wide[, c("district_id", "type", "row_total")]
  names(wide_totals)[3] <- "n_wide"

  merged <- merge(tidy_totals, wide_totals, by = c("district_id", "type"))

  mismatches <- merged[merged$n_tidy != merged$n_wide, ]
  expect_equal(nrow(mismatches), 0,
               info = paste("Found", nrow(mismatches),
                            "entities where tidy total != wide row_total"))
})

test_that("tidy race subgroup counts match wide columns for all entities", {
  skip_if(!file.exists(
    file.path(mnschooldata:::get_cache_dir(), "enr_wide_2023.rds")),
    "No cached 2023 wide data available"
  )

  wide <- readRDS(file.path(mnschooldata:::get_cache_dir(), "enr_wide_2023.rds"))
  tidy <- readRDS(file.path(mnschooldata:::get_cache_dir(), "enr_tidy_2023.rds"))

  races <- c("white", "black", "hispanic", "asian",
             "native_american", "pacific_islander", "multiracial")

  for (race in races) {
    if (!race %in% names(wide)) next

    tidy_race <- tidy[tidy$subgroup == race & tidy$grade_level == "TOTAL",
                      c("district_id", "type", "n_students")]
    names(tidy_race)[3] <- "n_tidy"

    wide_race <- wide[, c("district_id", "type", race)]
    names(wide_race)[3] <- "n_wide"

    merged <- merge(tidy_race, wide_race, by = c("district_id", "type"))
    mismatches <- merged[!is.na(merged$n_tidy) & !is.na(merged$n_wide) &
                         merged$n_tidy != merged$n_wide, ]

    expect_equal(nrow(mismatches), 0,
                 info = paste("Mismatch in", race, "counts:",
                              nrow(mismatches), "entities"))
  }
})

test_that("race subgroups sum to total enrollment for every entity", {
  skip_if(!file.exists(
    file.path(mnschooldata:::get_cache_dir(), "enr_tidy_2023.rds")),
    "No cached 2023 tidy data available"
  )

  tidy <- readRDS(file.path(mnschooldata:::get_cache_dir(), "enr_tidy_2023.rds"))

  races <- c("white", "black", "hispanic", "asian",
             "native_american", "pacific_islander", "multiracial")

  race_sums <- tidy[tidy$grade_level == "TOTAL" &
                    tidy$subgroup %in% races, ] |>
    dplyr::group_by(district_id, type) |>
    dplyr::summarize(race_sum = sum(n_students, na.rm = TRUE), .groups = "drop")

  totals <- tidy[tidy$subgroup == "total_enrollment" &
                 tidy$grade_level == "TOTAL",
                 c("district_id", "type", "n_students")]
  names(totals)[3] <- "total"

  check <- merge(race_sums, totals, by = c("district_id", "type"))
  mismatches <- check[check$race_sum != check$total, ]

  expect_equal(nrow(mismatches), 0,
               info = paste("Race subgroups don't sum to total for",
                            nrow(mismatches), "entities"))
})


# ==============================================================================
# SECTION 10: CLAUDE.md documented subgroups vs actual data
# ==============================================================================

test_that("CLAUDE.md documented subgroups match code expectations", {
  # CLAUDE.md documents these subgroups:
  # total_enrollment, white, black, hispanic, asian, native_american,
  # pacific_islander, multiracial, special_ed, lep, econ_disadv, homeless
  # (male and female documented as NOT in tidy enrollment)

  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)
  actual_subgroups <- unique(tidy$subgroup)

  documented_subgroups <- c("total_enrollment", "white", "black", "hispanic",
                            "asian", "native_american", "pacific_islander",
                            "multiracial", "special_ed", "lep", "econ_disadv",
                            "homeless")

  for (sg in documented_subgroups) {
    expect_true(sg %in% actual_subgroups,
                info = paste("CLAUDE.md documented subgroup missing:", sg))
  }
})

test_that("CLAUDE.md documented grade levels match code expectations", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)
  actual_grades <- unique(tidy$grade_level)

  documented_grades <- c("PK", "K", "01", "02", "03", "04", "05", "06",
                         "07", "08", "09", "10", "11", "12", "TOTAL")

  for (gl in documented_grades) {
    expect_true(gl %in% actual_grades,
                info = paste("CLAUDE.md documented grade_level missing:", gl))
  }
})


# ==============================================================================
# SECTION 11: tidy_enr() gender handling
# ==============================================================================

test_that("tidy_enr includes male and female when present in wide data", {
  # CLAUDE.md says male/female are NOT in tidy, but the code includes them
  # in gender_cols. This test verifies the code behavior.
  wide <- make_wide_enr(male = 260, female = 240)
  tidy <- tidy_enr(wide)

  actual_subgroups <- unique(tidy$subgroup)

  # The code adds "male" and "female" to all_subgroups (gender_cols)
  # but CLAUDE.md says they are NOT included.
  # The code should prevail -- if both columns exist, they should be tidied.
  expect_true("male" %in% actual_subgroups,
              info = "male should be in tidy when present in wide")
  expect_true("female" %in% actual_subgroups,
              info = "female should be in tidy when present in wide")
})


# ==============================================================================
# SECTION 12: Assessment tidy_assessment() correctness
# ==============================================================================

test_that("tidy_assessment produces 4 proficiency levels", {
  wide <- make_wide_assess()
  tidy <- tidy_assessment(wide)

  expected_levels <- c("does_not_meet", "partially_meets", "meets", "exceeds")
  actual_levels <- sort(unique(tidy$proficiency_level))

  expect_equal(actual_levels, sort(expected_levels))
})

test_that("tidy_assessment preserves count values exactly", {
  wide <- make_wide_assess(
    n_does_not_meet = 25,
    n_partially_meets = 35,
    n_meets = 30,
    n_exceeds = 10
  )
  tidy <- tidy_assessment(wide)

  expect_equal(
    tidy$n_students[tidy$proficiency_level == "does_not_meet"], 25)
  expect_equal(
    tidy$n_students[tidy$proficiency_level == "partially_meets"], 35)
  expect_equal(
    tidy$n_students[tidy$proficiency_level == "meets"], 30)
  expect_equal(
    tidy$n_students[tidy$proficiency_level == "exceeds"], 10)
})

test_that("tidy_assessment normalizes pct from 0-100 to 0-1 range", {
  wide <- make_wide_assess(
    pct_does_not_meet = 25.0,
    pct_partially_meets = 35.0,
    pct_meets = 30.0,
    pct_exceeds = 10.0
  )
  tidy <- tidy_assessment(wide)

  expect_equal(tidy$pct[tidy$proficiency_level == "does_not_meet"], 0.25)
  expect_equal(tidy$pct[tidy$proficiency_level == "partially_meets"], 0.35)
  expect_equal(tidy$pct[tidy$proficiency_level == "meets"], 0.30)
  expect_equal(tidy$pct[tidy$proficiency_level == "exceeds"], 0.10)
})

test_that("tidy_assessment pct values sum to approximately 1.0", {
  wide <- make_wide_assess(
    pct_does_not_meet = 20.0,
    pct_partially_meets = 30.0,
    pct_meets = 35.0,
    pct_exceeds = 15.0
  )
  tidy <- tidy_assessment(wide)

  pct_sum <- sum(tidy$pct, na.rm = TRUE)
  expect_equal(pct_sum, 1.0, tolerance = 0.01)
})

test_that("tidy_assessment n_students sum equals n_tested", {
  wide <- make_wide_assess(
    n_tested = 200,
    n_does_not_meet = 40,
    n_partially_meets = 60,
    n_meets = 70,
    n_exceeds = 30
  )
  tidy <- tidy_assessment(wide)

  n_sum <- sum(tidy$n_students, na.rm = TRUE)
  expect_equal(n_sum, 200)
})

test_that("tidy_assessment preserves invariant columns", {
  wide <- make_wide_assess(
    end_year = 2024,
    type = "District",
    district_id = "0001",
    district_name = "Minneapolis",
    subject = "Math",
    grade = "03",
    subgroup = "All Students"
  )
  tidy <- tidy_assessment(wide)

  expect_true(all(tidy$end_year == 2024))
  expect_true(all(tidy$type == "District"))
  expect_true(all(tidy$district_id == "0001"))
  expect_true(all(tidy$district_name == "Minneapolis"))
  expect_true(all(tidy$subject == "Math"))
  expect_true(all(tidy$grade == "03"))
  expect_true(all(tidy$subgroup == "All Students"))
})

test_that("tidy_assessment handles empty input", {
  wide <- make_wide_assess()
  wide <- wide[0, ]  # empty
  tidy <- tidy_assessment(wide)

  expect_equal(nrow(tidy), 0)
  expect_true("proficiency_level" %in% names(tidy))
})

test_that("tidy_assessment handles n_tested = 0 without NaN/Inf", {
  wide <- make_wide_assess(
    n_tested = 0,
    n_does_not_meet = 0, n_partially_meets = 0,
    n_meets = 0, n_exceeds = 0,
    pct_does_not_meet = 0, pct_partially_meets = 0,
    pct_meets = 0, pct_exceeds = 0
  )
  tidy <- tidy_assessment(wide)

  expect_false(any(is.nan(tidy$pct), na.rm = TRUE))
  expect_false(any(is.infinite(tidy$pct), na.rm = TRUE))
  expect_false(any(is.nan(tidy$n_students), na.rm = TRUE))
  expect_false(any(is.infinite(tidy$n_students), na.rm = TRUE))
})


# ==============================================================================
# SECTION 13: id_assessment_aggs() correctness
# ==============================================================================

test_that("id_assessment_aggs correctly identifies state records", {
  df <- data.frame(type = c("State", "District", "School"),
                   stringsAsFactors = FALSE)
  result <- id_assessment_aggs(df)

  expect_equal(result$is_state, c(TRUE, FALSE, FALSE))
  expect_equal(result$is_district, c(FALSE, TRUE, FALSE))
  expect_equal(result$is_school, c(FALSE, FALSE, TRUE))
})

test_that("id_assessment_aggs flags are mutually exclusive", {
  wide <- make_wide_assess()
  wide <- dplyr::bind_rows(
    make_wide_assess(type = "State"),
    make_wide_assess(type = "District", district_id = "0001"),
    make_wide_assess(type = "School", district_id = "0001", school_id = "001")
  )
  tidy <- tidy_assessment(wide)

  type_sums <- as.integer(tidy$is_state) +
               as.integer(tidy$is_district) +
               as.integer(tidy$is_school)
  expect_true(all(type_sums == 1))
})


# ==============================================================================
# SECTION 14: calc_proficiency() correctness
# ==============================================================================

test_that("calc_proficiency sums meets + exceeds correctly for single entity", {
  tidy <- data.frame(
    end_year = rep(2024, 4),
    type = rep("State", 4),
    district_id = rep(NA_character_, 4),
    school_id = rep(NA_character_, 4),
    test = rep("MCA", 4),
    subject = rep("Math", 4),
    grade = rep("03", 4),
    subgroup = rep("All Students", 4),
    n_tested = rep(100, 4),
    proficiency_level = c("does_not_meet", "partially_meets", "meets", "exceeds"),
    n_students = c(20, 30, 35, 15),
    pct = c(0.20, 0.30, 0.35, 0.15),
    is_state = rep(TRUE, 4),
    is_district = rep(FALSE, 4),
    is_school = rep(FALSE, 4),
    stringsAsFactors = FALSE
  )

  result <- calc_proficiency(tidy)
  expect_equal(result$n_proficient, 50)
  expect_equal(result$pct_proficient, 0.5)
})

test_that("calc_proficiency uses first() for n_tested (known behavior)", {
  # BUG: When grouping across multiple entities with different n_tested,
  # calc_proficiency uses first(n_tested) instead of sum().
  # This produces incorrect pct_proficient when entities have different n_tested.
  tidy <- data.frame(
    end_year = rep(2024, 8),
    type = rep("District", 8),
    district_id = rep(c("0001", "0002"), each = 4),
    school_id = rep(NA_character_, 8),
    test = rep("MCA", 8),
    subject = rep("Math", 8),
    grade = rep("03", 8),
    subgroup = rep("All Students", 8),
    n_tested = c(rep(100, 4), rep(200, 4)),
    proficiency_level = rep(c("does_not_meet", "partially_meets",
                              "meets", "exceeds"), 2),
    n_students = c(20, 30, 35, 15,    # District A: 50/100 proficient
                   40, 60, 70, 30),    # District B: 100/200 proficient
    pct = c(0.20, 0.30, 0.35, 0.15,
            0.20, 0.30, 0.35, 0.15),
    is_state = rep(FALSE, 8),
    is_district = rep(TRUE, 8),
    is_school = rep(FALSE, 8),
    stringsAsFactors = FALSE
  )

  # Grouped by subject -- combines both districts
  result <- calc_proficiency(tidy, subject)

  # n_proficient = (35+15) + (70+30) = 150 -- correct
  expect_equal(result$n_proficient, 150)

  # BUG: n_tested uses first() = 100, not sum() = 300
  # This makes pct_proficient = 150/100 = 1.5, which is > 1 (impossible)
  # The correct value should be 150/300 = 0.5
  # We document the current (buggy) behavior:
  expect_equal(result$n_tested, 100,
               info = paste("BUG: calc_proficiency uses first(n_tested)",
                            "instead of sum when grouping across entities.",
                            "This produces pct_proficient > 1.0"))
})


# ==============================================================================
# SECTION 15: assessment_summary() correctness
# ==============================================================================

test_that("assessment_summary groups by proficiency_level", {
  tidy <- data.frame(
    subject = rep("Math", 4),
    proficiency_level = c("does_not_meet", "partially_meets",
                          "meets", "exceeds"),
    n_students = c(20, 30, 35, 15),
    n_tested = rep(100, 4),
    stringsAsFactors = FALSE
  )

  result <- assessment_summary(tidy, subject)

  expect_equal(nrow(result), 4)
  expect_true("pct" %in% names(result))
})

test_that("assessment_summary requires tidy format", {
  df <- data.frame(subject = "Math", n_students = 100)
  expect_error(assessment_summary(df, subject), "tidy")
})


# ==============================================================================
# SECTION 16: Standardization function correctness
# ==============================================================================

test_that("standardize_test_name handles MCA-III correctly", {
  sttn <- mnschooldata:::standardize_test_name

  # BUG: "MCA III" and "MCA-III" incorrectly map to "MCA-II" because
  # the MCA-II regex pattern also matches strings containing "III".
  # The III pattern fires first, producing "MCA-III", but then the
  # II pattern also matches and overwrites to "MCA-II".
  expect_equal(sttn("MCA-III"), "MCA-III",
               info = paste("BUG: MCA-III should not be overwritten to MCA-II.",
                            "The gsub for MCA-II matches III because III contains II"))
})

test_that("standardize_test_name handles MCA-IV correctly", {
  sttn <- mnschooldata:::standardize_test_name
  expect_equal(sttn("MCA-IV"), "MCA-IV")
  expect_equal(sttn("MCA IV"), "MCA-IV")
})

test_that("standardize_test_name handles MTAS correctly", {
  sttn <- mnschooldata:::standardize_test_name
  expect_equal(sttn("MTAS"), "MTAS")
  expect_equal(sttn("Minnesota Test of Academic Skills"), "MTAS")
})

test_that("standardize_grade pads single-digit grades correctly", {
  sg <- mnschooldata:::standardize_grade

  # Grades 3-9 are padded by the regex ^([3-9])$ -> 0\\1
  expect_equal(sg("3"), "03")
  expect_equal(sg("9"), "09")

  # BUG: Grades 1 and 2 are NOT padded because the regex only covers [3-9]
  expect_equal(sg("1"), "01",
               info = paste("BUG: Grade 1 is not zero-padded.",
                            "The regex ^([3-9])$ misses 1 and 2"))
  expect_equal(sg("2"), "02",
               info = paste("BUG: Grade 2 is not zero-padded.",
                            "The regex ^([3-9])$ misses 1 and 2"))
})

test_that("standardize_grade handles ordinal formats", {
  sg <- mnschooldata:::standardize_grade
  expect_equal(sg("3rd"), "03")
  expect_equal(sg("4th"), "04")
  expect_equal(sg("10th"), "10")
})

test_that("standardize_grade handles 'All' and 'All Grades'", {
  sg <- mnschooldata:::standardize_grade
  expect_equal(sg("All Grades"), "All")
  expect_equal(sg("ALL"), "All")
})

test_that("standardize_grade handles already-padded grades", {
  sg <- mnschooldata:::standardize_grade
  expect_equal(sg("03"), "03")
  expect_equal(sg("10"), "10")
  expect_equal(sg("12"), "12")
})

test_that("standardize_subject normalizes Reading variants", {
  ss <- mnschooldata:::standardize_subject
  expect_equal(ss("Reading"), "Reading")
  expect_equal(ss("RLA"), "Reading")
  expect_equal(ss("ELA"), "Reading")
  expect_equal(ss("English Language Arts"), "Reading")
})

test_that("standardize_subject normalizes Math variants", {
  ss <- mnschooldata:::standardize_subject
  expect_equal(ss("Math"), "Math")
  expect_equal(ss("Mathematics"), "Math")
  expect_equal(ss("MATHEMATICS"), "Math")
})

test_that("standardize_subject normalizes Science", {
  ss <- mnschooldata:::standardize_subject
  expect_equal(ss("Science"), "Science")
  expect_equal(ss("SCI"), "Science")
})

test_that("standardize_subgroup maps all documented variants", {
  ss <- mnschooldata:::standardize_subgroup

  # All Students
  expect_equal(ss("All Students"), "All Students")
  expect_equal(ss("ALL STUDENTS"), "All Students")
  expect_equal(ss("All"), "All Students")
  expect_equal(ss("Total"), "All Students")

  # Race/ethnicity
  expect_equal(ss("American Indian or Alaska Native"), "Native American")
  expect_equal(ss("Black or African American"), "Black")
  expect_equal(ss("Hispanic or Latino"), "Hispanic")
  expect_equal(ss("Two or More Races"), "Multiracial")
  expect_equal(ss("Native Hawaiian or Other Pacific Islander"),
               "Pacific Islander")

  # Special populations
  expect_equal(ss("Students with disabilities"),
               "Students with Disabilities")
  expect_equal(ss("English learner"), "English Learners")
  expect_equal(ss("Eligible for free or reduced-price lunch"),
               "Economically Disadvantaged")
})

test_that("standardize_subgroup preserves unknown values", {
  ss <- mnschooldata:::standardize_subgroup
  expect_equal(ss("Some Unknown Group"), "Some Unknown Group")
})


# ==============================================================================
# SECTION 17: district_type and is_charter handling
# ==============================================================================

test_that("district_type column is preserved through tidy_enr", {
  # BUG: district_type exists in wide format but is NOT in the tidy_enr

  # invariants list, so it gets dropped during tidying.
  # CLAUDE.md documents is_charter as an entity flag, but it's never created
  # by id_enr_aggs(). The bundled example data has both district_type and
  # is_charter, but the live pipeline does not produce them.
  wide <- make_wide_enr()
  wide$district_type <- "07"  # charter

  tidy <- tidy_enr(wide)

  # This test documents the current behavior (district_type is dropped)
  # and will fail when the bug is fixed (which is the desired outcome).
  expect_true("district_type" %in% names(tidy),
              info = paste("BUG: district_type is dropped by tidy_enr",
                           "because it is not in the invariants list.",
                           "The bundled example data has it but live data does not."))
})


# ==============================================================================
# SECTION 18: No Inf/NaN in any output
# ==============================================================================

test_that("tidy_enr output has no Inf values", {
  wide <- make_wide_enr()
  tidy <- tidy_enr(wide)

  for (col in names(tidy)) {
    if (is.numeric(tidy[[col]])) {
      expect_false(any(is.infinite(tidy[[col]]), na.rm = TRUE),
                   info = paste("Inf found in", col))
    }
  }
})

test_that("tidy_assessment output has no Inf values", {
  wide <- make_wide_assess()
  tidy <- tidy_assessment(wide)

  for (col in names(tidy)) {
    if (is.numeric(tidy[[col]])) {
      expect_false(any(is.infinite(tidy[[col]]), na.rm = TRUE),
                   info = paste("Inf found in", col))
    }
  }
})


# ==============================================================================
# SECTION 19: Multi-row assessment transformations
# ==============================================================================

test_that("tidy_assessment handles multiple subjects", {
  wide <- dplyr::bind_rows(
    make_wide_assess(subject = "Math"),
    make_wide_assess(subject = "Reading")
  )
  tidy <- tidy_assessment(wide)

  expect_true("Math" %in% tidy$subject)
  expect_true("Reading" %in% tidy$subject)

  # Each subject should have 4 proficiency levels
  math_levels <- unique(tidy$proficiency_level[tidy$subject == "Math"])
  reading_levels <- unique(tidy$proficiency_level[tidy$subject == "Reading"])
  expect_equal(length(math_levels), 4)
  expect_equal(length(reading_levels), 4)
})

test_that("tidy_assessment handles multiple grades", {
  wide <- dplyr::bind_rows(
    make_wide_assess(grade = "03"),
    make_wide_assess(grade = "08")
  )
  tidy <- tidy_assessment(wide)

  expect_true("03" %in% tidy$grade)
  expect_true("08" %in% tidy$grade)
})

test_that("tidy_assessment handles multiple entity types", {
  wide <- dplyr::bind_rows(
    make_wide_assess(type = "State"),
    make_wide_assess(type = "District", district_id = "0001",
                     district_name = "Test"),
    make_wide_assess(type = "School", district_id = "0001",
                     school_id = "001", school_name = "Test School")
  )
  tidy <- tidy_assessment(wide)

  expect_true("State" %in% tidy$type)
  expect_true("District" %in% tidy$type)
  expect_true("School" %in% tidy$type)
})


# ==============================================================================
# SECTION 20: pct clamping in tidy_assessment
# ==============================================================================

test_that("tidy_assessment clamps pct to max 1.0", {
  # The code uses pmin(df[[pct_col]] / 100, 1.0)
  wide <- make_wide_assess(pct_meets = 105.0)  # over 100%
  tidy <- tidy_assessment(wide)

  meets_pct <- tidy$pct[tidy$proficiency_level == "meets"]
  expect_true(meets_pct <= 1.0,
              info = "pct should be clamped to max 1.0")
})
