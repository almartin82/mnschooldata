# Create comprehensive example datasets for vignettes
# These datasets demonstrate the structure and enable all 15 stories to render

library(tibble)
library(dplyr)

# Define years for multi-year data (2015-2024)
years <- 2015:2024

# Major districts to include
districts <- c(
  "Minneapolis Public Schools",
  "St. Paul Public Schools",
  "Anoka-Hennepin Public Schools",
  "Rosemount-Apple Valley-Eagan Public Schools",
  "Lakeville Area Public Schools",
  "Prior Lake-Savage Area Schools",
  "Rochester Public Schools",
  "Duluth Public Schools",
  "Hibbing Public Schools",
  "Virginia Public Schools",
  "Eveleth-Gilbert Public Schools",
  "Chisholm Public Schools",
  "Mountain Iron-Buhl Public Schools",
  "Edina Public Schools",
  "Minnetonka Public Schools",
  "Brooklyn Center Community Schools",
  "Richfield Public Schools",
  "Red Lake Public Schools",
  "Cass Lake-Bena Public Schools",
  "Fond du Lac Ojibwe School"
)

# Charter district examples (district IDs starting with 07)
charters <- c(
  "Harvest Preparatory School",
  "Higher Ground Academy",
  "Hmong College Prep Academy",
  "Best Academy"
)

subgroups <- c("total_enrollment", "white", "black", "hispanic", "asian",
               "native_american", "pacific_islander", "multiracial",
               "econ_disadv", "lep", "special_ed", "homeless")

grade_levels <- c("TOTAL", "K", "01", "02", "03", "04", "05", "06",
                  "07", "08", "09", "10", "11", "12")

# Helper function to generate enrollment with realistic decline/growth patterns
generate_district_data <- function(district_name, base_year = 2015, base_enrollment,
                                   growth_rate = 0, district_id = "0001-01") {

  # Calculate enrollment for each year
  enrollments <- numeric(length(years))
  enrollments[1] <- base_enrollment
  for (i in 2:length(years)) {
    # Add some noise
    noise <- rnorm(1, 0, base_enrollment * 0.01)
    enrollments[i] <- enrollments[i-1] * (1 + growth_rate) + noise
  }
  enrollments <- round(pmax(enrollments, 100))

  rows <- list()
  for (i in seq_along(years)) {
    yr <- years[i]
    total <- enrollments[i]

    # Generate subgroup percentages (vary slightly by year)
    # Use realistic Minnesota demographics - varies by district type
    if (grepl("Minneapolis|Saint Paul|Brooklyn", district_name)) {
      # Urban districts - more diverse
      white_pct <- runif(1, 0.25, 0.35)
      black_pct <- runif(1, 0.30, 0.40)
      hispanic_pct <- runif(1, 0.10, 0.18)
      asian_pct <- runif(1, 0.06, 0.12)
    } else if (grepl("Edina|Minnetonka|Lakeville|Prior Lake", district_name)) {
      # Wealthy suburbs - mostly white
      white_pct <- runif(1, 0.70, 0.85)
      black_pct <- runif(1, 0.03, 0.08)
      hispanic_pct <- runif(1, 0.03, 0.08)
      asian_pct <- runif(1, 0.05, 0.12)
    } else {
      # Other districts - moderate diversity
      white_pct <- runif(1, 0.55, 0.75)
      black_pct <- runif(1, 0.05, 0.15)
      hispanic_pct <- runif(1, 0.05, 0.12)
      asian_pct <- runif(1, 0.03, 0.08)
    }
    native_pct <- runif(1, 0.01, 0.03)
    remaining <- 1 - white_pct - black_pct - hispanic_pct - asian_pct - native_pct
    multiracial_pct <- remaining * 0.7
    pacific_pct <- remaining * 0.05

    # Program subgroups
    econ_pct <- runif(1, 0.30, 0.70)
    lep_pct <- runif(1, 0.10, 0.35)
    sped_pct <- runif(1, 0.12, 0.20)
    homeless_pct <- runif(1, 0.01, 0.05)

    # Total enrollment
    rows[[length(rows) + 1]] <- tibble(
      end_year = yr, state = "MN", district_id = district_id,
      district_name = district_name, school_name = district_name,
      level = "district", subgroup = "total_enrollment", grade_level = "TOTAL",
      n_students = total, pct = 1.0, aggregation_flag = "official",
      is_district = TRUE, is_state = FALSE, is_school = FALSE, type = "District"
    )

    # Demographic subgroups
    for (sg in c("white", "black", "hispanic", "asian", "native_american",
                 "pacific_islander", "multiracial")) {
      pct_val <- switch(sg,
        white = white_pct, black = black_pct, hispanic = hispanic_pct,
        asian = asian_pct, native_american = native_pct,
        pacific_islander = pacific_pct, multiracial = multiracial_pct
      )
      rows[[length(rows) + 1]] <- tibble(
        end_year = yr, state = "MN", district_id = district_id,
        district_name = district_name, school_name = district_name,
        level = "district", subgroup = sg, grade_level = "TOTAL",
        n_students = round(total * pct_val), pct = pct_val,
        aggregation_flag = "official",
        is_district = TRUE, is_state = FALSE, is_school = FALSE, type = "District"
      )
    }

    # Program subgroups
    for (sg in c("econ_disadv", "lep", "special_ed", "homeless")) {
      pct_val <- switch(sg,
        econ_disadv = econ_pct, lep = lep_pct,
        special_ed = sped_pct, homeless = homeless_pct
      )
      rows[[length(rows) + 1]] <- tibble(
        end_year = yr, state = "MN", district_id = district_id,
        district_name = district_name, school_name = district_name,
        level = "district", subgroup = sg, grade_level = "TOTAL",
        n_students = round(total * pct_val), pct = pct_val,
        aggregation_flag = "official",
        is_district = TRUE, is_state = FALSE, is_school = FALSE, type = "District"
      )
    }

    # Grade levels for total enrollment
    for (gl in c("K", "01", "06", "12")) {
      grade_pct <- ifelse(gl == "K", 0.07, ifelse(gl == "01", 0.075,
                   ifelse(gl == "06", 0.08, 0.07)))
      rows[[length(rows) + 1]] <- tibble(
        end_year = yr, state = "MN", district_id = district_id,
        district_name = district_name, school_name = district_name,
        level = "district", subgroup = "total_enrollment", grade_level = gl,
        n_students = round(total * grade_pct), pct = grade_pct,
        aggregation_flag = "official",
        is_district = TRUE, is_state = FALSE, is_school = FALSE, type = "District"
      )
    }
  }

  bind_rows(rows)
}

# Generate data for each district type

# Minneapolis - declining urban (lost ~10,000 students)
mpls_data <- generate_district_data("Minneapolis Public Schools",
  base_enrollment = 38000, growth_rate = -0.03, district_id = "0003-01")

# St. Paul - declining urban
stpaul_data <- generate_district_data("St. Paul Public Schools",
  base_enrollment = 42000, growth_rate = -0.025, district_id = "0003-02")

# Anoka-Hennepin - stable large suburb
anoka_data <- generate_district_data("Anoka-Hennepin Public Schools",
  base_enrollment = 37500, growth_rate = 0.002, district_id = "0001-11")

# Rosemount-Apple Valley-Eagan - growing suburb
rave_data <- generate_district_data("Rosemount-Apple Valley-Eagan Public Schools",
  base_enrollment = 28000, growth_rate = 0.01, district_id = "0001-196")

# Lakeville - growing suburb
lakeville_data <- generate_district_data("Lakeville Area Public Schools",
  base_enrollment = 11500, growth_rate = 0.015, district_id = "0001-194")

# Prior Lake - growing suburb
priorlake_data <- generate_district_data("Prior Lake-Savage Area Schools",
  base_enrollment = 9000, growth_rate = 0.02, district_id = "0001-719")

# Rochester - Mayo Clinic growth
rochester_data <- generate_district_data("Rochester Public Schools",
  base_enrollment = 16000, growth_rate = 0.012, district_id = "0001-535")

# Duluth - declining
duluth_data <- generate_district_data("Duluth Public Schools",
  base_enrollment = 9500, growth_rate = -0.015, district_id = "0003-03")

# Iron Range districts - declining
hibbing_data <- generate_district_data("Hibbing Public Schools",
  base_enrollment = 2200, growth_rate = -0.025, district_id = "0001-701")
virginia_data <- generate_district_data("Virginia Public Schools",
  base_enrollment = 1800, growth_rate = -0.02, district_id = "0001-706")
eveleth_data <- generate_district_data("Eveleth-Gilbert Public Schools",
  base_enrollment = 1100, growth_rate = -0.03, district_id = "0001-2154")
chisholm_data <- generate_district_data("Chisholm Public Schools",
  base_enrollment = 900, growth_rate = -0.025, district_id = "0001-695")
mtnirn_data <- generate_district_data("Mountain Iron-Buhl Public Schools",
  base_enrollment = 600, growth_rate = -0.02, district_id = "0001-712")

# Wealthy suburbs (low FRL)
edina_data <- generate_district_data("Edina Public Schools",
  base_enrollment = 8500, growth_rate = 0.005, district_id = "0001-273")
minnetonka_data <- generate_district_data("Minnetonka Public Schools",
  base_enrollment = 11000, growth_rate = 0.008, district_id = "0001-276")

# High EL districts
brooklyn_data <- generate_district_data("Brooklyn Center Community Schools",
  base_enrollment = 2000, growth_rate = -0.01, district_id = "0001-286")
richfield_data <- generate_district_data("Richfield Public Schools",
  base_enrollment = 4200, growth_rate = 0.005, district_id = "0001-280")

# Native American concentrated districts
redlake_data <- generate_district_data("Red Lake Public Schools",
  base_enrollment = 1200, growth_rate = 0.0, district_id = "0001-38")
casslake_data <- generate_district_data("Cass Lake-Bena Public Schools",
  base_enrollment = 800, growth_rate = -0.01, district_id = "0001-115")
fonddulac_data <- generate_district_data("Fond du Lac Ojibwe School",
  base_enrollment = 350, growth_rate = 0.01, district_id = "0001-32")

# Small rural districts (for story 14)
small_districts <- list()
set.seed(42)  # For reproducibility
for (i in 1:150) {
  # Sample from size categories
  cat_probs <- c(0.3, 0.25, 0.2, 0.2, 0.05)
  cat_idx <- sample(1:5, 1, prob = cat_probs)
  size_ranges <- list(100:249, 250:499, 500:999, 1000:4999, 5000:10000)
  size <- sample(size_ranges[[cat_idx]], 1)
  small_districts[[i]] <- tibble(
    end_year = 2024, state = "MN",
    district_id = sprintf("0001-%03d", i + 500),
    district_name = sprintf("Small District %d", i),
    school_name = sprintf("Small District %d", i),
    level = "district", subgroup = "total_enrollment", grade_level = "TOTAL",
    n_students = size, pct = 1.0, aggregation_flag = "official",
    is_district = TRUE, is_state = FALSE, is_school = FALSE, type = "District"
  )
}
small_data <- bind_rows(small_districts)

# Charter schools (district IDs starting with 07)
charter_data <- list()
for (i in 1:180) {
  charter_data[[i]] <- tibble(
    end_year = rep(years, each = 1),
    state = "MN",
    district_id = sprintf("07%03d-01", i),
    district_name = sprintf("Charter School %d", i),
    school_name = sprintf("Charter School %d", i),
    level = "district",
    subgroup = "total_enrollment",
    grade_level = "TOTAL",
    n_students = round(runif(length(years), 200, 800) * (1 + 0.02)^(seq_along(years)-1)),
    pct = 1.0,
    aggregation_flag = "official",
    is_district = TRUE, is_state = FALSE, is_school = FALSE, type = "District"
  )
}
charter_combined <- bind_rows(charter_data)

# Combine all district data
all_districts <- bind_rows(
  mpls_data, stpaul_data, anoka_data, rave_data, lakeville_data,
  priorlake_data, rochester_data, duluth_data,
  hibbing_data, virginia_data, eveleth_data, chisholm_data, mtnirn_data,
  edina_data, minnetonka_data, brooklyn_data, richfield_data,
  redlake_data, casslake_data, fonddulac_data,
  small_data, charter_combined
)

# Get total enrollment lookup for proper pct calculations
total_lookup <- all_districts %>%
  filter(subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, total = n_students)

# Join total enrollment to compute proper n_students for overrides
all_districts <- all_districts %>%
  left_join(total_lookup, by = c("end_year", "district_name"))

# Native American concentrated districts - high native_american %
all_districts <- all_districts %>%
  mutate(
    pct = case_when(
      district_name == "Red Lake Public Schools" & subgroup == "native_american" ~ 0.95,
      district_name == "Cass Lake-Bena Public Schools" & subgroup == "native_american" ~ 0.75,
      district_name == "Fond du Lac Ojibwe School" & subgroup == "native_american" ~ 0.85,
      TRUE ~ pct
    ),
    n_students = case_when(
      district_name == "Red Lake Public Schools" & subgroup == "native_american" ~ round(total * 0.95),
      district_name == "Cass Lake-Bena Public Schools" & subgroup == "native_american" ~ round(total * 0.75),
      district_name == "Fond du Lac Ojibwe School" & subgroup == "native_american" ~ round(total * 0.85),
      TRUE ~ n_students
    )
  )

# Low FRL for wealthy districts
all_districts <- all_districts %>%
  mutate(
    pct = case_when(
      district_name == "Edina Public Schools" & subgroup == "econ_disadv" ~ 0.04,
      district_name == "Minnetonka Public Schools" & subgroup == "econ_disadv" ~ 0.05,
      TRUE ~ pct
    ),
    n_students = case_when(
      district_name == "Edina Public Schools" & subgroup == "econ_disadv" ~ round(total * 0.04),
      district_name == "Minnetonka Public Schools" & subgroup == "econ_disadv" ~ round(total * 0.05),
      TRUE ~ n_students
    )
  )

# High EL for Brooklyn Center and Richfield
all_districts <- all_districts %>%
  mutate(
    pct = case_when(
      district_name == "Brooklyn Center Community Schools" & subgroup == "lep" ~ 0.35,
      district_name == "Richfield Public Schools" & subgroup == "lep" ~ 0.32,
      TRUE ~ pct
    ),
    n_students = case_when(
      district_name == "Brooklyn Center Community Schools" & subgroup == "lep" ~ round(total * 0.35),
      district_name == "Richfield Public Schools" & subgroup == "lep" ~ round(total * 0.32),
      TRUE ~ n_students
    )
  )

# Remove total column
all_districts <- all_districts %>% select(-total)

# Generate state-level data
state_data <- all_districts %>%
  filter(subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  group_by(end_year) %>%
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    state = "MN", district_id = NA, district_name = NA, school_name = "Minnesota",
    level = "state", subgroup = "total_enrollment", grade_level = "TOTAL",
    pct = 1.0, aggregation_flag = "official",
    is_district = FALSE, is_state = TRUE, is_school = FALSE, type = "State"
  )

# State demographics
state_demos <- all_districts %>%
  filter(subgroup %in% c("white", "black", "hispanic", "asian", "native_american",
                         "econ_disadv", "lep", "special_ed"),
         grade_level == "TOTAL") %>%
  group_by(end_year, subgroup) %>%
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") %>%
  left_join(state_data %>% select(end_year, total = n_students), by = "end_year") %>%
  mutate(
    pct = n_students / total,
    state = "MN", district_id = NA, district_name = NA, school_name = "Minnesota",
    level = "state", grade_level = "TOTAL", aggregation_flag = "official",
    is_district = FALSE, is_state = TRUE, is_school = FALSE, type = "State"
  ) %>%
  select(-total)

# State grade levels
state_grades <- all_districts %>%
  filter(subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "06", "12")) %>%
  group_by(end_year, grade_level) %>%
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    state = "MN", district_id = NA, district_name = NA, school_name = "Minnesota",
    level = "state", subgroup = "total_enrollment",
    pct = 1.0, aggregation_flag = "official",
    is_district = FALSE, is_state = TRUE, is_school = FALSE, type = "State"
  )

# Combine all data
enr_multi_example <- bind_rows(all_districts, state_data, state_demos, state_grades) %>%
  arrange(end_year, desc(is_state), desc(is_district), district_name, subgroup, grade_level)

# Filter for 2024 only
enr_2024_example <- enr_multi_example %>%
  filter(end_year == 2024)

# Districts example
districts_example <- enr_2024_example %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, n_students)

# Save as R data files
usethis::use_data(enr_2024_example, enr_multi_example, districts_example, overwrite = TRUE)

cat("Created example data with", nrow(enr_multi_example), "rows for multi-year and",
    nrow(enr_2024_example), "rows for 2024\n")
