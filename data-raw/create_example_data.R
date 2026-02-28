# Create example datasets from real cached MDE data
#
# This script loads REAL enrollment data from the Minnesota Department of Education
# (cached from a successful download) and saves it as bundled example data.
# NO synthetic data generation is used.

library(dplyr)

# Load real cached data
# This requires having successfully fetched data at least once with:
#   mnschooldata::fetch_enr(2023, use_cache = TRUE)
enr_tidy <- mnschooldata::fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
enr_wide <- mnschooldata::fetch_enr(2023, tidy = FALSE, use_cache = TRUE)

# Add district_type to tidy data by joining from wide format
district_types <- enr_wide %>%
  filter(!is.na(district_id)) %>%
  select(district_id, district_type) %>%
  distinct()

enr_with_type <- enr_tidy %>%
  left_join(district_types, by = "district_id") %>%
  mutate(is_charter = !is.na(district_type) & district_type == "07")

# Save as bundled datasets
enr_2023_example <- enr_with_type
enr_multi_example <- enr_with_type
districts_example <- enr_with_type %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, district_name, n_students, district_type, is_charter)

usethis::use_data(enr_2023_example, enr_multi_example, districts_example, overwrite = TRUE)
