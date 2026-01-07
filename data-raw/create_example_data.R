# Create minimal example datasets for vignettes
# These are tiny fake datasets that demonstrate the structure

library(tibble)
library(dplyr)

# Example 2024 enrollment data (minimal subset)
enr_2024_example <- tribble(
  ~end_year, ~state, ~district_name, ~school_name, ~level, ~subgroup, ~grade_level, ~n_students, ~aggregation_flag,
  2024, "MN", "Minneapolis Public Schools", "Minneapolis Public Schools", "district", "total_enrollment", "TOTAL", 28500, "official",
  2024, "MN", "Minneapolis Public Schools", "Minneapolis Public Schools", "district", "white", "TOTAL", 9500, "official",
  2024, "MN", "Minneapolis Public Schools", "Minneapolis Public Schools", "district", "black", "TOTAL", 12500, "official",
  2024, "MN", "Minneapolis Public Schools", "Minneapolis Public Schools", "district", "hispanic", "TOTAL", 3200, "official",
  2024, "MN", "Minneapolis Public Schools", "Minneapolis Public Schools", "district", "asian", "TOTAL", 2100, "official",
  2024, "MN", "Saint Paul Public Schools", "Saint Paul Public Schools", "district", "total_enrollment", "TOTAL", 33200, "official",
  2024, "MN", "Saint Paul Public Schools", "Saint Paul Public Schools", "district", "white", "TOTAL", 9800, "official",
  2024, "MN", "Saint Paul Public Schools", "Saint Paul Public Schools", "district", "black", "TOTAL", 13200, "official",
  2024, "MN", "Saint Paul Public Schools", "Saint Paul Public Schools", "district", "hispanic", "TOTAL", 5600, "official",
  2024, "MN", "Saint Paul Public Schools", "Saint Paul Public Schools", "district", "asian", "TOTAL", 3100, "official",
  2024, "MN", NA, "Minnesota", "state", "total_enrollment", "TOTAL", 876000, "official",
  2024, "MN", NA, "Minnesota", "state", "white", "TOTAL", 525000, "official",
  2024, "MN", NA, "Minnesota", "state", "black", "TOTAL", 105000, "official",
  2024, "MN", NA, "Minnesota", "state", "hispanic", "TOTAL", 97000, "official",
  2024, "MN", NA, "Minnesota", "state", "asian", "TOTAL", 78000, "official"
)

# Example multi-year enrollment data (just 3 years for brevity)
enr_multi_example <- bind_rows(
  enr_2024_example |> mutate(end_year = 2022),
  enr_2024_example |> mutate(end_year = 2023),
  enr_2024_example |> mutate(end_year = 2024)
)

# Example districts data (for quickstart vignette)
districts_example <- enr_2024_example |>
  filter(level == "district") |>
  select(end_year, district_name, subgroup, grade_level, n_students)

# Save as R data files
usethis::use_data(enr_2024_example, enr_multi_example, districts_example, overwrite = TRUE)
