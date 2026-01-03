#!/usr/bin/env Rscript
# Generate README figures for mnschooldata

library(ggplot2)
library(dplyr)
library(scales)
devtools::load_all(".")

# Create figures directory
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

# Theme
theme_readme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray40"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

colors <- c("total" = "#2C3E50", "white" = "#3498DB", "black" = "#E74C3C",
            "hispanic" = "#F39C12", "asian" = "#9B59B6")

# Get available years (handles both vector and list return types)
years <- get_available_years()
if (is.list(years)) {
  max_year <- years$max_year
  min_year <- years$min_year
} else {
  max_year <- max(years)
  min_year <- min(years)
}

# Fetch data
message("Fetching data...")
enr <- fetch_enr_multi((max_year - 9):max_year)
key_years <- seq(max(min_year, 2007), max_year, by = 5)
if (!max_year %in% key_years) key_years <- c(key_years, max_year)
enr_long <- fetch_enr_multi(key_years)
enr_current <- fetch_enr(max_year)

# 1. Twin Cities decline
message("Creating Twin Cities decline chart...")
tc <- enr %>%
  filter(is_district, grepl("Minneapolis|St. Paul", district_name, ignore.case = TRUE),
         subgroup == "total_enrollment", grade_level == "TOTAL")

p <- ggplot(tc, aes(x = end_year, y = n_students, color = district_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = comma) +
  labs(title = "Minneapolis and St. Paul Are Shrinking",
       subtitle = "Each lost over 10,000 students in the past decade",
       x = "School Year", y = "Students", color = "") +
  theme_readme()
ggsave("man/figures/twin-cities-decline.png", p, width = 10, height = 6, dpi = 150)

# 2. Minneapolis diversity (Somali)
message("Creating Minneapolis diversity chart...")
mpls <- enr %>%
  filter(is_district, grepl("Minneapolis", district_name, ignore.case = TRUE),
         grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian"))

p <- ggplot(mpls, aes(x = end_year, y = pct * 100, color = subgroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colors,
                     labels = c("Asian", "Black", "Hispanic", "White")) +
  labs(title = "Minneapolis Transformed by Somali Students",
       subtitle = "Over 20,000 Black students - one of largest Somali populations in US",
       x = "School Year", y = "Percent", color = "") +
  theme_readme()
ggsave("man/figures/mpls-diversity.png", p, width = 10, height = 6, dpi = 150)

# 3. Charter enrollment
message("Creating charter enrollment chart...")
charter <- enr %>%
  filter(is_charter, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  group_by(end_year) %>%
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop")

p <- ggplot(charter, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.5, color = colors["total"]) +
  geom_point(size = 3, color = colors["total"]) +
  scale_y_continuous(labels = comma) +
  labs(title = "Minnesota Invented Charter Schools",
       subtitle = "Over 60,000 students in 180+ charters",
       x = "School Year", y = "Students") +
  theme_readme()
ggsave("man/figures/charter-enrollment.png", p, width = 10, height = 6, dpi = 150)

# 4. Suburban growth
message("Creating suburban growth chart...")
suburban <- c("Anoka-Hennepin", "Lakeville", "Prior Lake", "Rosemount")
suburb_trend <- enr %>%
  filter(is_district, grepl(paste(suburban, collapse = "|"), district_name, ignore.case = TRUE),
         subgroup == "total_enrollment", grade_level == "TOTAL")

p <- ggplot(suburb_trend, aes(x = end_year, y = n_students, color = district_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = comma) +
  labs(title = "Suburban Ring is Booming",
       subtitle = "Growing while core cities shrink",
       x = "School Year", y = "Students", color = "") +
  theme_readme()
ggsave("man/figures/suburban-growth.png", p, width = 10, height = 6, dpi = 150)

# 5. Demographics
message("Creating demographics chart...")
demo <- enr_long %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian"))

p <- ggplot(demo, aes(x = end_year, y = pct * 100, color = subgroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colors,
                     labels = c("Asian", "Black", "Hispanic", "White")) +
  labs(title = "Minnesota Diversifying Fast",
       subtitle = "From 85% white in 2007 to under 70% today",
       x = "School Year", y = "Percent", color = "") +
  theme_readme()
ggsave("man/figures/demographics.png", p, width = 10, height = 6, dpi = 150)

# 6. COVID kindergarten
message("Creating COVID K chart...")
k_trend <- enr %>%
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "06", "12")) %>%
  mutate(grade_label = case_when(
    grade_level == "K" ~ "Kindergarten",
    grade_level == "01" ~ "Grade 1",
    grade_level == "06" ~ "Grade 6",
    grade_level == "12" ~ "Grade 12"
  ))

p <- ggplot(k_trend, aes(x = end_year, y = n_students, color = grade_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_y_continuous(labels = comma) +
  labs(title = "COVID Hit Minnesota Kindergarten Hard",
       subtitle = "Lost 8% - nearly 5,000 fewer kids",
       x = "School Year", y = "Students", color = "") +
  theme_readme()
ggsave("man/figures/covid-k.png", p, width = 10, height = 6, dpi = 150)

# 7. Iron Range
message("Creating Iron Range chart...")
iron_range <- c("Hibbing", "Virginia", "Eveleth", "Chisholm")
iron <- enr %>%
  filter(is_district, grepl(paste(iron_range, collapse = "|"), district_name, ignore.case = TRUE),
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  group_by(end_year) %>%
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop")

p <- ggplot(iron, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.5, color = colors["total"]) +
  geom_point(size = 3, color = colors["total"]) +
  scale_y_continuous(labels = comma) +
  labs(title = "Iron Range Still Declining",
       subtitle = "Hibbing, Virginia, Eveleth, Chisholm combined",
       x = "School Year", y = "Students") +
  theme_readme()
ggsave("man/figures/iron-range.png", p, width = 10, height = 6, dpi = 150)

# 8. Rochester growth
message("Creating Rochester chart...")
rochester <- enr %>%
  filter(is_district, grepl("Rochester", district_name, ignore.case = TRUE),
         subgroup == "total_enrollment", grade_level == "TOTAL")

p <- ggplot(rochester, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.5, color = colors["total"]) +
  geom_point(size = 3, color = colors["total"]) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(title = "Rochester: A Growing City",
       subtitle = "Mayo Clinic expansion drives student growth",
       x = "School Year", y = "Students") +
  theme_readme()
ggsave("man/figures/rochester-growth.png", p, width = 10, height = 6, dpi = 150)

# 9. EL concentration
message("Creating EL concentration chart...")
el <- enr_current %>%
  filter(is_district, subgroup == "lep", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(10) %>%
  mutate(district_label = reorder(district_name, n_students))

p <- ggplot(el, aes(x = district_label, y = n_students)) +
  geom_col(fill = colors["total"]) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "English Learners Approaching 10%",
       subtitle = "Over 80,000 students concentrated in Twin Cities metro",
       x = "", y = "English Learner Students") +
  theme_readme()
ggsave("man/figures/el-concentration.png", p, width = 10, height = 6, dpi = 150)

# 10. Economic divide
message("Creating econ divide chart...")
econ <- enr_current %>%
  filter(is_district, subgroup == "econ_disadv", grade_level == "TOTAL") %>%
  arrange(desc(pct)) %>%
  head(10) %>%
  mutate(district_label = reorder(district_name, pct))

p <- ggplot(econ, aes(x = district_label, y = pct * 100)) +
  geom_col(fill = colors["total"]) +
  coord_flip() +
  labs(title = "Free/Reduced Lunch Shows Economic Divide",
       subtitle = "From 3% in wealthy Edina to 80%+ in some districts",
       x = "", y = "Percent Economically Disadvantaged") +
  theme_readme()
ggsave("man/figures/econ-divide.png", p, width = 10, height = 6, dpi = 150)

message("Done! Generated 10 figures in man/figures/")
