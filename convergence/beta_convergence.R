# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# Load the dataset
data <- read_csv("./data/cleaned_data/data_with_county.csv")

# Filter for years 2013 and 2023 with non-missing digital_index and COUNTYFIP
panel_data <- data %>%
  filter(YEAR %in% c(2013, 2023), !is.na(digital_index), !is.na(COUNTYFIP)) %>%
  group_by(YEAR, COUNTYFIP) %>%
  summarise(
    avg_index = mean(digital_index, na.rm = TRUE),
    .groups = "drop"
  )

# Reshape to wide format and only keep counties with non-missing values in both years
county_wide <- panel_data %>%
  pivot_wider(names_from = YEAR, values_from = avg_index, names_prefix = "y") %>%
  filter(!is.na(y2013), !is.na(y2023)) %>%
  mutate(change = y2023 - y2013)

# Add METRO information from 2013 (for urban/rural classification)
metro_info <- data %>%
  filter(YEAR == 2013, !is.na(METRO)) %>%
  select(COUNTYFIP, METRO) %>%
  distinct()

county_wide <- county_wide %>%
  left_join(metro_info, by = "COUNTYFIP") %>%
  mutate(
    region_type = case_when(
      METRO %in% c(1, 3, 0) ~ "Rural",
      METRO %in% c(2, 4) ~ "Urban",
      TRUE ~ "Other"
    )
  )

# Run linear model for β-convergence
beta_model <- lm(change ~ y2013, data = county_wide)
summary(beta_model)

# Visualization: color points by region type
ggplot(county_wide %>% filter(region_type %in% c("Urban", "Rural")),
       aes(x = y2013, y = change, color = region_type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(
    values = c("Urban" = "red", "Rural" = "blue"),
    breaks = c("Urban", "Rural")
  ) +
  labs(
    title = "County-Level β-Convergence of Digital Index (2013–2023)",
    x = "Digital Index in 2013",
    y = "Change in Digital Index (2023 - 2013)",
    color = "Region Type"
  ) +
  theme_minimal()



