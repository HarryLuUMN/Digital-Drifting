# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(ggrepel)

# Load the dataset
data <- read_csv("./data/cleaned_data/data_with_county.csv")

# Filter valid rows
data_filtered <- data %>%
  filter(YEAR %in% c(2013, 2023), !is.na(COUNTYFIP), !is.na(STATEICP), !is.na(digital_index))

# Construct county-level panel for 2013 and 2023
county_panel <- data_filtered %>%
  group_by(YEAR, COUNTYFIP, STATEICP) %>%
  summarise(avg_index = mean(digital_index, na.rm = TRUE), .groups = "drop")

# Pivot to wide format and compute change
county_wide <- county_panel %>%
  pivot_wider(names_from = YEAR, values_from = avg_index, names_prefix = "y") %>%
  filter(!is.na(y2013), !is.na(y2023)) %>%
  mutate(change = y2023 - y2013)

# Merge in METRO info and classify region_type
metro_info <- data_filtered %>%
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

# Compute state-level urban ratio
urban_ratio_by_state <- county_wide %>%
  filter(region_type %in% c("Urban", "Rural")) %>%
  group_by(STATEICP) %>%
  summarise(urban_ratio = mean(region_type == "Urban", na.rm = TRUE), .groups = "drop")

# Compute state-level averages for 2013 and 2023
state_panel <- county_wide %>%
  group_by(STATEICP) %>%
  summarise(
    y2013 = mean(y2013, na.rm = TRUE),
    y2023 = mean(y2023, na.rm = TRUE),
    change = mean(change, na.rm = TRUE),
    .groups = "drop"
  )

# Add state name mapping
state_name_map <- tibble::tribble(
  ~STATEICP, ~StateName,
  1, "Connecticut", 2, "Maine", 3, "Massachusetts", 4, "New Hampshire", 5, "Rhode Island", 6, "Vermont",
  11, "Delaware", 12, "New Jersey", 13, "New York", 14, "Pennsylvania",
  21, "Illinois", 22, "Indiana", 23, "Michigan", 24, "Ohio", 25, "Wisconsin",
  31, "Iowa", 32, "Kansas", 33, "Minnesota", 34, "Missouri", 35, "Nebraska", 36, "North Dakota", 37, "South Dakota",
  40, "Virginia", 41, "Alabama", 42, "Arkansas", 43, "Florida", 44, "Georgia", 45, "Louisiana", 46, "Mississippi",
  47, "North Carolina", 48, "South Carolina", 49, "Texas", 51, "Kentucky", 52, "Maryland", 53, "Oklahoma",
  54, "Tennessee", 56, "West Virginia", 61, "Arizona", 62, "Colorado", 63, "Idaho", 64, "Montana",
  65, "Nevada", 66, "New Mexico", 67, "Utah", 68, "Wyoming", 71, "California", 72, "Oregon", 73, "Washington",
  81, "Alaska", 82, "Hawaii", 83, "Puerto Rico", 98, "District of Columbia"
)

# Merge in state names and urban ratio
state_wide <- state_panel %>%
  left_join(state_name_map, by = "STATEICP") %>%
  left_join(urban_ratio_by_state, by = "STATEICP")

# Fit beta-convergence model
state_beta_model <- lm(change ~ y2013, data = state_wide)
summary(state_beta_model)

# Plot: State labels with alpha mapped to urban_ratio
ggplot(state_wide, aes(x = y2013, y = change)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_text_repel(aes(label = StateName, alpha = urban_ratio), size = 3.2, max.overlaps = 100) +
  scale_alpha(range = c(0.3, 1), name = "Urban Share") +
  labs(
    title = "State-Level β-Convergence of Digital Index (2013–2023)",
    x = "Digital Index in 2013 (State Average)",
    y = "Change in Digital Index (2023 - 2013)"
  ) +
  theme_minimal()


# Bar chart of state-level urban ratio (sorted from high to low)
state_wide %>%
  filter(!is.na(urban_ratio)) %>%
  arrange(desc(urban_ratio)) %>%
  mutate(StateName = factor(StateName, levels = StateName)) %>%  # preserve order
  ggplot(aes(x = StateName, y = urban_ratio)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # horizontal bar chart
  labs(
    title = "Urban Share by State",
    x = "State",
    y = "Urban County Share"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold")
  )

