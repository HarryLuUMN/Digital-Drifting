library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

data <- read_csv("./data/cleaned_data/data_with_county.csv")

# Panel Data Construction

county_panel <- data %>%
  filter(YEAR %in% c(2013, 2023), !is.na(digital_index), !is.na(COUNTYFIP)) %>%
  group_by(YEAR, COUNTYFIP) %>%
  summarise(avg_index = mean(digital_index, na.rm = TRUE), .groups = "drop")

county_wide <- county_panel %>%
  pivot_wider(names_from = YEAR, values_from = avg_index, names_prefix = "y") %>%
  filter(!is.na(y2013), !is.na(y2023)) %>%
  mutate(change = y2023 - y2013)

# Modeling

beta_model <- lm(change ~ y2013, data = county_wide)
summary(beta_model)

# Visualization

ggplot(county_wide, aes(x = y2013, y = change)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "County-Level β-Convergence of Digital Index (2013–2023)",
    x = "Digital Index in 2013",
    y = "Change in Digital Index (2023 - 2013)"
  ) +
  theme_minimal()

# results
# write_csv(county_wide, "./data//county_beta_convergence_panel.csv")


