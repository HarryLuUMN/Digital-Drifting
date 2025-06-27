library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

data <- read_csv("./data/cleaned_data/data_with_county.csv")

# panel data construction (county-level)
county_panel <- data %>%
  filter(!is.na(digital_index), !is.na(COUNTYFIP)) %>%
  group_by(YEAR, COUNTYFIP) %>%
  summarise(avg_digital_index = mean(digital_index, na.rm = TRUE), .groups = "drop")

# Compute signma
sigma_series <- county_panel %>%
  group_by(YEAR) %>%
  summarise(std_dev = sd(avg_digital_index, na.rm = TRUE))

# Visualization
ggplot(sigma_series, aes(x = YEAR, y = std_dev)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "County-Level σ-Convergence of Digital Index",
    x = "Year",
    y = "Standard Deviation across Counties"
  ) +
  theme_minimal()

# saved the results
write_csv(sigma_series, "./data/results/county_sigma_convergence.csv")

