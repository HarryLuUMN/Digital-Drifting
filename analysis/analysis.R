# Prepare Computational Env

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

data <- read_csv("./data/cleaned_data/data.csv")

summary(data)

# Data Aggregation(based on 'YEAR' and 'PUMA')
puma_panel <- data %>%
  group_by(YEAR, PUMA) %>%
  summarise(
    avg_digital_index = mean(digital_index, na.rm = TRUE),
    metro = first(METRO),
    density = first(DENSITY),
    .groups = "drop"
  )

# Sigma-convergence

sigma_stats <- puma_panel %>%
  group_by(YEAR) %>%
  summarise(std_dev = sd(avg_digital_index, na.rm = TRUE))

ggplot(sigma_stats, aes(x = YEAR, y = std_dev)) +
  geom_line() +
  geom_point() +
  labs(title = "σ-Convergence: Std Dev of Regional Digital Index",
       y = "Standard Deviation", x = "Year") +
  theme_minimal()

# Beta-convergence(Aggregated Analysis - 'METRO' level)

## Modeling

metro_panel <- data %>%
  filter(!is.na(digital_index), YEAR %in% c(2013, 2023)) %>%
  group_by(YEAR, METRO) %>%
  summarise(
    avg_digital_index = mean(digital_index, na.rm = TRUE),
    .groups = "drop"
  )


metro_wide <- metro_panel %>%
  pivot_wider(names_from = YEAR, values_from = avg_digital_index, names_prefix = "y") %>%
  filter(!is.na(y2013), !is.na(y2023)) %>%
  mutate(change = y2023 - y2013)

beta_metro <- lm(change ~ y2013, data = metro_wide)
summary(beta_metro)

## Visualization

library(ggplot2)

ggplot(metro_wide, aes(x = y2013, y = change, label = METRO)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  geom_text(vjust = -0.5, size = 3.5) +
  labs(
    title = "β-Convergence Across METRO Types (2013–2023)",
    x = "Digital Index in 2013",
    y = "Change in Digital Index (2023 - 2013)"
  ) +
  theme_minimal()

# Beta-convergence(Yearly - METRO-level)

## re-constrcut the metro panel data
metro_panel <- data %>%
  filter(!is.na(digital_index), YEAR >= 2013) %>%
  group_by(YEAR, METRO) %>%
  summarise(avg_index = mean(digital_index, na.rm = TRUE), .groups = "drop")


run_beta <- function(year_target) {
  year_var <- paste0("y", year_target)
  
  df <- metro_panel %>%
    filter(YEAR %in% c(2013, year_target)) %>%
    pivot_wider(names_from = YEAR, values_from = avg_index, names_prefix = "y") %>%
    filter(!is.na(y2013), !is.na(!!sym(year_var))) %>%
    mutate(change = !!sym(year_var) - y2013)
  
  model <- lm(change ~ y2013, data = df)
  data.frame(
    year = year_target,
    beta = coef(model)["y2013"],
    p = summary(model)$coefficients["y2013", "Pr(>|t|)"],
    r2 = summary(model)$r.squared
  )
}


results <- do.call(rbind, lapply(2014:2023, run_beta))

ggplot(results, aes(x = year, y = beta)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Change in β-Convergence Slope over Time (METRO level)",
    x = "Target Year (Compared to 2013)",
    y = "β Coefficient (Slope)"
  ) +
  theme_minimal()

