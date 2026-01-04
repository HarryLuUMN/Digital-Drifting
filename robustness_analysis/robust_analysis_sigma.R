library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

source("./robustness_analysis/robust_analysis_beta.R")
setwd("/Users/harrylu_mac/digital-drifting/Digital-Drifting")

getwd()

# Read cleaned data
# data <- read_csv("./data/cleaned_data/data_with_county.csv")
# data_pca      <- read_csv("./data/cleaned_data/data_with_pca_index.csv")
data_min      <- read_csv("./data/cleaned_data/data_with_min_index.csv")

# ---------------------------
# Function: compute sigma-series
# ---------------------------

compute_sigma <- function(data, index_col) {
  
  county_panel <- data %>%
    filter(!is.na(.data[[index_col]]), !is.na(COUNTYFIP)) %>%
    group_by(YEAR, COUNTYFIP) %>%
    summarise(
      avg_index = mean(.data[[index_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  sigma_series <- county_panel %>%
    group_by(YEAR) %>%
    summarise(std_dev = sd(avg_index, na.rm = TRUE))
  
  return(sigma_series)
}

# ---------------------------
# Run sigma convergence for 3 indices
# ---------------------------

# sigma_baseline <- compute_sigma(data, "digital_index")
sigma_pca      <- compute_beta(data_pca, "digital_index_pca")
# sigma_min      <- compute_beta(data_min, "digital_index_min")
# sigma_min
# ---------------------------
# Save results
# ---------------------------

# write_csv(sigma_baseline, "./data/results/sigma_baseline.csv")
# write_csv(sigma_pca,      "./data/results/sigma_pca.csv")
write_csv(sigma_min,      "./data/results/beta_min.csv")

# ---------------------------
# Example plot (baseline)
# ---------------------------

plot_beta(
  sigma_pca,
  2013,
  "County-Level β-Convergence (PCA Digital Index)"
)

ggplot(sigma_min, aes(x = YEAR, y = std_dev)) +
  geom_line(color = "darkorange", size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "σ-Convergence (Minimum Digital Index)",
    x = "Year",
    y = "Standard Deviation across Counties"
  ) +
  theme_minimal()



# ggsave("./images/sigma_baseline.png", width = 8, height = 5)
# ggsave("./images/sigma_pca.png", width = 8, height = 5)
ggsave("./images/sigma_min.png", width = 8, height = 5)


