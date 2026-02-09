# Process Data for Deployment
# This script creates aggregated datasets optimized for Shiny app deployment
# The aggregated data is much smaller than the raw data (~2GB -> ~few MB)

library(dplyr)
library(tidyr)
library(readr)

cat("Starting data processing for deployment...\n")

# Load the full dataset
data_path <- "./data/cleaned_data/data_with_county.csv"
cat("Loading data from:", data_path, "\n")

data <- read_csv(data_path, show_col_types = FALSE)
cat("Data loaded. Rows:", nrow(data), "\n")

# Create deployment data directory
deploy_dir <- "./deploy_use_data"
if (!dir.exists(deploy_dir)) {
  dir.create(deploy_dir, recursive = TRUE)
  cat("Created directory:", deploy_dir, "\n")
}

# ============================================================================
# 1. Create County-Year Panel Data (for sigma convergence and general use)
# ============================================================================
cat("\n1. Creating county-year panel data...\n")
county_panel <- data %>%
  filter(!is.na(digital_index), !is.na(COUNTYFIP)) %>%
  group_by(YEAR, COUNTYFIP, STATEICP) %>%
  summarise(
    avg_digital_index = mean(digital_index, na.rm = TRUE),
    n_households = n(),
    .groups = "drop"
  )

# Add METRO information (use most common METRO value for each county-year)
metro_info <- data %>%
  filter(!is.na(METRO), !is.na(COUNTYFIP)) %>%
  group_by(YEAR, COUNTYFIP) %>%
  summarise(
    METRO = names(sort(table(METRO), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  mutate(METRO = as.numeric(METRO))

county_panel <- county_panel %>%
  left_join(metro_info, by = c("YEAR", "COUNTYFIP"))

write_csv(county_panel, file.path(deploy_dir, "county_panel.csv"))
cat("   Saved county_panel.csv. Rows:", nrow(county_panel), "\n")
cat("   File size:", round(file.info(file.path(deploy_dir, "county_panel.csv"))$size / 1024 / 1024, 2), "MB\n")

# ============================================================================
# 2. Pre-compute Sigma Convergence Results
# ============================================================================
cat("\n2. Computing sigma convergence results...\n")
sigma_results <- county_panel %>%
  group_by(YEAR) %>%
  summarise(std_dev = sd(avg_digital_index, na.rm = TRUE), .groups = "drop")

write_csv(sigma_results, file.path(deploy_dir, "sigma_convergence.csv"))
cat("   Saved sigma_convergence.csv. Rows:", nrow(sigma_results), "\n")

# ============================================================================
# 3. Pre-compute County-Level Beta Convergence Data
# ============================================================================
cat("\n3. Creating county-level beta convergence data...\n")
# First aggregate by county-year (handle any duplicates)
county_beta_panel <- county_panel %>%
  filter(YEAR %in% c(2013, 2023)) %>%
  group_by(YEAR, COUNTYFIP) %>%
  summarise(
    avg_digital_index = mean(avg_digital_index, na.rm = TRUE),
    METRO = first(METRO[!is.na(METRO)]),  # Take first non-NA METRO value
    .groups = "drop"
  )

county_beta_data <- county_beta_panel %>%
  pivot_wider(names_from = YEAR, values_from = avg_digital_index, names_prefix = "y") %>%
  filter(!is.na(y2013), !is.na(y2023)) %>%
  mutate(change = y2023 - y2013) %>%
  mutate(
    region_type = case_when(
      METRO %in% c(1, 3, 0) ~ "Rural",
      METRO %in% c(2, 4) ~ "Urban",
      TRUE ~ "Other"
    )
  )

write_csv(county_beta_data, file.path(deploy_dir, "county_beta_convergence.csv"))
cat("   Saved county_beta_convergence.csv. Rows:", nrow(county_beta_data), "\n")

# ============================================================================
# 4. Pre-compute State-Level Beta Convergence Data
# ============================================================================
cat("\n4. Creating state-level beta convergence data...\n")

# State name mapping
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

# Get STATEICP mapping from county_panel
county_state_map <- county_panel %>%
  filter(YEAR == 2013) %>%
  select(COUNTYFIP, STATEICP) %>%
  distinct()

# Compute county-level data for state aggregation
county_for_state <- county_beta_data %>%
  left_join(county_state_map, by = "COUNTYFIP") %>%
  filter(!is.na(STATEICP))

# Compute state-level averages
state_panel <- county_for_state %>%
  group_by(STATEICP) %>%
  summarise(
    y2013 = mean(y2013, na.rm = TRUE),
    y2023 = mean(y2023, na.rm = TRUE),
    change = mean(change, na.rm = TRUE),
    .groups = "drop"
  )

# Compute urban ratio by state
urban_ratio_by_state <- county_for_state %>%
  filter(region_type %in% c("Urban", "Rural")) %>%
  group_by(STATEICP) %>%
  summarise(urban_ratio = mean(region_type == "Urban", na.rm = TRUE), .groups = "drop")

state_beta_data <- state_panel %>%
  left_join(state_name_map, by = "STATEICP") %>%
  left_join(urban_ratio_by_state, by = "STATEICP")

write_csv(state_beta_data, file.path(deploy_dir, "state_beta_convergence.csv"))
cat("   Saved state_beta_convergence.csv. Rows:", nrow(state_beta_data), "\n")

# ============================================================================
# 5. Create Summary Statistics Dataset (for summary tab)
# ============================================================================
cat("\n5. Creating summary statistics dataset...\n")
summary_stats <- county_panel %>%
  group_by(YEAR) %>%
  summarise(
    Count = sum(n_households, na.rm = TRUE),
    Mean = mean(avg_digital_index, na.rm = TRUE),
    Median = median(avg_digital_index, na.rm = TRUE),
    SD = sd(avg_digital_index, na.rm = TRUE),
    Min = min(avg_digital_index, na.rm = TRUE),
    Max = max(avg_digital_index, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_stats, file.path(deploy_dir, "summary_stats.csv"))
cat("   Saved summary_stats.csv. Rows:", nrow(summary_stats), "\n")

# ============================================================================
# 6. Create Sample Data for Data Explorer (limited rows)
# ============================================================================
cat("\n6. Creating sample data for data explorer...\n")
# Create a sample dataset with key columns for the data explorer
sample_data <- data %>%
  select(YEAR, COUNTYFIP, STATEICP, METRO, digital_index) %>%
  sample_n(min(10000, nrow(data)))  # Sample up to 10,000 rows

write_csv(sample_data, file.path(deploy_dir, "sample_data.csv"))
cat("   Saved sample_data.csv. Rows:", nrow(sample_data), "\n")

# ============================================================================
# Summary
# ============================================================================
cat("\n" , rep("=", 60), "\n")
cat("Data processing complete!\n")
cat(rep("=", 60), "\n\n")

cat("Files created in", deploy_dir, ":\n")
files <- list.files(deploy_dir, full.names = TRUE)
for (f in files) {
  size_mb <- round(file.info(f)$size / 1024 / 1024, 2)
  cat("  -", basename(f), ":", size_mb, "MB\n")
}

total_size <- sum(file.info(files)$size) / 1024 / 1024
cat("\nTotal size:", round(total_size, 2), "MB\n")
cat("Original data size: ~", round(file.info(data_path)$size / 1024 / 1024 / 1024, 2), "GB\n")
cat("Size reduction: ~", round((1 - total_size / (file.info(data_path)$size / 1024 / 1024)) * 100, 1), "%\n")

cat("\nData is ready for deployment!\n")
