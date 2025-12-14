###############################################
# min_index.R
# Construct Minimum Digital Access Index
# For Digital Development Convergence Project
###############################################

library(dplyr)
library(readr)

# ------------------------------------------------
# Load cleaned dataset (already processed)
# ------------------------------------------------

data_model <- read_csv("./data/cleaned_data/data_with_county.csv")

# Ensure components exist
required_vars <- c("cilaptop_bin", "cismrtphn_bin", "cidatapln_bin", "cihispeed_bin")

if (!all(required_vars %in% names(data_model))) {
  stop("Some required variables are missing from the dataset!")
}

# ------------------------------------------------
# Step 1: Construct Minimum Index
# DigitalIndex_min = min(Laptop, Smartphone, DataPlan, Broadband)
# ------------------------------------------------

# Use pmin() — vectorized and extremely fast
data_model <- data_model %>%
  mutate(
    digital_index_min = pmin(
      cilaptop_bin,
      cismrtphn_bin,
      cidatapln_bin,
      cihispeed_bin,
      na.rm = TRUE
    )
  )

# ------------------------------------------------
# Step 2: Save output
# ------------------------------------------------

write_csv(
  data_model,
  "./data/cleaned_data/data_with_min_index.csv"
)

message("Minimum digital access index saved to data_with_min_index.csv")
message("min_index.R script completed successfully.")
