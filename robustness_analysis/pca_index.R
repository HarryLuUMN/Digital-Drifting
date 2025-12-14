###############################################
# pca_index.R
# Construct PCA-weighted Digital Access Index
# For Digital Development Convergence Project
###############################################

library(dplyr)
library(readr)
library(tidyr)

# -----------------------------------------
# Load cleaned dataset
# -----------------------------------------

data_model <- read_csv("./data/cleaned_data/data_with_county.csv")

# Keep only the 4 digital access components (already in 0/1 form)
pca_vars <- data_model %>%
  select(cilaptop_bin, cismrtphn_bin, cidatapln_bin, cihispeed_bin)

# -----------------------------------------
# Step 1: Standardize the variables
# -----------------------------------------

pca_scaled <- scale(pca_vars)

# -----------------------------------------
# Step 2: Run PCA
# -----------------------------------------

pca_res <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)

# View PCA output
summary(pca_res)
print(pca_res$rotation)

# -----------------------------------------
# Step 3: Extract first principal component weights
# -----------------------------------------

# The first eigenvector (loadings of PC1)
pc1 <- pca_res$rotation[,1]

# Convert to absolute weights and normalize to sum = 1
weights <- abs(pc1) / sum(abs(pc1))

# Display weight table
weight_table <- data.frame(
  component = c("Laptop", "Smartphone", "DataPlan", "Broadband"),
  weight = round(weights, 4)
)

print("PCA Weights:")
print(weight_table)

# -----------------------------------------
# Step 4: Construct PCA-weighted Digital Index
# digital_index_pca = Σ wi * Xi
# -----------------------------------------

data_model <- data_model %>%
  mutate(
    digital_index_pca =
      weights[1] * cilaptop_bin +
      weights[2] * cismrtphn_bin +
      weights[3] * cidatapln_bin +
      weights[4] * cihispeed_bin
  )

# -----------------------------------------
# Step 5: Save PCA results
# -----------------------------------------

write_csv(
  data_model,
  "./data/cleaned_data/data_with_pca_index.csv"
)

message("PCA-weighted digital index saved to data_with_pca_index.csv")
message("PCA script complete.")
