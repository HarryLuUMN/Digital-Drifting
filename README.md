# Digital Development Convergence in the United States: A Spatial and Temporal Analysis from 2013 to 2023

This study investigates the convergence of digital infrastructure across U.S. counties and states from 2013 to 2023, with a focus on whether regional disparities in digital access have narrowed over time. Using microdata from the Integrated Public Use Microdata Series (IPUMS USA), we construct a household-level digital access index based on device and internet connectivity variables. We apply both standard σ- and β-convergence models as well as a spatial lag β-convergence model to evaluate two key hypotheses: (1) initially disadvantaged regions have experienced faster digital growth, and (2) geographically proximate regions converge together through spatial spillovers. Our results provide strong evidence of both σ- and β-convergence at the county and state levels, indicating a general reduction in the digital divide. However, we find no robust support for spatial spillovers—regions do not appear to converge faster as a result of their neighbors' growth. This suggests that while local catch-up dynamics are at play, geographic diffusion of digital development may be limited by policy fragmentation, administrative boundaries, or infrastructure barriers.

## Reproducibility Instructions

### Prerequisites

- **R Version**: R 4.0 or higher is recommended
- **RStudio**: Optional but recommended for easier navigation

### Step 1: Install Required R Packages

Install all required R packages by running the following in your R console:

```r
# Core data manipulation and visualization packages
install.packages(c("dplyr", "tidyr", "readr", "ggplot2", "ggrepel"))

# IPUMS data access package
install.packages("ipumsr", type = "source")

# Spatial analysis packages
install.packages(c("spdep", "sf", "spatialreg"))

# Convergence analysis package
install.packages("ConvergenceClubs")
```

Alternatively, you can install all packages at once:

```r
required_packages <- c("dplyr", "tidyr", "readr", "ggplot2", "ggrepel", 
                      "ipumsr", "spdep", "sf", "spatialreg", "ConvergenceClubs")

install.packages(required_packages)
```

### Step 2: Data Setup

1. **IPUMS USA Data**: 
   - The project uses IPUMS USA microdata files (`usa_00005.xml` and `usa_00006.dat`)
   - These files should be placed in the `data/raw_data/` directory
   - If you need to download the data, visit [IPUMS USA](https://usa.ipums.org/usa/) and extract the DDI file and microdata file

2. **Geographic Data**:
   - County shapefiles are already included in `data/geo_data/`
   - Files: `cb_2013_us_county_500k.shp` and associated files

3. **Directory Structure**:
   Ensure your project directory structure matches:
   ```
   Digital-Drifting/
   ├── data/
   │   ├── raw_data/          # IPUMS data files
   │   ├── geo_data/          # Shapefiles
   │   ├── cleaned_data/      # Processed data (generated)
   │   └── results/           # Analysis results (generated)
   ├── data_processing/
   ├── convergence/
   ├── robustness_analysis/
   └── images/                # Generated plots
   ```

### Step 3: Set Working Directory

Before running any scripts, set your working directory to the project root:

```r
setwd("/path/to/Digital-Drifting")
```

Or open the `writing_project.Rproj` file in RStudio, which will automatically set the working directory.

### Step 4: Execute Scripts in Order

1. **Data Processing** (`data_processing/processing.R`):
   - Processes raw IPUMS data
   - Creates digital access index
   - Generates cleaned datasets in `data/cleaned_data/`

2. **Convergence Analysis**:
   - `convergence/sigma_convergence.R` - σ-convergence analysis
   - `convergence/beta_convergence.R` - County-level β-convergence
   - `convergence/beta_convergence_state_level.R` - State-level β-convergence
   - `convergence/spatial_convergence.R` - Spatial lag β-convergence
   - `convergence/club_convergence.R` - Club convergence analysis

3. **Robustness Checks** (`robustness_analysis/`):
   - `robust_analysis_beta.R` - Robustness checks for β-convergence
   - `robust_analysis_sigma.R` - Robustness checks for σ-convergence
   - `min_index.R` - Analysis with minimum index
   - `pca_index.R` - Analysis with PCA index

### Step 5: Expected Outputs

After running the scripts, you should see:
- Cleaned data files in `data/cleaned_data/`
- Results files in `data/results/`
- Visualization plots in `images/`

### Notes

- Some scripts may require manual adjustment of file paths depending on your system
- The IPUMS data files are large and may take time to process
- Spatial analysis scripts require the geographic data files to be properly loaded
- If you encounter package installation issues, ensure you have the latest version of R and try installing packages individually
