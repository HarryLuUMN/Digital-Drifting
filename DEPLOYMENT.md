# Deployment Guide for Shiny App

This guide explains how to deploy the Digital Development Convergence Shiny app to shinyapps.io.

## Data Processing for Deployment

The app has been optimized for deployment by creating aggregated datasets that are much smaller than the original data (~2GB → ~0.34MB).

### Processed Data Files

All deployment-ready data files are located in the `deploy_use_data/` folder:

- **county_panel.csv** (0.19 MB): County-year panel data with aggregated digital index
- **sigma_convergence.csv** (<0.01 MB): Pre-computed sigma convergence results
- **county_beta_convergence.csv** (0.01 MB): Pre-computed county-level beta convergence data
- **state_beta_convergence.csv** (<0.01 MB): Pre-computed state-level beta convergence data
- **summary_stats.csv** (<0.01 MB): Summary statistics by year
- **sample_data.csv** (0.14 MB): Sample data for data explorer (10,000 rows)

**Total size: ~0.34 MB** (down from ~2GB)

### Regenerating Deployment Data

If you need to regenerate the deployment data (e.g., after updating the source data), run:

```r
Rscript process_data_for_deployment.R
```

This script will:
1. Load the full dataset from `data/cleaned_data/data_with_county.csv`
2. Create aggregated datasets optimized for the Shiny app
3. Save all files to `deploy_use_data/`

## App Configuration

The app (`app.R`) automatically detects deployment data:

- **If `deploy_use_data/` folder exists**: Uses pre-processed, lightweight datasets
- **If folder doesn't exist**: Falls back to loading the full dataset (for local development)

This allows the same app code to work both locally (with full data) and in deployment (with aggregated data).

## Deploying to shinyapps.io

### Step 1: Install rsconnect Package

```r
install.packages("rsconnect")
```

### Step 2: Set Up shinyapps.io Account

1. Go to https://www.shinyapps.io/
2. Sign up for a free account (or log in if you already have one)
3. Go to **Account** → **Tokens**
4. Copy your **Token** and **Secret**

### Step 3: Authorize Your Account

```r
library(rsconnect)

rsconnect::setAccountInfo(
  name = "your-account-name",      # Your shinyapps.io username
  token = "your-token",             # From Account → Tokens
  secret = "your-secret"            # From Account → Tokens
)
```

### Step 4: Deploy the App

From your project directory:

```r
library(rsconnect)

# Set working directory to project root
setwd("/path/to/Digital-Drifting")

# Deploy the app
rsconnect::deployApp(
  appDir = getwd(),
  appFiles = c("app.R", "deploy_use_data"),  # Include app and data folder
  appName = "digital-convergence",           # Choose a unique name
  account = "your-account-name",
  server = "shinyapps.io"
)
```

### Alternative: Deploy from RStudio

1. Open `app.R` in RStudio
2. Click the **Publish** button (or go to **Tools** → **Publish**)
3. Select **Publish to shinyapps.io**
4. Follow the prompts to authorize and deploy

## Deployment Checklist

Before deploying, ensure:

- [ ] `deploy_use_data/` folder exists with all CSV files
- [ ] All required packages are installed locally
- [ ] App runs successfully locally with deployment data
- [ ] shinyapps.io account is set up and authorized
- [ ] App name is unique (if deploying to free tier)

## File Size Considerations

- **Free Tier Limit**: 1GB total
- **Current Deployment Size**: ~0.34 MB (well within limits)
- **Starter Tier**: 5GB (if you need more space later)

## Troubleshooting

### App fails to start

- Check that `deploy_use_data/` folder is included in deployment
- Verify all CSV files are present
- Check app logs in shinyapps.io dashboard

### Data not loading

- Ensure file paths in `app.R` are relative (not absolute)
- Verify CSV files are not corrupted
- Check that `deploy_use_data/` folder structure matches expected format

### Package installation errors

- Some packages may need to be explicitly listed
- Check shinyapps.io package compatibility
- Review deployment logs for specific package errors

## Updating the Deployed App

To update the app after making changes:

```r
rsconnect::deployApp(
  appDir = getwd(),
  appFiles = c("app.R", "deploy_use_data"),
  appName = "digital-convergence",
  account = "your-account-name"
)
```

The app will be updated in place (same URL).

## Local Testing with Deployment Data

To test the app locally using deployment data (faster startup):

1. Ensure `deploy_use_data/` folder exists
2. Run the app normally:
   ```r
   shiny::runApp("app.R")
   ```
3. The app will automatically use deployment data if available

## Notes

- The deployment data is a subset/aggregation of the full dataset
- All visualizations and analyses work identically with deployment data
- Data explorer uses a 10,000-row sample in deployment mode
- Full dataset is still available for local development and analysis
