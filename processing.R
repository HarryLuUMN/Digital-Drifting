# Packages Preparation

install.packages("ipumsr")
install.packages("tidyr")

library("tidyr")
library(ipumsr)
library(dplyr)
library(ggplot2)
library(readr)

ddi <- read_ipums_ddi("./data/raw_data/usa_00004.xml")
data <- read_ipums_micro(ddi)
ipums_data <- read_ipums_micro(ddi)
glimpse(ipums_data)

# Data Status

summary(ipums_data)

na_rates <- sapply(data, function(x) mean(is.na(x))) 
na_df <- data.frame(variable = names(na_rates), na_rate = round(na_rates, 4))  

na_df <- na_df[order(-na_df$na_rate), ]

head(na_df, 10)

data_recent <- data %>% filter(YEAR >= 2013)

na_rates_recent <- sapply(data_recent, function(x) mean(is.na(x)))

na_df_recent <- data.frame(
  variable = names(na_rates_recent),
  na_rate = round(na_rates_recent, 4)
)


na_df_recent <- na_df_recent[order(-na_df_recent$na_rate), ]

head(na_df_recent, 10)

# Data Processing(Only using 2013-now data for processing)

## Data Encoding

data_recent <- data_recent %>%
  mutate(
    cilaptop_bin = ifelse(CILAPTOP == 1, 1, ifelse(CILAPTOP == 0, 0, NA)),
    cismrtphn_bin = ifelse(CISMRTPHN == 1, 1, ifelse(CISMRTPHN == 0, 0, NA)),
    cidatapln_bin = ifelse(CIDATAPLN == 1, 1, ifelse(CIDATAPLN == 0, 0, NA)),
    cihispeed_bin = case_when(
      CIHISPEED %in% c(10, 20) ~ 1,
      CIHISPEED == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )


## Drop URBAN Column
data_recent <- data_recent %>% select(-URBAN) # drop URBAN column

## Imputation for NA Values
data_recent <- data_recent %>%
  mutate(
    cismrtphn_bin = ifelse(is.na(cismrtphn_bin), 0, cismrtphn_bin)
  )


## Construct Digital Index
data_recent <- data_recent %>%
  rowwise() %>%
  mutate(digital_index = mean(c(cilaptop_bin, cismrtphn_bin, cidatapln_bin, cihispeed_bin), na.rm = TRUE)) %>%
  ungroup()

## Check the data again
summary(data_recent)

na_rates <- sapply(data_recent, function(x) mean(is.na(x)))
na_df <- data.frame(
  variable = names(na_rates),
  na_rate = round(na_rates, 4)
)

na_df <- na_df[order(-na_df$na_rate), ]
print(na_df)

# Final Cleaning
data_model <- data_recent %>%
  drop_na(cilaptop_bin, cismrtphn_bin, cidatapln_bin, cihispeed_bin)
summary(data_model)

## Save the cleaned data to designated path

write_csv(data_model, "./data/cleaned_data/data.csv")
file.exists("./data/cleaned_data/data.csv")






