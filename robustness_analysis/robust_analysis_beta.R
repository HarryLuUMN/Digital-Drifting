library(dplyr)
library(tidyr)
library(ggplot2)

# -------------------------------------------------
# Function: compute beta convergence
# -------------------------------------------------

compute_beta <- function(df, index_col,
                         year_start = 2013,
                         year_end   = 2023,
                         metro_year = 2013) {
  
  # Step 1: county-year aggregation
  panel_data <- df %>%
    filter(YEAR %in% c(year_start, year_end),
           !is.na(.data[[index_col]]),
           !is.na(COUNTYFIP)) %>%
    group_by(YEAR, COUNTYFIP) %>%
    summarise(
      avg_index = mean(.data[[index_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Step 2: reshape to wide
  county_wide <- panel_data %>%
    pivot_wider(
      names_from = YEAR,
      values_from = avg_index,
      names_prefix = "y"
    ) %>%
    filter(!is.na(.data[[paste0("y", year_start)]]),
           !is.na(.data[[paste0("y", year_end)]])) %>%
    mutate(
      change = .data[[paste0("y", year_end)]] -
        .data[[paste0("y", year_start)]]
    )
  
  # Step 3: add METRO (urban/rural)
  metro_info <- df %>%
    filter(YEAR == metro_year, !is.na(METRO)) %>%
    select(COUNTYFIP, METRO) %>%
    distinct()
  
  county_wide <- county_wide %>%
    left_join(metro_info, by = "COUNTYFIP") %>%
    mutate(
      region_type = case_when(
        METRO %in% c(1, 3, 0) ~ "Rural",
        METRO %in% c(2, 4)    ~ "Urban",
        TRUE                 ~ NA_character_
      )
    )
  
  # Step 4: regression
  formula_str <- paste0(
    "change ~ ", paste0("y", year_start)
  )
  beta_model <- lm(as.formula(formula_str), data = county_wide)
  
  # Step 5: return everything needed
  list(
    model = beta_model,
    data  = county_wide
  )
}


plot_beta <- function(beta_obj, year_start, title) {
  
  ggplot(
    beta_obj$data %>% filter(region_type %in% c("Urban", "Rural")),
    aes(
      x = .data[[paste0("y", year_start)]],
      y = change,
      color = region_type
    )
  ) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    scale_color_manual(
      values = c("Urban" = "red", "Rural" = "blue")
    ) +
    labs(
      title = title,
      x = paste0("Digital Index in ", year_start),
      y = paste0("Change in Digital Index (", year_start, "–2023)"),
      color = "Region Type"
    ) +
    theme_minimal()
}

