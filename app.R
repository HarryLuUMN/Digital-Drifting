# Digital Development Convergence Visualization App
# R Shiny Application for Visualizing Convergence Analysis Results

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DT)
library(plotly)

# Load data
data_path <- "./data/cleaned_data/data_with_county.csv"
sigma_results_path <- "./data/results/county_sigma_convergence.csv"

# Check if data files exist, if not provide instructions
if (!file.exists(data_path)) {
  stop("Data file not found. Please ensure data/cleaned_data/data_with_county.csv exists.")
}

# Load main dataset
data <- read_csv(data_path, show_col_types = FALSE)

# Load sigma convergence results if available
sigma_results <- NULL
if (file.exists(sigma_results_path)) {
  sigma_results <- read_csv(sigma_results_path, show_col_types = FALSE)
} else {
  # Compute sigma convergence on the fly
  county_panel <- data %>%
    filter(!is.na(digital_index), !is.na(COUNTYFIP)) %>%
    group_by(YEAR, COUNTYFIP) %>%
    summarise(avg_digital_index = mean(digital_index, na.rm = TRUE), .groups = "drop")
  
  sigma_results <- county_panel %>%
    group_by(YEAR) %>%
    summarise(std_dev = sd(avg_digital_index, na.rm = TRUE), .groups = "drop")
}

# Prepare county-level beta convergence data
prepare_county_beta_data <- function() {
  panel_data <- data %>%
    filter(YEAR %in% c(2013, 2023), !is.na(digital_index), !is.na(COUNTYFIP)) %>%
    group_by(YEAR, COUNTYFIP) %>%
    summarise(avg_index = mean(digital_index, na.rm = TRUE), .groups = "drop")
  
  county_wide <- panel_data %>%
    pivot_wider(names_from = YEAR, values_from = avg_index, names_prefix = "y") %>%
    filter(!is.na(y2013), !is.na(y2023)) %>%
    mutate(change = y2023 - y2013)
  
  # Add METRO information
  metro_info <- data %>%
    filter(YEAR == 2013, !is.na(METRO)) %>%
    select(COUNTYFIP, METRO) %>%
    distinct()
  
  county_wide <- county_wide %>%
    left_join(metro_info, by = "COUNTYFIP") %>%
    mutate(
      region_type = case_when(
        METRO %in% c(1, 3, 0) ~ "Rural",
        METRO %in% c(2, 4) ~ "Urban",
        TRUE ~ "Other"
      )
    )
  
  return(county_wide)
}

# Prepare state-level beta convergence data
prepare_state_beta_data <- function() {
  data_filtered <- data %>%
    filter(YEAR %in% c(2013, 2023), !is.na(COUNTYFIP), !is.na(STATEICP), !is.na(digital_index))
  
  county_panel <- data_filtered %>%
    group_by(YEAR, COUNTYFIP, STATEICP) %>%
    summarise(avg_index = mean(digital_index, na.rm = TRUE), .groups = "drop")
  
  county_wide <- county_panel %>%
    pivot_wider(names_from = YEAR, values_from = avg_index, names_prefix = "y") %>%
    filter(!is.na(y2013), !is.na(y2023)) %>%
    mutate(change = y2023 - y2013)
  
  # Add METRO info
  metro_info <- data_filtered %>%
    select(COUNTYFIP, METRO) %>%
    distinct()
  
  county_wide <- county_wide %>%
    left_join(metro_info, by = "COUNTYFIP") %>%
    mutate(
      region_type = case_when(
        METRO %in% c(1, 3, 0) ~ "Rural",
        METRO %in% c(2, 4) ~ "Urban",
        TRUE ~ "Other"
      )
    )
  
  # Compute state-level averages
  state_panel <- county_wide %>%
    group_by(STATEICP) %>%
    summarise(
      y2013 = mean(y2013, na.rm = TRUE),
      y2023 = mean(y2023, na.rm = TRUE),
      change = mean(change, na.rm = TRUE),
      .groups = "drop"
    )
  
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
  
  # Compute urban ratio by state
  urban_ratio_by_state <- county_wide %>%
    filter(region_type %in% c("Urban", "Rural")) %>%
    group_by(STATEICP) %>%
    summarise(urban_ratio = mean(region_type == "Urban", na.rm = TRUE), .groups = "drop")
  
  state_wide <- state_panel %>%
    left_join(state_name_map, by = "STATEICP") %>%
    left_join(urban_ratio_by_state, by = "STATEICP")
  
  return(state_wide)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Digital Development Convergence Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Sigma Convergence", tabName = "sigma", icon = icon("chart-line")),
      menuItem("Beta Convergence - County", tabName = "beta_county", icon = icon("map")),
      menuItem("Beta Convergence - State", tabName = "beta_state", icon = icon("flag")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Summary Statistics", tabName = "summary", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "About This Application", width = 12, solidHeader = TRUE, status = "primary",
            h3("Digital Development Convergence Visualization"),
            p("This interactive dashboard visualizes the results of convergence analysis examining digital infrastructure development across U.S. counties and states from 2013 to 2023."),
            h4("Key Features:"),
            tags$ul(
              tags$li("Sigma (σ) Convergence: Track the reduction in regional disparities over time"),
              tags$li("Beta (β) Convergence: Examine catch-up dynamics at county and state levels"),
              tags$li("Interactive Visualizations: Explore the data with customizable filters"),
              tags$li("Data Explorer: Browse and filter the underlying dataset")
            ),
            h4("Navigation:"),
            p("Use the sidebar menu to navigate between different analysis views.")
          )
        ),
        fluidRow(
          valueBox(
            value = nrow(data %>% filter(!is.na(COUNTYFIP)) %>% distinct(COUNTYFIP)),
            subtitle = "Counties Analyzed",
            icon = icon("map-marker-alt"),
            color = "blue"
          ),
          valueBox(
            value = length(unique(data$YEAR)),
            subtitle = "Years Covered",
            icon = icon("calendar"),
            color = "green"
          ),
          valueBox(
            value = round(mean(data$digital_index, na.rm = TRUE), 3),
            subtitle = "Average Digital Index",
            icon = icon("chart-bar"),
            color = "yellow"
          )
        )
      ),
      
      # Sigma Convergence Tab
      tabItem(tabName = "sigma",
        fluidRow(
          box(
            title = "Sigma (σ) Convergence Over Time", width = 12, solidHeader = TRUE, status = "primary",
            plotlyOutput("sigma_plot", height = "500px"),
            br(),
            p("Sigma convergence measures the reduction in dispersion of digital access across regions over time. 
              A declining standard deviation indicates convergence (reduction in regional disparities).")
          )
        ),
        fluidRow(
          box(
            title = "Sigma Convergence Data", width = 12, solidHeader = TRUE, status = "info",
            DT::dataTableOutput("sigma_table")
          )
        )
      ),
      
      # Beta Convergence - County Tab
      tabItem(tabName = "beta_county",
        fluidRow(
          box(
            title = "County-Level Beta (β) Convergence", width = 12, solidHeader = TRUE, status = "primary",
            plotlyOutput("beta_county_plot", height = "500px"),
            br(),
            p("Beta convergence examines whether initially disadvantaged counties experienced faster growth in digital access. 
              A negative relationship between initial level and change indicates convergence.")
          )
        ),
        fluidRow(
          box(
            title = "Filter by Region Type", width = 4, solidHeader = TRUE, status = "info",
            checkboxGroupInput("region_filter_county", "Region Type:",
              choices = c("Urban" = "Urban", "Rural" = "Rural", "Other" = "Other"),
              selected = c("Urban", "Rural")
            )
          ),
          box(
            title = "Model Summary", width = 8, solidHeader = TRUE, status = "info",
            verbatimTextOutput("beta_county_model")
          )
        ),
        fluidRow(
          box(
            title = "County-Level Data", width = 12, solidHeader = TRUE, status = "info",
            DT::dataTableOutput("beta_county_table")
          )
        )
      ),
      
      # Beta Convergence - State Tab
      tabItem(tabName = "beta_state",
        fluidRow(
          box(
            title = "State-Level Beta (β) Convergence", width = 12, solidHeader = TRUE, status = "primary",
            plotlyOutput("beta_state_plot", height = "500px"),
            br(),
            p("State-level beta convergence analysis. Point transparency reflects the urban share of counties within each state.")
          )
        ),
        fluidRow(
          box(
            title = "Model Summary", width = 12, solidHeader = TRUE, status = "info",
            verbatimTextOutput("beta_state_model")
          )
        ),
        fluidRow(
          box(
            title = "State-Level Data", width = 12, solidHeader = TRUE, status = "info",
            DT::dataTableOutput("beta_state_table")
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "explorer",
        fluidRow(
          box(
            title = "Data Explorer", width = 12, solidHeader = TRUE, status = "primary",
            DT::dataTableOutput("data_table")
          )
        ),
        fluidRow(
          box(
            title = "Filters", width = 12, solidHeader = TRUE, status = "info",
            fluidRow(
              column(4,
                sliderInput("year_filter", "Year Range:",
                  min = min(data$YEAR, na.rm = TRUE),
                  max = max(data$YEAR, na.rm = TRUE),
                  value = c(min(data$YEAR, na.rm = TRUE), max(data$YEAR, na.rm = TRUE)),
                  step = 1
                )
              ),
              column(4,
                sliderInput("index_filter", "Digital Index Range:",
                  min = 0,
                  max = 1,
                  value = c(0, 1),
                  step = 0.01
                )
              ),
              column(4,
                selectInput("metro_filter", "Metro Status:",
                  choices = c("All" = "all", sort(unique(data$METRO[!is.na(data$METRO)]))),
                  selected = "all"
                )
              )
            )
          )
        )
      ),
      
      # Summary Statistics Tab
      tabItem(tabName = "summary",
        fluidRow(
          box(
            title = "Digital Index by Year", width = 6, solidHeader = TRUE, status = "primary",
            plotlyOutput("summary_year_plot", height = "400px")
          ),
          box(
            title = "Digital Index Distribution", width = 6, solidHeader = TRUE, status = "primary",
            plotlyOutput("summary_dist_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Summary Statistics by Year", width = 12, solidHeader = TRUE, status = "info",
            DT::dataTableOutput("summary_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Prepare data once
  county_beta_data <- reactive({
    prepare_county_beta_data()
  })
  
  state_beta_data <- reactive({
    prepare_state_beta_data()
  })
  
  # Sigma Convergence Plot
  output$sigma_plot <- renderPlotly({
    p <- ggplot(sigma_results, aes(x = YEAR, y = std_dev)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(size = 2, color = "steelblue") +
      labs(
        title = "County-Level σ-Convergence of Digital Index",
        x = "Year",
        y = "Standard Deviation across Counties"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    ggplotly(p)
  })
  
  # Sigma Convergence Table
  output$sigma_table <- DT::renderDataTable({
    DT::datatable(sigma_results, 
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE) %>%
      DT::formatRound(columns = "std_dev", digits = 4)
  })
  
  # Beta Convergence - County Plot
  output$beta_county_plot <- renderPlotly({
    county_data <- county_beta_data()
    
    # Apply region filter
    if (!is.null(input$region_filter_county)) {
      county_data <- county_data %>%
        filter(region_type %in% input$region_filter_county)
    }
    
    # Fit model
    model <- lm(change ~ y2013, data = county_data)
    
    p <- ggplot(county_data %>% filter(region_type %in% c("Urban", "Rural")),
           aes(x = y2013, y = change, color = region_type, text = paste("County:", COUNTYFIP))) +
      geom_point(alpha = 0.6, size = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      scale_color_manual(
        values = c("Urban" = "red", "Rural" = "blue"),
        breaks = c("Urban", "Rural")
      ) +
      labs(
        title = "County-Level β-Convergence of Digital Index (2013–2023)",
        x = "Digital Index in 2013",
        y = "Change in Digital Index (2023 - 2013)",
        color = "Region Type"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    ggplotly(p, tooltip = c("x", "y", "color", "text"))
  })
  
  # Beta Convergence - County Model Summary
  output$beta_county_model <- renderPrint({
    county_data <- county_beta_data()
    
    if (!is.null(input$region_filter_county)) {
      county_data <- county_data %>%
        filter(region_type %in% input$region_filter_county)
    }
    
    model <- lm(change ~ y2013, data = county_data)
    summary(model)
  })
  
  # Beta Convergence - County Table
  output$beta_county_table <- DT::renderDataTable({
    county_data <- county_beta_data()
    
    if (!is.null(input$region_filter_county)) {
      county_data <- county_data %>%
        filter(region_type %in% input$region_filter_county)
    }
    
    DT::datatable(county_data %>% 
      select(COUNTYFIP, y2013, y2023, change, region_type),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE) %>%
      DT::formatRound(columns = c("y2013", "y2023", "change"), digits = 4)
  })
  
  # Beta Convergence - State Plot
  output$beta_state_plot <- renderPlotly({
    state_data <- state_beta_data() %>%
      filter(!is.na(StateName))  # Filter out missing state names
    
    p <- ggplot(state_data, aes(x = y2013, y = change, 
                                text = paste("State:", StateName, "<br>Urban Ratio:", round(urban_ratio, 3)))) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      geom_point(aes(alpha = urban_ratio), size = 3, color = "steelblue") +
      geom_text(aes(label = StateName), size = 2.5, hjust = 0, vjust = 0, check_overlap = TRUE, show.legend = FALSE) +
      scale_alpha(range = c(0.3, 1), name = "Urban Share") +
      labs(
        title = "State-Level β-Convergence of Digital Index (2013–2023)",
        x = "Digital Index in 2013 (State Average)",
        y = "Change in Digital Index (2023 - 2013)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # Beta Convergence - State Model Summary
  output$beta_state_model <- renderPrint({
    state_data <- state_beta_data()
    model <- lm(change ~ y2013, data = state_data)
    summary(model)
  })
  
  # Beta Convergence - State Table
  output$beta_state_table <- DT::renderDataTable({
    state_data <- state_beta_data()
    
    DT::datatable(state_data %>% 
      select(StateName, y2013, y2023, change, urban_ratio) %>%
      arrange(desc(y2013)),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE) %>%
      DT::formatRound(columns = c("y2013", "y2023", "change", "urban_ratio"), digits = 4)
  })
  
  # Data Explorer - Filtered Data
  filtered_data <- reactive({
    df <- data
    
    # Apply filters
    df <- df %>%
      filter(YEAR >= input$year_filter[1] & YEAR <= input$year_filter[2]) %>%
      filter(digital_index >= input$index_filter[1] & digital_index <= input$index_filter[2])
    
    if (input$metro_filter != "all") {
      df <- df %>% filter(METRO == as.numeric(input$metro_filter))
    }
    
    return(df)
  })
  
  # Data Explorer Table
  output$data_table <- DT::renderDataTable({
    DT::datatable(filtered_data() %>% 
      select(YEAR, COUNTYFIP, STATEICP, METRO, digital_index) %>%
      head(1000),  # Limit to 1000 rows for performance
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE) %>%
      DT::formatRound(columns = "digital_index", digits = 4)
  })
  
  # Summary Statistics - Year Plot
  output$summary_year_plot <- renderPlotly({
    yearly_summary <- data %>%
      filter(!is.na(digital_index)) %>%
      group_by(YEAR) %>%
      summarise(
        mean_index = mean(digital_index, na.rm = TRUE),
        median_index = median(digital_index, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(yearly_summary, aes(x = YEAR)) +
      geom_line(aes(y = mean_index, color = "Mean"), size = 1.2) +
      geom_line(aes(y = median_index, color = "Median"), size = 1.2) +
      geom_point(aes(y = mean_index, color = "Mean"), size = 2) +
      geom_point(aes(y = median_index, color = "Median"), size = 2) +
      labs(
        title = "Digital Index Trends Over Time",
        x = "Year",
        y = "Digital Index",
        color = "Statistic"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))
    
    ggplotly(p)
  })
  
  # Summary Statistics - Distribution Plot
  output$summary_dist_plot <- renderPlotly({
    p <- ggplot(data %>% filter(!is.na(digital_index)), aes(x = digital_index)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
      labs(
        title = "Distribution of Digital Index",
        x = "Digital Index",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))
    
    ggplotly(p)
  })
  
  # Summary Statistics Table
  output$summary_table <- DT::renderDataTable({
    summary_stats <- data %>%
      filter(!is.na(digital_index)) %>%
      group_by(YEAR) %>%
      summarise(
        Count = n(),
        Mean = mean(digital_index, na.rm = TRUE),
        Median = median(digital_index, na.rm = TRUE),
        SD = sd(digital_index, na.rm = TRUE),
        Min = min(digital_index, na.rm = TRUE),
        Max = max(digital_index, na.rm = TRUE),
        .groups = "drop"
      )
    
    DT::datatable(summary_stats,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE) %>%
      DT::formatRound(columns = c("Mean", "Median", "SD", "Min", "Max"), digits = 4)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
