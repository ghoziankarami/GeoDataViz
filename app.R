# app.R
# Version 1.2.4: "CSV-Only Simplification"
# Author: Ghozian Islam Karami
#
# Changelog v1.2.4:
# - SIMPLIFICATION: Removed all UI elements and logic related to Excel file uploads.
# - UI: The data input panel now directly shows CSV file inputs as the only upload option.
# - SERVER: Updated session loading logic to ignore Excel-related inputs.
#
# Changelog v1.2.3:
# - CRITICAL BUG FIX (Force Close): Refactored bivariate regression with `nest()` and `map()`.

# --- 1. Load Libraries ---
library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(ggplot2)
library(rlang)
library(readxl)
library(janitor)
library(shinyjs)
library(RColorBrewer)
library(colourpicker)
library(GGally)
library(ggrepel)
library(purrr)

# --- 2. Load Sample Data with Error Handling ---
required_files <- c("collar.csv", "assay.csv", "lithology.csv")
if (!all(file.exists(required_files))) {
  stop(paste("Error: One or more sample files not found. Please ensure", 
             paste(required_files, collapse=", "), 
             "are in the same directory as app.R"))
}
sample_collar <- read.csv("collar.csv")
sample_assay <- read.csv("assay.csv")
sample_lithology <- read.csv("lithology.csv")

# --- Helper functions for ggpairs plot ---
custom_scatter_with_lm <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = "blue", alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "red", ...)
}

custom_r_squared_text <- function(data, mapping, ...) {
  x_col <- GGally::eval_data_col(data, mapping$x)
  y_col <- GGally::eval_data_col(data, mapping$y)
  correlation <- cor(x_col, y_col, use = "pairwise.complete.obs")
  r_squared <- correlation^2
  x_range <- range(x_col, na.rm = TRUE)
  y_range <- range(y_col, na.rm = TRUE)
  ggplot() + theme_void() + annotate("text", x = mean(x_range), y = mean(y_range), label = paste("R² =", format(r_squared, digits = 2)), size = 5, ...)
}


# --- 3. UI Definition ---
ui <- fluidPage(
  useShinyjs(),
  tags$div(
    style = "position: relative; min-height: 100vh;",
    div(style="padding-bottom: 50px;",
        navbarPage(
          "GeoDataViz v1.2.4", 
          
          tabPanel("Data Input & Integration",
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
                       h3("1. Data Source"),
                       radioButtons("dataSource", "Choose data source:",
                                    choices = c("Use Sample Data" = "sample",
                                                "Upload Your Own Data" = "upload"),
                                    selected = "sample"),
                       hr(),
                       conditionalPanel(
                         condition = "input$dataSource == 'upload'",
                         h3("2. Upload CSV Files"),
                         fileInput("collarFile", "a. Upload Collar File (.csv)", accept = ".csv"),
                         fileInput("assayFile", "b. Upload Assay File (.csv)", accept = ".csv"),
                         fileInput("lithoFile", "c. Upload Lithology File (.csv)", accept = ".csv"),
                         radioButtons("sep", "CSV Separator:", choices = c(Comma = ",", Semicolon = ";"), selected = ";")
                       ),
                       hr(),
                       h3("Session Management (Local Version)"),
                       downloadButton("saveSession", "Save Session (.rds)"),
                       fileInput("loadSession", "Load Session (.rds)", accept = ".rds"),
                       tags$div(
                         class = "well",
                         style = "background-color: #f9f9f9; border: 1px solid #e3e3e3;",
                         h5(strong("Smart Workflow:")),
                         tags$ol(
                           style="padding-left: 20px;",
                           tags$li("Load your data files (Collar, Assay, Litho)."),
                           tags$li("Then, upload your saved `.rds` session file."),
                           tags$li("The app will automatically restore everything, including column definitions.")
                         )
                       )
                     ),
                     mainPanel(
                       width = 8,
                       wellPanel(
                         style = "background-color: #f0f8ff;",
                         h3("Data Security is a Priority"),
                         tags$ul(
                           tags$li(strong("No Storage:"), "This application DOES NOT store your uploaded data."),
                           tags$li(strong("Session-Based:"), "Your data is processed only in the server's memory during your active session."),
                           tags$li(strong("No Database:"), "This application is not connected to any database to store your information.")
                         ),
                         hr(),
                         p(strong("Author:"), "Ghozian Islam Karami"),
                         p(strong("Contact:"), "ghoziankarami@gmail.com | ", 
                           tags$a(href="https://www.linkedin.com/in/ghoziankarami/", target="_blank", "LinkedIn"))
                       ),
                       hr(),
                       h3("Combined Data Preview"),
                       DTOutput("combinedDataTable")
                     )
                   )
          ),
          tabPanel("Column Definitions",
                   fluidPage(
                     h3("Define Column Names"),
                     p("Select the appropriate column names from your uploaded data. Defaults are set for the sample data."),
                     hr(),
                     fluidRow(
                       column(4, h4("Collar Data"), 
                              selectInput("col_collar_holeid", "Hole ID Column:", choices = NULL),
                              selectInput("col_x", "X (Easting) Column:", choices = NULL),
                              selectInput("col_y", "Y (Northing) Column:", choices = NULL),
                              selectInput("col_z", "Z (RL) Column:", choices = NULL)
                       ),
                       column(4, h4("Assay Data"), 
                              selectInput("col_assay_holeid", "Hole ID Column:", choices = NULL),
                              selectInput("col_assay_from", "From Column:", choices = NULL),
                              selectInput("col_assay_to", "To Column:", choices = NULL)
                       ),
                       column(4, h4("Lithology Data"), 
                              selectInput("col_litho_holeid", "Hole ID Column:", choices = NULL),
                              selectInput("col_litho_from", "From Column:", choices = NULL),
                              selectInput("col_litho_to", "To Column:", choices = NULL),
                              selectInput("col_litho", "Lithology Column:", choices = NULL)
                       )
                     )
                   )
          ),
          
          tabPanel("Data Validation Summary",
                   fluidPage(
                     titlePanel("Data Validation Health Check"),
                     hr(),
                     fluidRow(
                       column(4, 
                              h4("File Record Counts"),
                              tableOutput("fileCountsTable")
                       ),
                       column(4,
                              h4("Collars Missing Assay Data"),
                              p("Hole IDs in Collar file, but not in Assay file."),
                              DTOutput("missingAssayTable")
                       ),
                       column(4,
                              h4("Assays Missing Collar Data"),
                              p("Hole IDs in Assay file, but not in Collar file."),
                              DTOutput("missingCollarTable")
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(12,
                              h4("Assay Interval Errors (Gaps / Overlaps)"),
                              p("This table shows intervals where the 'From' value does not match the previous 'To' value for the same Hole ID."),
                              DTOutput("intervalErrorsTable")
                       )
                     )
                   )
          ),
          
          tabPanel("Drillhole Location Map",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("Map Settings"),
                       uiOutput("mapColorSelectUI"),
                       sliderInput("markerSize", "Adjust Marker Size:", min = 1, max = 20, value = 8, step = 1),
                       hr(),
                       h4("Color Scale Settings"),
                       selectInput("scale_type", "Color Scale Type:",
                                   choices = c("Continuous" = "continuous", "Discrete" = "discrete")),
                       conditionalPanel(
                         condition = "input$scale_type == 'continuous'",
                         selectInput("continuous_palette", "Continuous Color Scale:",
                                     choices = c("Viridis", "Plasma", "Inferno", "Magma", "Cividis"),
                                     selected = "Viridis")
                       ),
                       conditionalPanel(
                         condition = "input$scale_type == 'discrete'",
                         selectInput("discrete_palette", "Discrete Color Palette:",
                                     choices = list(
                                       "Sequential" = c("YlOrRd", "YlGnBu", "Blues", "Greens", "Reds"),
                                       "Diverging" = c("RdYlBu", "BrBG", "PiYG"),
                                       "Qualitative" = c("Set1", "Set2", "Paired")
                                     ), selected = "YlOrRd"),
                         textInput("manual_breaks", "Enter Upper Interval Breaks (comma-separated)", placeholder = "e.g., 1, 1.5, 2")
                       )
                     ),
                     mainPanel(
                       width = 9,
                       plotlyOutput("drillholeMap", height = "700px")
                     )
                   )
          ),
          
          tabPanel("Statistical Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("1. Variable Selection"),
                       selectInput("selectedGrades", "Select Grades for Analysis:", choices = NULL, multiple = TRUE),
                       hr(),
                       selectInput("selectedBoxplotGrade", "Select Grade for Boxplot:", choices = NULL)
                     ),
                     mainPanel(
                       width = 9,
                       h3("Summary Statistics"),
                       DTOutput("summaryStatsTable"),
                       checkboxInput("excludeZeros", "Exclude zero values from statistics and plots", value = FALSE),
                       hr(),
                       h3("Grade Distribution"),
                       plotlyOutput("gradeHistogram"),
                       sliderInput("histBins", "Number of Bins:", min = 10, max = 200, value = 30, step = 5),
                       hr(),
                       h3("Grade Distribution by Lithology"),
                       plotlyOutput("lithologyHistograms", height="600px"),
                       hr(),
                       h3("Boxplot by Lithology"),
                       plotlyOutput("lithologyBoxplot"),
                       checkboxInput("zoomBoxplot", "Zoom to Interquartile Range (IQR)", value = FALSE),
                       hr(),
                       h3("Outlier Analysis"),
                       p("The table below lists data points identified as outliers (values beyond 1.5x the Interquartile Range)."),
                       uiOutput("outlierSummaryStatsUI"),
                       hr(),
                       DTOutput("outlierTable"),
                       hr(),
                       h4("Interactive Outlier Removal"),
                       p("Select rows from the table above, then click the button below to see the effect on summary statistics."),
                       actionButton("removeOutliers", "Remove Selected Outliers"),
                       hr(),
                       uiOutput("comparisonUI"),
                       uiOutput("downloadCleanedDataUI"),
                       hr(),
                       h4("Top Cut Analysis (Capping)"),
                       p("Enter a value to cap the selected grade and see the effect on summary statistics."),
                       numericInput("topCutValue", "Enter Top Cut Value:", value = NULL),
                       actionButton("applyTopCut", "Apply Top Cut"),
                       hr(),
                       uiOutput("topCutComparisonUI")
                     )
                   )
          ),
          
          tabPanel("Bivariate Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("Select Variables"),
                       uiOutput("bivariateXSelectUI"),
                       uiOutput("bivariateYSelectUI"),
                       hr(),
                       h4("Multivariate Variables"),
                       uiOutput("multivariateSelectUI")
                     ),
                     mainPanel(
                       width = 9,
                       tabsetPanel(
                         id = "bivariate_tabs",
                         tabPanel("Single Pair Regression",
                                  h3("Bivariate Regression Analysis by Lithology"),
                                  plotlyOutput("regressionPlot"),
                                  hr(),
                                  h4("R-squared Summary by Lithology"),
                                  DTOutput("regressionSummaryTable")
                         ),
                         tabPanel("Multivariate Matrix Plot",
                                  h3("Multivariate Scatter Plot Matrix"),
                                  p("This plot may take a few moments to render depending on the number of variables selected."),
                                  plotOutput("multivariatePlot", height = "800px")
                         )
                       )
                     )
                   )
          ),
          
          tabPanel("Downhole Plot",
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
                       uiOutput("holeSelectInput"),
                       hr(),
                       h4("Select Columns to Display (multiple allowed):"),
                       uiOutput("downholeColumnSelectUI")
                     ),
                     mainPanel(
                       width = 8,
                       plotlyOutput("downholePlot", height = "800px")
                     )
                   )
          ),
          
          tabPanel("Lithology Color Settings",
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
                       h3("Lithology Color Settings"),
                       helpText("Select a lithology to change its color, then pick a new color. The change is applied automatically."),
                       hr(),
                       uiOutput("selectLithoToColorUI"),
                       uiOutput("pickNewColorUI")
                     ),
                     mainPanel(
                       width = 8,
                       h4("Current Color Preview"),
                       DTOutput("lithoColorPreviewTable")
                     )
                   )
          ),
          
          tabPanel("About",
                   fluidPage(
                     fluidRow(
                       column(8,
                              h3("About GeoDataViz"),
                              p(strong("GeoDataViz"), " is a comprehensive, interactive web application built with R Shiny, designed as an all-in-one workbench for geologists, data analysts, and students. It transforms raw drillhole data (collar, assay, lithology) into actionable geological insights through powerful visualization, robust statistical analysis, and interactive data validation tools."),
                              p("Developed by a Senior Geologist for both educational and professional use, this application provides an open-source solution for the entire initial data analysis workflow, from data loading and cleaning to advanced QA/QC, without the need for proprietary software."),
                              hr(),
                              h3("Target Audience"),
                              tags$ul(
                                tags$li(strong("Resource & Exploration Geologists")),
                                tags$li(strong("Mine Geologists & Grade Control Engineers")),
                                tags$li(strong("Geoscience Students & Academics")),
                                tags$li(strong("Data Analysts in the Mining Sector"))
                              ),
                              hr(),
                              h3("License"),
                              p("This project is licensed under the MIT License. Source code is available on ", tags$a(href="https://github.com/ghoziankarami/GeoDataViz/", target="_blank", "GitHub.")),
                              hr(),
                              h3("Citation"),
                              p("The open-source version of this software is citable via Zenodo."),
                              wellPanel(
                                style="background-color: #f9f9f9; border-color: #eee; word-wrap: break-word;",
                                tags$p("ghoziankarami. (2025). ghoziankarami/GeoDataViz: v1.0.0 - Public Release (v1.0.0). Zenodo. ", tags$a(href="https://doi.org/10.5281/zenodo.17142676", target="_blank", "https://doi.org/10.5281/zenodo.17142676"))
                              )
                       ),
                       column(4,
                              wellPanel(
                                style = "margin-top: 20px;",
                                h4("Support the Project"),
                                p("For inquiries, contact the author via email:"),
                                p(strong("ghoziankarami@gmail.com"))
                              )
                       )
                     )
                   )
          )
        )
    ),
    tags$footer(
      style="position: absolute; bottom: 0; width: 100%; height: 50px; color: white; background-color: #333; text-align: center; padding: 15px;",
      "Developed by Ghozian Islam Karami"
    )
  )
)

# --- 4. Server Logic ---
server <- function(input, output, session) {
  
  useShinyjs()
  
  rv <- reactiveValues(
    litho_colors = setNames(character(0), character(0)),
    removed_outlier_indices = NULL
  )
  
  # Reactive Data Pipeline
  collar_data_raw <- reactive({
    if (input$dataSource == "sample") return(sample_collar)
    req(input$collarFile); read.csv(input$collarFile$datapath, sep = input$sep, stringsAsFactors = FALSE, check.names = FALSE)
  })
  assay_data_raw <- reactive({
    if (input$dataSource == "sample") return(sample_assay)
    req(input$assayFile); read.csv(input$assayFile$datapath, sep = input$sep, stringsAsFactors = FALSE, check.names = FALSE)
  })
  litho_data_raw <- reactive({
    if (input$dataSource == "sample") return(sample_lithology)
    req(input$lithoFile); read.csv(input$lithoFile$datapath, sep = input$sep, stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  collar_data <- reactive({ janitor::clean_names(collar_data_raw()) })
  assay_data <- reactive({ janitor::clean_names(assay_data_raw()) })
  litho_data <- reactive({ janitor::clean_names(litho_data_raw()) })
  
  collar_std <- reactive({
    req(collar_data(), input$col_collar_holeid, input$col_x, input$col_y, input$col_z)
    validate(need(all(c(input$col_collar_holeid, input$col_x, input$col_y, input$col_z) %in% names(collar_data())), "One or more collar columns not found."))
    df <- collar_data() %>%
      select(hole_id = .data[[input$col_collar_holeid]], x_coord = .data[[input$col_x]], y_coord = .data[[input$col_y]], z_coord = .data[[input$col_z]]) %>%
      mutate(hole_id = as.character(hole_id))
    if(any(duplicated(df$hole_id))) { showNotification("Warning: Duplicate Hole IDs in collar file.", type = "warning", duration = 15) }
    df %>% distinct(hole_id, .keep_all = TRUE)
  })
  assay_std <- reactive({
    req(assay_data(), input$col_assay_holeid, input$col_assay_from, input$col_assay_to)
    validate(need(all(c(input$col_assay_holeid, input$col_assay_from, input$col_assay_to) %in% names(assay_data())), "One or more assay columns not found."))
    assay_data() %>%
      select(hole_id = .data[[input$col_assay_holeid]], from = .data[[input$col_assay_from]], to = .data[[input$col_assay_to]], everything()) %>%
      mutate(across(where(is.double) | where(is.integer), as.numeric), hole_id = as.character(hole_id), from = as.numeric(from), to = as.numeric(to))
  })
  litho_std <- reactive({
    req(litho_data(), input$col_litho_holeid, input$col_litho_from, input$col_litho_to, input$col_litho)
    validate(need(all(c(input$col_litho_holeid, input$col_litho_from, input$col_litho_to, input$col_litho) %in% names(litho_data())), "One or more lithology columns not found."))
    litho_data() %>%
      select(hole_id = .data[[input$col_litho_holeid]], from = .data[[input$col_litho_from]], to = .data[[input$col_litho_to]], lithology = .data[[input$col_litho]]) %>%
      mutate(hole_id = as.character(hole_id), from = as.numeric(from), to = as.numeric(to), lithology = as.character(lithology))
  })
  
  combinedData <- reactive({
    req(assay_std(), collar_std(), litho_std())
    data_merged_1 <- left_join(assay_std(), collar_std(), by = "hole_id")
    final_data <- left_join(data_merged_1, litho_std(), by = c("hole_id", "from", "to"))
    final_data %>%
      arrange(hole_id, from) %>%
      group_by(hole_id) %>%
      fill(lithology, .direction = "downup") %>%
      ungroup() %>%
      mutate(unique_id = paste(hole_id, from, to, sep = "_"))
  })
  
  analysisData <- reactive({
    req(combinedData())
    data <- combinedData()
    if (input$excludeZeros) {
      grades_to_check <- isolate(numeric_assay_cols())
      data <- data %>%
        mutate(across(all_of(grades_to_check), ~if_else(. == 0, NA_real_, .)))
    }
    return(data)
  })
  
  # UI Observers & Updaters
  find_col <- function(possible_names, actual_names) { intersect(possible_names, actual_names)[1] }
  observe({ req(collar_data()); cols <- names(collar_data()); updateSelectInput(session, "col_collar_holeid", choices = cols, selected = find_col(c("hole_id", "holeid", "bhid", "hole"), cols)); updateSelectInput(session, "col_x", choices = cols, selected = find_col(c("x", "easting", "east"), cols)); updateSelectInput(session, "col_y", choices = cols, selected = find_col(c("y", "northing", "north"), cols)); updateSelectInput(session, "col_z", choices = cols, selected = find_col(c("z", "rl", "elevation", "depth"), cols)) })
  observe({ req(assay_data()); cols <- names(assay_data()); updateSelectInput(session, "col_assay_holeid", choices = cols, selected = find_col(c("hole_id", "holeid", "bhid", "hole"), cols)); updateSelectInput(session, "col_assay_from", choices = cols, selected = find_col(c("from", "dari"), cols)); updateSelectInput(session, "col_assay_to", choices = cols, selected = find_col(c("to", "sampai"), cols)) })
  observe({ req(litho_data()); cols <- names(litho_data()); updateSelectInput(session, "col_litho_holeid", choices = cols, selected = find_col(c("hole_id", "holeid", "bhid", "hole"), cols)); updateSelectInput(session, "col_litho_from", choices = cols, selected = find_col(c("from", "dari"), cols)); updateSelectInput(session, "col_litho_to", choices = cols, selected = find_col(c("to", "sampai"), cols)); updateSelectInput(session, "col_litho", choices = cols, selected = find_col(c("lithology", "litho_code", "litho", "rock", "deskripsi"), cols)) })
  numeric_assay_cols <- reactive({ req(assay_std()); assay_std() %>% select(where(is.numeric), -any_of(c("from", "to", "x_coord", "y_coord", "z_coord"))) %>% names() })
  observe({ choices <- numeric_assay_cols(); updateSelectInput(session, "selectedGrades", choices = choices, selected = choices[1]) })
  observe({ req(input$selectedGrades); updateSelectInput(session, "selectedBoxplotGrade", choices = input$selectedGrades, selected = input$selectedGrades[1]) })
  
  # Session Management Logic
  output$saveSession <- downloadHandler(
    filename = function() { paste0("geodataviz-session-", Sys.Date(), ".rds") },
    content = function(file) {
      inputs_to_save <- reactiveValuesToList(input); inputs_to_save <- inputs_to_save[!grepl("File|loadSession", names(inputs_to_save))]
      session_data <- list(inputs = inputs_to_save, reactives = reactiveValuesToList(rv)); saveRDS(session_data, file = file)
      showNotification("Session settings saved successfully!", type = "message")
    }
  )
  observeEvent(input$loadSession, {
    req(input$loadSession)
    tryCatch({
      session_data <- readRDS(input$loadSession$datapath)
      if (!is.null(session_data$reactives)) { for (name in names(session_data$reactives)) { rv[[name]] <- session_data$reactives[[name]] } }
      shinyjs::delay(500, {
        if (!is.null(session_data$inputs)) {
          saved_inputs <- session_data$inputs
          update_input <- function(type, id) { if (id %in% names(saved_inputs) && !is.null(saved_inputs[[id]])) { value <- saved_inputs[[id]]; try({ switch(type, select = updateSelectInput(session, id, selected = value), radio = updateRadioButtons(session, id, selected = value), slider = updateSliderInput(session, id, value = value), text = updateTextInput(session, id, value = value), numeric = updateNumericInput(session, id, value = value), checkbox = updateCheckboxInput(session, id, value = value)) }, silent = TRUE) } }
          sapply(c("dataSource", "sep"), function(id) update_input("radio", id))
          sapply(c("col_collar_holeid", "col_x", "col_y", "col_z", "col_assay_holeid", "col_assay_from", "col_assay_to", "col_litho_holeid", "col_litho_from", "col_litho_to", "col_litho", "scale_type", "continuous_palette", "discrete_palette", "selectedGrades", "selectedBoxplotGrade", "mapColorColumn", "bivariateX", "bivariateY", "multivariateVars", "selectedHole", "selectedDownholeCols", "selectedLithoColor"), function(id) update_input("select", id))
          sapply(c("markerSize", "histBins"), function(id) update_input("slider", id))
          sapply(c("manual_breaks"), function(id) update_input("text", id))
          sapply(c("topCutValue"), function(id) update_input("numeric", id))
          sapply(c("excludeZeros", "zoomBoxplot"), function(id) update_input("checkbox", id))
        }
        showNotification("Session loaded successfully!", type = "message")
      })
    }, error = function(e) { showNotification(paste("Failed to load session:", e$message), type = "error") })
  })
  
  # ... [All other output renders remain the same] ...
  
  output$combinedDataTable <- renderDT({ validate(need(combinedData(), "Processing data...")); datatable(head(combinedData(), 100), options = list(pageLength = 10, scrollX = TRUE, scrollCollapse = TRUE)) })
  output$fileCountsTable <- renderTable({ req(collar_data(), assay_data(), litho_data()); data.frame(File = c("Collar", "Assay", "Lithology"), Records = c(nrow(collar_data()), nrow(assay_data()), nrow(litho_data()))) })
  missing_assay_data <- reactive({ req(collar_std(), assay_std()); anti_join(collar_std(), assay_std(), by = "hole_id") })
  missing_collar_data <- reactive({ req(collar_std(), assay_std()); anti_join(assay_std(), collar_std(), by = "hole_id") %>% distinct(hole_id, .keep_all = TRUE) })
  output$missingAssayTable <- renderDT({ df <- missing_assay_data(); if (nrow(df) > 0) { datatable(df, options = list(pageLength = 3, scrollX = TRUE)) } else { datatable(data.frame(Status = "No missing assay data found."), options = list(dom = 't')) } })
  output$missingCollarTable <- renderDT({ df <- missing_collar_data(); if (nrow(df) > 0) { datatable(df %>% select(hole_id), options = list(pageLength = 3, scrollX = TRUE)) } else { datatable(data.frame(Status = "No missing collar data found."), options = list(dom = 't')) } })
  interval_error_data <- reactive({ req(assay_std()); assay_std() %>% arrange(hole_id, from) %>% group_by(hole_id) %>% mutate(prev_to = lag(to)) %>% ungroup() %>% filter(!is.na(prev_to) & from != prev_to) %>% select(hole_id, prev_to, from, to) })
  output$intervalErrorsTable <- renderDT({ df <- interval_error_data(); if (nrow(df) > 0) { datatable(df, options = list(pageLength = 5, scrollX = TRUE)) } else { datatable(data.frame(Status = "No interval errors detected."), options = list(dom = 't')) } })
  output$mapColorSelectUI <- renderUI({ selectInput("mapColorColumn", "Color Map by Average:", choices = c("None" = "", numeric_assay_cols())) })
  output$drillholeMap <- renderPlotly({
    validate(need(analysisData(), "Please upload files to view the map.")); plot_data <- collar_std(); p <- plot_ly()
    if (!is.null(input$mapColorColumn) && input$mapColorColumn != "") { avg_grades <- analysisData() %>% group_by(hole_id) %>% summarise(avg_grade = mean(.data[[input$mapColorColumn]], na.rm = TRUE), .groups = 'drop'); plot_data <- left_join(plot_data, avg_grades, by = "hole_id"); data_with_color <- plot_data %>% filter(!is.na(avg_grade)); data_without_color <- plot_data %>% filter(is.na(avg_grade)); if(nrow(data_without_color) > 0){ p <- p %>% add_markers(data = data_without_color, x = ~x_coord, y = ~y_coord, type = 'scatter', mode = 'markers', marker = list(size = input$markerSize, color = 'lightgrey'), name = "No Data", text = ~paste("Hole ID:", hole_id), hoverinfo = 'text') }; if(nrow(data_with_color) > 0) { if (input$scale_type == "continuous") { p <- p %>% add_markers(data = data_with_color, x = ~x_coord, y = ~y_coord, type = 'scatter', mode = 'markers', marker = list(size = input$markerSize, color = ~avg_grade, colorscale = input$continuous_palette, showscale = TRUE, colorbar = list(title = paste("Avg.", input$mapColorColumn))), name = "With Data", text = ~paste("Hole ID:", hole_id, "<br>Avg.", input$mapColorColumn, ":", round(avg_grade, 3)), hoverinfo = "text") } else { req(input$manual_breaks, input$discrete_palette); validate(need(input$manual_breaks != "", "Please enter interval breaks.")); breaks_num <- as.numeric(trimws(unlist(strsplit(input$manual_breaks, ",")))); breaks_num <- sort(na.omit(breaks_num)); validate(need(length(breaks_num) > 0, "Please enter valid interval breaks.")); breaks_vec_inf <- c(-Inf, breaks_num, Inf); labels <- sapply(1:(length(breaks_vec_inf)-1), function(i) { if (i == 1) return(paste("<=", format(breaks_vec_inf[i+1], nsmall=2))); if (i == length(breaks_vec_inf)-1) return(paste(">", format(breaks_vec_inf[i], nsmall=2))); return(paste(format(breaks_vec_inf[i], nsmall=2), "-", format(breaks_vec_inf[i+1], nsmall=2))) }); data_with_color$grade_category <- cut(data_with_color$avg_grade, breaks = breaks_vec_inf, labels = labels, right = FALSE, include.lowest = TRUE); num_colors <- length(labels); palette_colors <- RColorBrewer::brewer.pal(max(3, min(num_colors, 9)), input$discrete_palette); if(num_colors > length(palette_colors)) { palette_colors <- colorRampPalette(palette_colors)(num_colors) } else { palette_colors <- palette_colors[1:num_colors] }; p <- p %>% add_markers(data = data_with_color, x = ~x_coord, y = ~y_coord, type = 'scatter', mode = 'markers', color = ~grade_category, colors = palette_colors, marker = list(size = input$markerSize), text = ~paste("Hole ID:", hole_id, "<br>Avg.", input$mapColorColumn, ":", round(avg_grade, 3)), hoverinfo = "text") }}
    } else { p <- p %>% add_markers(data = plot_data, x = ~x_coord, y = ~y_coord, type = 'scatter', mode = 'markers', marker = list(size = input$markerSize), text = ~paste("Hole ID:", hole_id), hoverinfo = 'text') }
    p %>% layout(title = "Interactive Drillhole Location Map", xaxis = list(title = "X Coordinate"), yaxis = list(title = "Y Coordinate", scaleanchor = "x", scaleratio = 1), showlegend = TRUE)
  })
  output$summaryStatsTable <- renderDT({
    req(analysisData(), input$selectedGrades); validate(need(length(input$selectedGrades) > 0, "Please select at least one grade."))
    summary_df <- analysisData() %>% select(all_of(input$selectedGrades)) %>% pivot_longer(everything(), names_to = "Grade", values_to = "Value") %>% group_by(Grade) %>% summarise(Mean = round(mean(Value, na.rm = TRUE), 3), Median = round(median(Value, na.rm = TRUE), 3), `Std Dev` = round(sd(Value, na.rm = TRUE), 3), CV = round(sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE), 3), Min = round(min(Value, na.rm = TRUE), 3), Max = round(max(Value, na.rm = TRUE), 3), Count = sum(!is.na(Value))); datatable(summary_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$gradeHistogram <- renderPlotly({
    req(analysisData(), input$selectedGrades, input$histBins); data_plot <- analysisData() %>% select(all_of(input$selectedGrades)) %>% pivot_longer(everything(), names_to = "Grade", values_to = "Value") %>% filter(!is.na(Value))
    plot_ly(data_plot, x = ~Value, color = ~Grade, type = "histogram", nbinsx = input$histBins, alpha = 0.7, colors = "viridis") %>% layout(title = "Grade Distribution Histogram", xaxis = list(title = "Value"), yaxis = list(title = "Frequency"), barmode = "overlay")
  })
  output$lithologyHistograms <- renderPlotly({
    req(analysisData(), input$selectedBoxplotGrade, input$histBins); p <- ggplot(analysisData(), aes(x = .data[[input$selectedBoxplotGrade]])) + geom_histogram(bins = input$histBins, fill = "skyblue", color = "black") + facet_wrap(~ lithology, scales = "free") + theme_minimal() + labs(title = paste("Distribution of", input$selectedBoxplotGrade, "by Lithology"), x = input$selectedBoxplotGrade, y = "Frequency"); ggplotly(p)
  })
  output$lithologyBoxplot <- renderPlotly({
    req(analysisData(), input$selectedBoxplotGrade, rv$litho_colors); p <- ggplot(analysisData(), aes(x = lithology, y = .data[[input$selectedBoxplotGrade]], fill = lithology)) + geom_boxplot() + scale_fill_manual(values = rv$litho_colors) + labs(title = paste("Boxplot of", input$selectedBoxplotGrade, "by Lithology"), x = "Lithology", y = "Value") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    if (input$zoomBoxplot) { stats <- analysisData() %>% filter(!is.na(.data[[input$selectedBoxplotGrade]])) %>% summarise(Q1 = quantile(.data[[input$selectedBoxplotGrade]], 0.25, na.rm = TRUE), Q3 = quantile(.data[[input$selectedBoxplotGrade]], 0.75, na.rm = TRUE)) %>% mutate(IQR = Q3-Q1); lower_bound <- stats$Q1 - 1.5 * stats$IQR; upper_bound <- stats$Q3 + 1.5 * stats$IQR; p <- p + coord_cartesian(ylim = c(lower_bound, upper_bound)) }; ggplotly(p)
  })
  outlier_data <- reactive({
    req(analysisData(), input$selectedBoxplotGrade); grade_col <- sym(input$selectedBoxplotGrade); stats <- analysisData() %>% filter(!is.na(!!grade_col)) %>% summarise(Q1 = quantile(!!grade_col, 0.25, na.rm = TRUE), Q3 = quantile(!!grade_col, 0.75, na.rm = TRUE)) %>% mutate(IQR = Q3 - Q1); lower_bound <- stats$Q1 - 1.5 * stats$IQR; upper_bound <- stats$Q3 + 1.5 * stats$IQR; analysisData() %>% filter((!!grade_col < lower_bound | !!grade_col > upper_bound) & !is.na(!!grade_col)) %>% select(unique_id, hole_id, from, to, lithology, !!grade_col)
  })
  output$outlierSummaryStatsUI <- renderUI({ req(outlier_data(), input$selectedBoxplotGrade); df <- outlier_data(); if (nrow(df) > 0) { grade_col <- input$selectedBoxplotGrade; summary_stats <- df %>% summarise(Count = n(), Average = round(mean(.data[[grade_col]], na.rm = TRUE), 3), Min = round(min(.data[[grade_col]], na.rm = TRUE), 3), Max = round(max(.data[[grade_col]], na.rm = TRUE), 3)); wellPanel(h4("Outlier Statistics"), p(paste("Count:", summary_stats$Count)), p(paste("Average:", summary_stats$Average)), p(paste("Min:", summary_stats$Min, " | Max:", summary_stats$Max))) } })
  output$outlierTable <- renderDT({ datatable(outlier_data(), options = list(pageLength = 5, scrollX = TRUE), selection = 'multiple') })
  observeEvent(input$removeOutliers, { req(input$outlierTable_rows_selected); selected_ids <- outlier_data()[input$outlierTable_rows_selected, ]$unique_id; rv$removed_outlier_indices <- unique(c(rv$removed_outlier_indices, selected_ids)) })
  cleaned_data <- reactive({ analysisData() %>% filter(!unique_id %in% rv$removed_outlier_indices) })
  output$comparisonUI <- renderUI({ req(rv$removed_outlier_indices); grade_col <- sym(input$selectedBoxplotGrade); original_summary <- analysisData() %>% summarise(Statistic = c("Count", "Mean", "Std Dev", "Min", "Max"), Value = c(n(), mean(!!grade_col, na.rm=T), sd(!!grade_col, na.rm=T), min(!!grade_col, na.rm=T), max(!!grade_col, na.rm=T))); cleaned_summary <- cleaned_data() %>% summarise(Statistic = c("Count", "Mean", "Std Dev", "Min", "Max"), Value = c(n(), mean(!!grade_col, na.rm=T), sd(!!grade_col, na.rm=T), min(!!grade_col, na.rm=T), max(!!grade_col, na.rm=T))); fluidRow(column(6, h5("Original Data Summary"), renderTable(original_summary, digits=3)), column(6, h5("Summary After Removal"), renderTable(cleaned_summary, digits=3))) })
  output$downloadCleanedDataUI <- renderUI({ req(rv$removed_outlier_indices); downloadButton("downloadCleaned", "Download Cleaned Data (.csv)") })
  output$downloadCleaned <- downloadHandler(filename = function() { paste("cleaned_data-", Sys.Date(), ".csv", sep = "") }, content = function(file) { write.csv(cleaned_data() %>% select(-unique_id), file, row.names = FALSE) })
  
  output$bivariateXSelectUI <- renderUI({ selectInput("bivariateX", "Select X-axis Variable:", choices = numeric_assay_cols()) })
  output$bivariateYSelectUI <- renderUI({ choices <- numeric_assay_cols(); selected_y <- if(length(choices) > 1) choices[2] else choices[1]; selectInput("bivariateY", "Select Y-axis Variable:", choices = choices, selected = selected_y) })
  output$multivariateSelectUI <- renderUI({ selectInput("multivariateVars", "Select Variables for Matrix:", choices = numeric_assay_cols(), selected = head(numeric_assay_cols(), 4), multiple = TRUE) })
  
  regression_summary_data <- reactive({
    req(analysisData(), input$bivariateX, input$bivariateY); df <- analysisData() %>% filter(!is.na(.data[[input$bivariateX]]) & !is.na(.data[[input$bivariateY]])); x_var_str <- input$bivariateX; y_var_str <- input$bivariateY; formula <- as.formula(paste0("`", y_var_str, "` ~ `", x_var_str, "`"))
    overall_model <- lm(formula, data = df); overall_r2 <- summary(overall_model)$r.squared; overall_eq <- paste0("y = ", round(coef(overall_model)[2], 3), "x + ", round(coef(overall_model)[1], 3))
    by_litho <- df %>% group_by(lithology) %>% filter(n() > 5) %>% nest() %>% mutate(model = map(data, ~lm(formula, data = .x)), summary = map(model, summary), r_squared = map_dbl(summary, "r.squared"), label_x = map_dbl(data, ~median(.[[x_var_str]], na.rm = TRUE)), label_y = map2_dbl(model, label_x, ~predict(.x, newdata = setNames(data.frame(.y), x_var_str)))) %>% mutate(r_squared_label = paste("R² =", round(r_squared, 2))) %>% arrange(desc(r_squared))
    list(overall = list(r2 = overall_r2, eq = overall_eq, model = overall_model), by_litho = by_litho)
  })
  output$regressionPlot <- renderPlotly({
    req(analysisData(), rv$litho_colors, regression_summary_data()); reg_data <- regression_summary_data(); summary_stats <- reg_data$overall
    p <- ggplot(analysisData(), aes(x = .data[[input$bivariateX]], y = .data[[input$bivariateY]])) + geom_point(aes(color = lithology), alpha = 0.6) + geom_smooth(aes(color = lithology, group = lithology), method = "lm", se = FALSE) + geom_abline(intercept = coef(summary_stats$model)[1], slope = coef(summary_stats$model)[2], color = "black", linetype = "dashed", size = 1) + ggrepel::geom_label_repel(data = reg_data$by_litho, aes(x = label_x, y = label_y, label = r_squared_label, color = lithology), fontface = "bold", show.legend = FALSE, segment.alpha = 0.5, box.padding = 0.5) + scale_color_manual(values = rv$litho_colors) + labs(title = paste("Regression of", input$bivariateY, "vs.", input$bivariateX), x = input$bivariateX, y = input$bivariateY) + theme_minimal() + annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 2, label = paste0("Overall R² = ", round(summary_stats$r2, 2), "\n", summary_stats$eq), size = 4, fontface = "bold")
    ggplotly(p)
  })
  output$regressionSummaryTable <- renderDT({ table_data <- regression_summary_data()$by_litho %>% select(lithology, r_squared); datatable(table_data, options = list(pageLength = 5)) })
  output$multivariatePlot <- renderPlot({
    req(analysisData(), input$multivariateVars); validate(need(length(input$multivariateVars) >= 2, "Please select at least two variables."))
    data_for_plot <- analysisData() %>% select(all_of(input$multivariateVars)); ggpairs(data_for_plot, upper = list(continuous = custom_scatter_with_lm), lower = list(continuous = custom_r_squared_text), diag = list(continuous = "densityDiag")) + theme_minimal()
  })
  
  output$holeSelectInput <- renderUI({ req(collar_std()); hole_ids <- unique(collar_std()$hole_id); selectInput("selectedHole", "Select Hole ID:", choices = sort(hole_ids), selected = hole_ids[1]) })
  output$downholeColumnSelectUI <- renderUI({ req(assay_std()); exclude_cols <- c("hole_id", "from", "to", "x_coord", "y_coord", "z_coord"); choices <- setdiff(names(assay_std()), exclude_cols); selectizeInput("selectedDownholeCols", NULL, choices = choices, selected = choices[1], multiple = TRUE) })
  output$downholePlot <- renderPlotly({
    req(analysisData(), input$selectedHole, rv$litho_colors); validate(need(!is.null(input$selectedDownholeCols) && length(input$selectedDownholeCols) > 0, "Please select at least one data column."))
    hole_data <- analysisData() %>% filter(hole_id == input$selectedHole) %>% arrange(from); validate(need(nrow(hole_data) > 0, "No data found for this Hole ID."))
    p_litho <- ggplot(hole_data) + geom_rect(aes(xmin = 0, xmax = 1, ymin = from, ymax = to, fill = lithology, text = paste("From:", from, "<br>To:", to, "<br>Litho:", lithology))) + scale_y_reverse(name = "Depth (m)") + scale_fill_manual(values = rv$litho_colors) + theme_minimal() + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())
    plot_list <- list(ggplotly(p_litho, tooltip = "text"))
    for (col_name in input$selectedDownholeCols) {
      if(!col_name %in% names(hole_data)) next
      selected_col_sym <- sym(col_name)
      if (is.numeric(hole_data[[col_name]])) {
        p_data <- ggplot(hole_data, aes(y = from, text = paste("From:", from, "<br>To:", to, "<br>", col_name, ":", !!selected_col_sym))) + geom_rect(aes(xmin = 0, xmax = !!selected_col_sym, ymin = from, ymax = to), fill = "skyblue", color = "black", alpha = 0.7) + scale_y_reverse() + theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + labs(x = col_name)
      } else {
        p_data <- ggplot(hole_data) + geom_rect(aes(xmin = 0, xmax = 1, ymin = from, ymax = to), fill = "gray95", color = "gray80") + geom_text(aes(x = 0.5, y = (from + to) / 2, label = !!selected_col_sym, text = paste("From:", from, "<br>To:", to, "<br>", !!selected_col_sym)), check_overlap = TRUE, size = 3) + scale_y_reverse() + theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())
      }
      plot_list <- append(plot_list, list(ggplotly(p_data, tooltip = "text")))
    }
    num_plots <- length(plot_list)
    if (num_plots > 1) { widths <- c(0.25, rep(0.75 / (num_plots - 1), num_plots - 1)) } else { widths <- c(1) }
    subplot(plot_list, nrows = 1, shareY = TRUE, titleX = TRUE, widths = widths) %>% layout(title = paste("Downhole Plot for", input$selectedHole), margin = list(t = 80))
  })
  
  observe({
    req(combinedData()); unique_lithos <- unique(combinedData()$lithology); unique_lithos <- unique_lithos[!is.na(unique_lithos)]
    if (!setequal(names(rv$litho_colors), unique_lithos)) {
      num_lithos <- length(unique_lithos)
      if (num_lithos > 0) {
        palette <- RColorBrewer::brewer.pal(min(num_lithos, 9), "Set1")
        if(num_lithos > length(palette)) { colors <- colorRampPalette(palette)(num_lithos) } else { colors <- palette[1:num_lithos] }
        rv$litho_colors <- setNames(colors, unique_lithos)
      } else {
        rv$litho_colors <- setNames(character(0), character(0))
      }
    }
  })
  output$selectLithoToColorUI <- renderUI({ req(length(rv$litho_colors) > 0); selectInput("selectedLithoColor", "1. Select Lithology to Color:", choices = names(rv$litho_colors)) })
  output$pickNewColorUI <- renderUI({ req(input$selectedLithoColor); colourpicker::colourInput("newLithoColor", "2. Pick a New Color:", value = rv$litho_colors[input$selectedLithoColor]) })
  observeEvent(input$newLithoColor, { req(input$selectedLithoColor); rv$litho_colors[input$selectedLithoColor] <- input$newLithoColor })
  output$lithoColorPreviewTable <- renderDT({
    req(length(rv$litho_colors) > 0)
    color_df <- data.frame(Lithology = names(rv$litho_colors), Color = unname(rv$litho_colors)) %>% mutate(Preview = paste0('<span style="display: inline-block; width: 100%; height: 20px; background-color:', Color, '; border: 1px solid #ccc;"></span>'))
    datatable(color_df, escape = FALSE, options = list(pageLength = 10, autoWidth = TRUE, dom = 't'), rownames = FALSE)
  })
}

# --- 5. Run Application ---
shinyApp(ui, server)