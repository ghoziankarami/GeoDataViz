# app.R
# Version 10.2: "New Sample Data Build"
# Author: Ghozian Islam Karami
#
# Changelog v10.2:
# - SAMPLE DATA REPLACED:
#   - The application now loads the new, stable set of collar, assay, and litho
#     CSV files provided by the user by default.
# - SURVEY DATA ADDED:
#   - The provided survey.csv is now also loaded and displayed in its own
#     preview table on the first tab for complete data visibility.
# - DEFAULT COLUMNS UPDATED:
#   - The default selections in the "Column Definitions" tab have been updated
#     to perfectly match the headers in the new sample files, making the app
#     immediately functional on startup.

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

# --- 2. Load Sample Data with Error Handling ---
# This version uses the new, stable dataset provided by the user.
required_files <- c("collar.csv", "assay.csv", "lithology.csv", "survey.csv")
if (!all(file.exists(required_files))) {
  stop(paste("Error: One or more sample files not found. Please ensure", 
             paste(required_files, collapse=", "), 
             "are in the same directory as app.R"))
}
sample_collar <- read.csv("collar.csv")
sample_assay <- read.csv("assay.csv")
sample_lithology <- read.csv("lithology.csv")
sample_survey <- read.csv("survey.csv")


# --- 3. UI Definition ---
ui <- fluidPage(
  tags$div(
    style = "position: relative; min-height: 100vh;",
    div(style="padding-bottom: 50px;",
        navbarPage(
          "Geological Drillhole Data Analyzer v10.2",
          
          # Tab 1: Data Input & Integration
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
                       # Conditional panel for uploading user's own data
                       conditionalPanel(
                         condition = "input.dataSource == 'upload'",
                         h3("2. Upload Files"),
                         selectInput("inputType", "Select Input Format:",
                                     choices = c("Excel (Single File)" = "excel",
                                                 "CSV (Multiple Files)" = "csv")),
                         hr(),
                         conditionalPanel(
                           condition = "input.inputType == 'excel'",
                           fileInput("excelFile", "Upload Excel File (.xlsx)", accept = c(".xlsx")),
                           hr(),
                           h3("Select Sheets"),
                           uiOutput("collarSheetUI"),
                           uiOutput("assaySheetUI"),
                           uiOutput("lithoSheetUI")
                         ),
                         conditionalPanel(
                           condition = "input.inputType == 'csv'",
                           fileInput("collarFile", "a. Upload Collar File (.csv)", accept = ".csv"),
                           fileInput("assayFile", "b. Upload Assay File (.csv)", accept = ".csv"),
                           fileInput("lithoFile", "c. Upload Lithology File (.csv)", accept = ".csv"),
                           radioButtons("sep", "CSV Separator:", choices = c(Comma = ",", Semicolon = ";"), selected = ";")
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
                           tags$li(strong("Session-Based:"), "Your data is processed only in the server's memory during your active session. It is permanently deleted when you close the browser window."),
                           tags$li(strong("No Database:"), "This application is not connected to any database to store your information.")
                         ),
                         hr(),
                         p(strong("Author:"), "Ghozian Islam Karami"),
                         p(strong("Contact:"), "ghoziankarami@gmail.com | ", 
                           tags$a(href="https://www.linkedin.com/in/ghozian-islam-karami/", target="_blank", "LinkedIn"))
                       ),
                       hr(),
                       h3("Combined Data Preview"),
                       DTOutput("combinedDataTable"),
                       hr(),
                       h3("Survey Data Preview"),
                       DTOutput("surveyDataTable")
                     )
                   )
          ),
          tabPanel("Column Definitions",
                   fluidPage(
                     h3("Define Column Names"),
                     p("Select the appropriate column names from your uploaded data. Defaults are set for the sample data."),
                     hr(),
                     fluidRow(
                       column(4, h4("Collar Data"), uiOutput("colCollarHoleIDUI"), uiOutput("colXUI"), uiOutput("colYUI"), uiOutput("colZUI")),
                       column(4, h4("Assay Data"), uiOutput("colAssayHoleIDUI"), uiOutput("colAssayFromUI"), uiOutput("colAssayToUI")),
                       column(4, h4("Lithology Data"), uiOutput("colLithoHoleIDUI"), uiOutput("colLithoFromUI"), uiOutput("colLithoToUI"), uiOutput("colLithoUI"))
                     )
                   )
          ),
          
          # Tab 2: Drillhole Location Map
          tabPanel("Drillhole Location Map",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("Map Color Settings"),
                       uiOutput("mapColorSelectUI"),
                       selectInput("scale_type", "Color Scale Type:",
                                   choices = c("Continuous" = "continuous", "Discrete" = "discrete")),
                       conditionalPanel(
                         condition = "input.scale_type == 'discrete'",
                         hr(),
                         h5("Set Intervals & Colors (Discrete)"),
                         textInput("manual_breaks", "Enter Upper Interval Breaks (comma-separated)", placeholder = "e.g., 1, 1.5, 2"),
                         selectInput("discrete_palette", "Select Color Palette:",
                                     choices = rownames(RColorBrewer::brewer.pal.info),
                                     selected = "YlOrRd")
                       )
                     ),
                     mainPanel(
                       width = 9,
                       plotlyOutput("drillholeMap", height = "700px")
                     )
                   )
          ),
          
          # Tab 3: Statistical Analysis
          tabPanel("Statistical Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       uiOutput("gradeSelectInput"),
                       hr(),
                       uiOutput("boxplotGradeSelectUI")
                     ),
                     mainPanel(
                       width = 9,
                       h3("Summary Statistics"),
                       DTOutput("summaryStatsTable"),
                       hr(),
                       plotlyOutput("gradeHistogram"),
                       hr(),
                       plotlyOutput("lithologyBoxplot")
                     )
                   )
          ),
          
          # Tab 4: Downhole Plot
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
          
          # Tab 5: Color Settings
          tabPanel("Lithology Color Settings",
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
                       h3("Lithology Color Settings"),
                       helpText("Select a lithology to change its color, then pick a new color. The change is applied automatically."),
                       hr(),
                       uiOutput("selectLithoToColorUI"),
                       uiOutput("pickNewColorUI"),
                       hr(),
                       uiOutput("copyLithoColorUI")
                     ),
                     mainPanel(
                       width = 8,
                       h4("Current Color Preview"),
                       DTOutput("lithoColorPreviewTable")
                     )
                   )
          )
        )
    ),
    tags$footer(
      style="position: absolute; bottom: 0; width: 100%; height: 50px; color: white; background-color: #333; text-align: center; padding: 15px;",
      "Developed for educational and professional use. Open-source for everyone."
    )
  )
)

# --- 4. Server Logic ---
server <- function(input, output, session) {
  
  useShinyjs()
  
  # Reactive value to store the color mapping
  rv <- reactiveValues(
    litho_colors = setNames(character(0), character(0)) # initialize as a named character vector
  )
  
  # --- PART 1: DATA READING AND PREPARATION ---
  
  # Logic to switch between sample and uploaded data
  collar_data_raw <- reactive({
    if (input$dataSource == "sample") {
      return(sample_collar)
    } else {
      req(input$inputType)
      if (input$inputType == "excel") {
        req(input$excelFile, input$collar_sheet)
        return(readxl::read_excel(input$excelFile$datapath, sheet = input$collar_sheet))
      } else {
        req(input$collarFile)
        return(read.csv(input$collarFile$datapath, sep = input$sep, stringsAsFactors = FALSE, check.names = FALSE))
      }
    }
  })
  
  assay_data_raw <- reactive({
    if (input$dataSource == "sample") {
      return(sample_assay)
    } else {
      req(input$inputType)
      if (input$inputType == "excel") {
        req(input$excelFile, input$assay_sheet)
        return(readxl::read_excel(input$excelFile$datapath, sheet = input$assay_sheet))
      } else {
        req(input$assayFile)
        return(read.csv(input$assayFile$datapath, sep = input$sep, stringsAsFactors = FALSE, check.names = FALSE))
      }
    }
  })
  
  litho_data_raw <- reactive({
    if (input$dataSource == "sample") {
      return(sample_lithology)
    } else {
      req(input$inputType)
      if (input$inputType == "excel") {
        req(input$excelFile, input$litho_sheet)
        return(readxl::read_excel(input$excelFile$datapath, sheet = input$litho_sheet))
      } else {
        req(input$lithoFile)
        return(read.csv(input$lithoFile$datapath, sep = input$sep, stringsAsFactors = FALSE, check.names = FALSE))
      }
    }
  })
  
  survey_data_raw <- reactive({
    if (input$dataSource == "sample") {
      return(sample_survey)
    } else {
      # For now, we don't support uploading a custom survey file,
      # but the framework is here for future expansion.
      return(sample_survey) 
    }
  })
  
  # Cleaned data reactives
  collar_data <- reactive({ janitor::clean_names(collar_data_raw()) })
  assay_data <- reactive({ janitor::clean_names(assay_data_raw()) })
  litho_data <- reactive({ janitor::clean_names(litho_data_raw()) })
  survey_data <- reactive({ janitor::clean_names(survey_data_raw()) })
  
  # UI for Excel sheet selection
  excel_sheets <- reactive({
    req(input$inputType == "excel", input$excelFile)
    tryCatch(readxl::excel_sheets(input$excelFile$datapath),
             error = function(e) {
               showNotification("Invalid or corrupt Excel file.", type = "error"); NULL
             })
  })
  
  output$collarSheetUI <- renderUI({ selectInput("collar_sheet", "Select Collar Sheet:", choices = excel_sheets()) })
  output$assaySheetUI <- renderUI({ selectInput("assay_sheet", "Select Assay Sheet:", choices = excel_sheets()) })
  output$lithoSheetUI <- renderUI({ selectInput("litho_sheet", "Select Lithology Sheet:", choices = excel_sheets()) })
  
  
  # Dynamic UI for column definitions
  find_col <- function(possible_names, actual_names) { intersect(possible_names, actual_names)[1] }
  
  output$colCollarHoleIDUI <- renderUI({ req(collar_data()); selectInput("col_collar_holeid", "Hole ID Column:", choices = names(collar_data()), selected = find_col(c("hole_id", "holeid", "bhid", "hole"), names(collar_data()))) })
  output$colXUI <- renderUI({ req(collar_data()); selectInput("col_x", "X (Easting) Column:", choices = names(collar_data()), selected = find_col(c("x", "easting", "east"), names(collar_data()))) })
  output$colYUI <- renderUI({ req(collar_data()); selectInput("col_y", "Y (Northing) Column:", choices = names(collar_data()), selected = find_col(c("y", "northing", "north"), names(collar_data()))) })
  output$colZUI <- renderUI({ req(collar_data()); selectInput("col_z", "Z (RL) Column:", choices = names(collar_data()), selected = find_col(c("z", "rl", "elevation"), names(collar_data()))) })
  
  output$colAssayHoleIDUI <- renderUI({ req(assay_data()); selectInput("col_assay_holeid", "Hole ID Column:", choices = names(assay_data()), selected = find_col(c("hole_id", "holeid", "bhid", "hole"), names(assay_data()))) })
  output$colAssayFromUI <- renderUI({ req(assay_data()); selectInput("col_assay_from", "From Column:", choices = names(assay_data()), selected = find_col(c("from", "dari"), names(assay_data()))) })
  output$colAssayToUI <- renderUI({ req(assay_data()); selectInput("col_assay_to", "To Column:", choices = names(assay_data()), selected = find_col(c("to", "sampai"), names(assay_data()))) })
  
  output$colLithoHoleIDUI <- renderUI({ req(litho_data()); selectInput("col_litho_holeid", "Hole ID Column:", choices = names(litho_data()), selected = find_col(c("hole_id", "holeid", "bhid", "hole"), names(litho_data()))) })
  output$colLithoFromUI <- renderUI({ req(litho_data()); selectInput("col_litho_from", "From Column:", choices = names(litho_data()), selected = find_col(c("from", "dari"), names(litho_data()))) })
  output$colLithoToUI <- renderUI({ req(litho_data()); selectInput("col_litho_to", "To Column:", choices = names(litho_data()), selected = find_col(c("to", "sampai"), names(litho_data()))) })
  output$colLithoUI <- renderUI({ req(litho_data()); selectInput("col_litho", "Lithology Column:", choices = names(litho_data()), selected = find_col(c("lithology", "litho_code", "litho", "rock", "deskripsi"), names(litho_data()))) })
  
  # Standardized data reactives
  collar_std <- reactive({
    req(collar_data(), input$col_collar_holeid, input$col_x, input$col_y, input$col_z)
    collar_data() %>%
      select(hole_id = !!sym(input$col_collar_holeid), x_coord = !!sym(input$col_x), y_coord = !!sym(input$col_y), z_coord = !!sym(input$col_z)) %>%
      mutate(hole_id = as.character(hole_id)) %>% 
      distinct(hole_id, .keep_all = TRUE)
  })
  
  assay_std <- reactive({
    req(assay_data(), input$col_assay_holeid, input$col_assay_from, input$col_assay_to)
    assay_data() %>%
      rename(hole_id = !!sym(input$col_assay_holeid), from = !!sym(input$col_assay_from), to = !!sym(input$col_assay_to)) %>%
      mutate(across(where(is.double) | where(is.integer), as.numeric), hole_id = as.character(hole_id), from = as.numeric(from), to = as.numeric(to))
  })
  
  litho_std <- reactive({
    req(litho_data(), input$col_litho_holeid, input$col_litho_from, input$col_litho_to, input$col_litho)
    litho_data() %>%
      rename(hole_id = !!sym(input$col_litho_holeid), from = !!sym(input$col_litho_from), to = !!sym(input$col_litho_to), lithology = !!sym(input$col_litho)) %>%
      mutate(hole_id = as.character(hole_id), from = as.numeric(from), to = as.numeric(to), lithology = as.character(lithology)) %>%
      select(hole_id, from, to, lithology)
  })
  
  # Final combined data
  combinedData <- reactive({
    tryCatch({
      data_merged_1 <- left_join(assay_std(), collar_std(), by = "hole_id")
      left_join(data_merged_1, litho_std(), by = c("hole_id", "from", "to"))
    }, error = function(e) {
      showNotification(paste("Error during data join:", e$message), type = "error", duration = 15); NULL
    })
  })
  
  output$combinedDataTable <- renderDT({
    validate(need(!is.null(combinedData()), "Data loading failed. Please check your input files and column definitions."))
    datatable(head(combinedData(), 100), options = list(pageLength = 10, scrollX = TRUE, scrollCollapse = TRUE))
  })
  
  output$surveyDataTable <- renderDT({
    validate(need(!is.null(survey_data()), "Survey data could not be loaded."))
    datatable(head(survey_data(), 100), options = list(pageLength = 5, scrollX = TRUE, scrollCollapse = TRUE))
  })
  
  # --- PART 2: VISUALIZATION AND ANALYSIS ---
  
  numeric_assay_cols <- reactive({
    req(assay_std())
    assay_std() %>% 
      select(where(is.numeric), -any_of(c("from", "to"))) %>% 
      names()
  })
  
  # --- 2b. Drillhole Location Map ---
  output$mapColorSelectUI <- renderUI({
    selectInput("mapColorColumn", "Color Map by Average:", 
                choices = c("None" = "", numeric_assay_cols()))
  })
  
  output$drillholeMap <- renderPlotly({
    validate(need(combinedData(), "Please upload and define files to view the map."))
    
    plot_data <- collar_std()
    
    p <- plot_ly(data = plot_data, x = ~x_coord, y = ~y_coord, type = 'scatter', mode = 'markers',
                 text = ~paste("Hole ID:", hole_id, "<br>X:", round(x_coord, 2), "<br>Y:", round(y_coord, 2), "<br>RL:", round(z_coord, 2)),
                 hoverinfo = 'text')
    
    if (!is.null(input$mapColorColumn) && input$mapColorColumn != "") {
      req(input$mapColorColumn %in% names(assay_std()))
      
      avg_grades <- combinedData() %>%
        group_by(hole_id) %>%
        summarise(avg_grade = mean(.data[[input$mapColorColumn]], na.rm = TRUE), .groups = 'drop')
      
      plot_data <- left_join(plot_data, avg_grades, by = "hole_id")
      
      if (input$scale_type == "continuous") {
        p <- style(p,
                   marker = list(color = plot_data$avg_grade, colorscale = 'Viridis', showscale = TRUE,
                                 colorbar = list(title = paste("Avg.", input$mapColorColumn))),
                   text = paste("Hole ID:", plot_data$hole_id, "<br>Avg.", input$mapColorColumn, ":", round(plot_data$avg_grade, 3)),
                   hoverinfo = "text")
      } else {
        req(input$manual_breaks != "") 
        
        breaks_num <- as.numeric(unlist(strsplit(input$manual_breaks, ",")))
        breaks_num <- sort(na.omit(breaks_num))
        validate(need(length(breaks_num) > 0, "Please enter at least one valid number for the interval break."))
        
        num_intervals <- length(breaks_num)
        palette_info <- RColorBrewer::brewer.pal.info[input$discrete_palette, ]
        validate(need(num_intervals <= palette_info$maxcolors, 
                      paste("The", input$discrete_palette, "palette only supports up to", palette_info$maxcolors, "colors.")))
        
        colors <- RColorBrewer::brewer.pal(max(3, num_intervals), input$discrete_palette)[1:num_intervals]
        breaks_vec <- c(-Inf, breaks_num)
        
        plot_data$grade_category <- cut(plot_data$avg_grade, breaks = breaks_vec, labels = FALSE)
        plot_data <- plot_data %>% filter(!is.na(grade_category))
        validate(need(nrow(plot_data) > 0, "No data falls within the specified interval ranges."))
        
        labels <- sapply(2:length(breaks_vec), function(i) {
          if (i == 2) paste("<=", format(breaks_vec[i], nsmall=2))
          else paste(format(breaks_vec[i-1], nsmall=2), "-", format(breaks_vec[i], nsmall=2))
        })
        
        p <- plot_ly()
        for (i in 1:length(labels)) {
          data_subset <- filter(plot_data, grade_category == i)
          if(nrow(data_subset) > 0) {
            p <- p %>% add_markers(data = data_subset, x = ~x_coord, y = ~y_coord, type = 'scatter', mode = 'markers',
                                   marker = list(color = colors[i]), name = labels[i],
                                   text = ~paste("Hole ID:", hole_id, "<br>Avg.", input$mapColorColumn, ":", round(avg_grade, 3)),
                                   hoverinfo = "text")
          }
        }
      }
    }
    
    p %>% layout(title = "Interactive Drillhole Location Map",
                 xaxis = list(title = "X Coordinate"),
                 yaxis = list(title = "Y Coordinate", scaleanchor = "x", scaleratio = 1),
                 legend = list(title = list(text = paste("<b> Avg.", input$mapColorColumn, "</b>"))))
  })
  
  # --- 2c. Statistical Analysis ---
  output$gradeSelectInput <- renderUI({
    selectInput("selectedGrades", "Select Grades for Analysis:", 
                choices = numeric_assay_cols(), multiple = TRUE, selected = numeric_assay_cols()[1])
  })
  
  output$boxplotGradeSelectUI <- renderUI({
    validate(
      need(input$selectedGrades, "Waiting for grade selection...")
    )
    selectInput("selectedBoxplotGrade", "Select Grade for Boxplot:", 
                choices = input$selectedGrades, 
                selected = input$selectedGrades[1])
  })
  
  output$summaryStatsTable <- renderDT({
    req(combinedData(), input$selectedGrades)
    validate(need(length(input$selectedGrades) > 0, "Please select at least one grade."))
    validate(need(all(input$selectedGrades %in% names(combinedData())), "One or more selected grades are not valid. Please re-select."))
    
    summary_df <- combinedData() %>%
      select(all_of(input$selectedGrades)) %>%
      pivot_longer(everything(), names_to = "Grade", values_to = "Value") %>%
      group_by(Grade) %>%
      summarise(Mean = round(mean(Value, na.rm = TRUE), 3), Median = round(median(Value, na.rm = TRUE), 3), 
                Min = round(min(Value, na.rm = TRUE), 3), Max = round(max(Value, na.rm = TRUE), 3), 
                `Std Dev` = round(sd(Value, na.rm = TRUE), 3), Count = n())
    datatable(summary_df)
  })
  
  output$gradeHistogram <- renderPlotly({
    req(combinedData(), input$selectedGrades)
    validate(need(length(input$selectedGrades) > 0, "Please select at least one grade."))
    validate(need(all(input$selectedGrades %in% names(combinedData())), "One or more selected grades are not valid. Please re-select."))
    
    data_plot <- combinedData() %>%
      select(all_of(input$selectedGrades)) %>%
      pivot_longer(everything(), names_to = "Grade", values_to = "Value")
    
    plot_ly(data_plot, x = ~Value, color = ~Grade, type = "histogram", alpha = 0.7, colors = "viridis") %>%
      layout(title = "Grade Distribution Histogram", xaxis = list(title = "Value"), yaxis = list(title = "Frequency"), barmode = "overlay")
  })
  
  output$lithologyBoxplot <- renderPlotly({
    req(combinedData(), input$selectedBoxplotGrade)
    validate(need("lithology" %in% names(combinedData()), "'lithology' column not found."))
    validate(need(input$selectedBoxplotGrade %in% names(combinedData()), "The selected grade for the boxplot is not valid."))
    validate(need(length(rv$litho_colors) > 0, "Lithology colors not set. Please set them in the 'Lithology Color Settings' tab."))
    
    p <- ggplot(combinedData(), aes(x = lithology, y = .data[[input$selectedBoxplotGrade]], fill = lithology)) +
      geom_boxplot() +
      scale_fill_manual(values = rv$litho_colors) +
      labs(title = paste("Boxplot of", input$selectedBoxplotGrade, "by Lithology"), x = "Lithology", y = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # --- 2d. Downhole Plot ---
  output$holeSelectInput <- renderUI({
    req(collar_std())
    hole_ids <- unique(collar_std()$hole_id)
    selectInput("selectedHole", "Select Hole ID:", choices = sort(hole_ids), selected = hole_ids[1])
  })
  
  output$downholeColumnSelectUI <- renderUI({
    req(assay_std())
    exclude_cols <- c("hole_id", "from", "to")
    choices <- setdiff(names(assay_std()), exclude_cols)
    selectizeInput("selectedDownholeCols", NULL, 
                   choices = choices, selected = choices[1], multiple = TRUE)
  })
  
  output$downholePlot <- renderPlotly({
    req(combinedData(), input$selectedHole)
    validate(need(!is.null(input$selectedDownholeCols) && length(input$selectedDownholeCols) > 0, 
                  "Please select at least one data column to display."))
    validate(need(length(rv$litho_colors) > 0, "Lithology colors not set. Please configure them in the 'Lithology Color Settings' tab."))
    
    hole_data <- combinedData() %>%
      filter(hole_id == input$selectedHole) %>%
      arrange(from)
    
    validate(need(nrow(hole_data) > 0, "No data found for this Hole ID."))
    
    p_litho <- ggplot(hole_data) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = from, ymax = to, fill = lithology,
                    text = paste("From:", from, "<br>To:", to, "<br>Litho:", lithology))) +
      scale_y_reverse(name = "Depth (m)") +
      scale_fill_manual(values = rv$litho_colors) +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())
    
    plot_list <- list(ggplotly(p_litho, tooltip = "text"))
    
    for (col_name in input$selectedDownholeCols) {
      selected_col_sym <- sym(col_name)
      
      if (is.numeric(hole_data[[col_name]])) {
        p_data <- ggplot(hole_data, aes(y = from, text = paste("From:", from, "<br>To:", to, "<br>", col_name, ":", !!selected_col_sym))) +
          geom_rect(aes(xmin = 0, xmax = !!selected_col_sym, ymin = from, ymax = to), fill = "skyblue", color = "black", alpha = 0.7) +
          scale_y_reverse() + 
          theme_minimal() +
          theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
          labs(x = col_name)
      } else {
        p_data <- ggplot(hole_data) +
          geom_rect(aes(xmin = 0, xmax = 1, ymin = from, ymax = to), fill = "gray95", color = "gray80") +
          geom_text(aes(x = 0.5, y = (from + to) / 2, label = !!selected_col_sym, text = paste("From:", from, "<br>To:", to, "<br>", !!selected_col_sym)), check_overlap = TRUE, size = 3) +
          scale_y_reverse() + theme_minimal() +
          theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())
      }
      plot_list <- append(plot_list, list(ggplotly(p_data, tooltip = "text")))
    }
    
    num_plots <- length(plot_list)
    if (num_plots > 1) {
      widths <- c(0.25, rep(0.75 / (num_plots - 1), num_plots - 1))
    } else {
      widths <- c(1)
    }
    
    subplot(plot_list, nrows = 1, shareY = TRUE, titleX = TRUE, widths = widths) %>%
      layout(title = paste("Downhole Plot for", input$selectedHole))
  })
  
  # --- 2e. Custom Color Settings Logic (STABLE & USER-FRIENDLY) ---
  
  unique_lithos_reactive <- reactive({
    req(combinedData())
    unique_lithos <- unique(sort(combinedData()$lithology))
    unique_lithos[!is.na(unique_lithos)]
  })
  
  observeEvent(unique_lithos_reactive(), {
    current_lithos <- unique_lithos_reactive()
    req(length(current_lithos) > 0)
    
    saved_colors <- rv$litho_colors
    
    lithos_to_add <- current_lithos[!current_lithos %in% names(saved_colors)]
    
    lithos_to_remove <- names(saved_colors)[!names(saved_colors) %in% current_lithos]
    
    if (length(lithos_to_remove) > 0) {
      saved_colors <- saved_colors[!names(saved_colors) %in% lithos_to_remove]
    }
    
    if (length(lithos_to_add) > 0) {
      palette <- colorRampPalette(brewer.pal(8, "Set2"))(length(lithos_to_add))
      new_colors <- setNames(palette, lithos_to_add)
      saved_colors <- c(saved_colors, new_colors)
    }
    
    rv$litho_colors <- saved_colors
  })
  
  # UI for selecting lithology
  output$selectLithoToColorUI <- renderUI({
    req(rv$litho_colors)
    selectInput("selected_litho_color", "1. Select Lithology to Change:",
                choices = names(rv$litho_colors))
  })
  
  # UI for picking a new color
  output$pickNewColorUI <- renderUI({
    req(input$selected_litho_color)
    colourpicker::colourInput("new_litho_color", "2. Pick a New Color:",
                              value = rv$litho_colors[input$selected_litho_color])
  })
  
  # UI for copying color from another lithology
  output$copyLithoColorUI <- renderUI({
    req(rv$litho_colors)
    selectInput("copy_from_litho", "3. Or, Copy Color from Another Lithology:",
                choices = c("None" = "", names(rv$litho_colors)))
  })
  
  # Observer to update colors
  observe({
    req(input$selected_litho_color, input$new_litho_color, rv$litho_colors)
    if(!is.null(input$new_litho_color)){
      temp_colors <- rv$litho_colors
      temp_colors[input$selected_litho_color] <- input$new_litho_color
      rv$litho_colors <- temp_colors
    }
  })
  
  # Observer to copy colors
  observeEvent(input$copy_from_litho, {
    req(input$copy_from_litho != "", input$selected_litho_color, rv$litho_colors)
    
    temp_colors <- rv$litho_colors
    temp_colors[input$selected_litho_color] <- rv$litho_colors[input$copy_from_litho]
    rv$litho_colors <- temp_colors
    
    updateColourInput(session, "new_litho_color", value = rv$litho_colors[input$selected_litho_color])
    updateSelectInput(session, "copy_from_litho", selected = "")
  })
  
  # Display the color preview table
  output$lithoColorPreviewTable <- renderDT({
    req(rv$litho_colors)
    
    df <- data.frame(
      Lithology = names(rv$litho_colors),
      Color_Code = unname(rv$litho_colors),
      Preview = paste0("<div style='background-color:", unname(rv$litho_colors), "; width:100%; height:20px; border: 1px solid #ddd;'></div>")
    )
    
    datatable(df, 
              escape = FALSE, 
              options = list(dom = 't', ordering = FALSE), 
              rownames = FALSE)
  })
  
}

# --- 5. Run Application ---
shinyApp(ui, server)

