ui <- fluidPage(
  useShinyjs(),
  tags$div(
    style = "position: relative; min-height: 100vh;",
    div(style="padding-bottom: 50px;",
        navbarPage(
          "GeoDataViz v1.2.3", 
          
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
                         h3("2. Upload Files"),
                         selectInput("inputType", "Select Input Format:",
                                     choices = c("Excel (Single File)" = "excel",
                                                 "CSV (Multiple Files)" = "csv")),
                         hr(),
                         conditionalPanel(
                           condition = "input$inputType == 'excel'",
                           fileInput("excelFile", "Upload Excel File (.xlsx)", accept = c(".xlsx")),
                           hr(),
                           h3("Select Sheets"),
                           uiOutput("collarSheetUI"),
                           uiOutput("assaySheetUI"),
                           uiOutput("lithoSheetUI")
                         ),
                         conditionalPanel(
                           condition = "input$inputType == 'csv'",
                           fileInput("collarFile", "a. Upload Collar File (.csv)", accept = ".csv"),
                           fileInput("assayFile", "b. Upload Assay File (.csv)", accept = ".csv"),
                           fileInput("lithoFile", "c. Upload Lithology File (.csv)", accept = ".csv"),
                           radioButtons("sep", "CSV Separator:", choices = c(Comma = ",", Semicolon = ";"), selected = ";")
                         )
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