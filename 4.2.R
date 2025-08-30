# app.R
# Versi 4.3: "Clean Join Edition"
# - Memperbaiki bug duplikasi kolom (.x, .y) secara definitif.
# - Logika `combinedData` sekarang secara eksplisit menyeleksi dan menstandardisasi
#   nama kolom dari data collar SEBELUM join, mencegah semua konflik nama kolom.
# - Meningkatkan kebersihan, stabilitas, dan prediktabilitas data akhir.

# --- 1. Memuat Library ---
library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(ggplot2)
library(rlang)
library(readxl) # Untuk membaca file Excel
library(janitor)  # Untuk membersihkan nama kolom

# --- 2. Definisi UI (User Interface) ---
ui <- navbarPage(
  "Analisis Data Bor Geologi v4.3",
  
  # Tab 1: Unggah Data & Definisi
  tabPanel("Unggah & Integrasi Data",
           sidebarLayout(
             sidebarPanel(
               h3("1. Mode Input Data"),
               selectInput("inputType", "Pilih Sumber Data:",
                           choices = c("Excel (Satu File)" = "excel",
                                       "CSV (Beberapa File)" = "csv")),
               
               hr(),
               
               conditionalPanel(
                 condition = "input.inputType == 'excel'",
                 h3("2. Unggah File Excel"),
                 fileInput("excelFile", "Unggah File Excel (.xlsx)", accept = c(".xlsx")),
                 hr(),
                 h3("3. Pilih Sheet Data"),
                 uiOutput("collarSheetUI"),
                 uiOutput("assaySheetUI"),
                 uiOutput("lithoSheetUI")
               ),
               
               conditionalPanel(
                 condition = "input.inputType == 'csv'",
                 h3("2. Unggah File CSV"),
                 fileInput("collarFile", "a. Unggah File Collar (.csv)", accept = ".csv"),
                 fileInput("assayFile", "b. Unggah File Assay (.csv)", accept = ".csv"),
                 fileInput("lithoFile", "c. Unggah File Litho (.csv)", accept = ".csv"),
                 radioButtons("sep", "Pemisah CSV:", choices = c(Koma = ",", Titik_Koma = ";"), selected = ";")
               ),
               
               hr(),
               h3("4. Definisi Nama Kolom"),
               helpText("Pilih nama kolom yang sesuai dari data yang diunggah."),
               
               h4("Data Collar"),
               uiOutput("colCollarHoleIDUI"),
               uiOutput("colXUI"),
               uiOutput("colYUI"),
               uiOutput("colZUI"),
               
               h4("Data Assay"),
               uiOutput("colAssayHoleIDUI"),
               uiOutput("colAssayFromUI"),
               uiOutput("colAssayToUI"),
               
               h4("Data Litho"),
               uiOutput("colLithoHoleIDUI"),
               uiOutput("colLithoFromUI"),
               uiOutput("colLithoToUI"),
               uiOutput("colLithoUI")
             ),
             mainPanel(
               h3("Pratinjau Data Gabungan"),
               DTOutput("combinedDataTable")
             )
           )
  ),
  
  # Tab lainnya tidak berubah
  tabPanel("Peta Lokasi Bor", sidebarLayout(sidebarPanel(uiOutput("mapColorSelectUI")), mainPanel(plotlyOutput("drillholeMap", height = "600px")))),
  tabPanel("Analisis Statistik", sidebarLayout(sidebarPanel(uiOutput("gradeSelectInput"), hr(), uiOutput("boxplotGradeSelectUI")), mainPanel(DTOutput("summaryStatsTable"), hr(), plotlyOutput("gradeHistogram"), hr(), plotlyOutput("lithologyBoxplot")))),
  tabPanel("Plot Downhole", sidebarLayout(sidebarPanel(uiOutput("holeSelectInput")), mainPanel(plotlyOutput("downholePlot", height = "800px"))))
)

# --- 3. Definisi Server (Logika Aplikasi) ---
server <- function(input, output, session) {
  
  # --- Logika Pembacaan Data yang Adaptif ---
  excel_sheets <- reactive({
    req(input$inputType == "excel", input$excelFile)
    readxl::excel_sheets(input$excelFile$datapath)
  })
  
  output$collarSheetUI <- renderUI({ selectInput("collar_sheet", "Pilih Sheet Collar:", choices = excel_sheets()) })
  output$assaySheetUI <- renderUI({ selectInput("assay_sheet", "Pilih Sheet Assay:", choices = excel_sheets()) })
  output$lithoSheetUI <- renderUI({ selectInput("litho_sheet", "Pilih Sheet Litho:", choices = excel_sheets()) })
  
  collar_data <- reactive({
    req(input$inputType)
    df <- if (input$inputType == "excel") {
      req(input$excelFile, input$collar_sheet)
      readxl::read_excel(input$excelFile$datapath, sheet = input$collar_sheet)
    } else {
      req(input$collarFile)
      read.csv(input$collarFile$datapath, sep = input$sep, stringsAsFactors = FALSE)
    }
    df %>% janitor::clean_names()
  })
  
  assay_data <- reactive({
    req(input$inputType)
    df <- if (input$inputType == "excel") {
      req(input$excelFile, input$assay_sheet)
      readxl::read_excel(input$excelFile$datapath, sheet = input$assay_sheet)
    } else {
      req(input$assayFile)
      read.csv(input$assayFile$datapath, sep = input$sep, stringsAsFactors = FALSE)
    }
    df %>% janitor::clean_names()
  })
  
  litho_data <- reactive({
    req(input$inputType)
    df <- if (input$inputType == "excel") {
      req(input$excelFile, input$litho_sheet)
      readxl::read_excel(input$excelFile$datapath, sheet = input$litho_sheet)
    } else {
      req(input$lithoFile)
      read.csv(input$lithoFile$datapath, sep = input$sep, stringsAsFactors = FALSE)
    }
    df %>% janitor::clean_names()
  })
  
  # --- UI Dinamis untuk Definisi Kolom ---
  find_col <- function(possible_names, actual_names) { intersect(possible_names, actual_names)[1] }
  
  output$colCollarHoleIDUI <- renderUI({ req(collar_data()); selectInput("col_collar_holeid", "Kolom Hole ID:", choices = names(collar_data()), selected = find_col(c("hole", "holeid", "bhid"), names(collar_data()))) })
  output$colXUI <- renderUI({ req(collar_data()); selectInput("col_x", "Kolom X (Easting):", choices = names(collar_data()), selected = find_col(c("easting", "x", "east"), names(collar_data()))) })
  output$colYUI <- renderUI({ req(collar_data()); selectInput("col_y", "Kolom Y (Northing):", choices = names(collar_data()), selected = find_col(c("northing", "y", "north"), names(collar_data()))) })
  output$colZUI <- renderUI({ req(collar_data()); selectInput("col_z", "Kolom Z (RL):", choices = names(collar_data()), selected = find_col(c("z", "rl", "elevation"), names(collar_data()))) })
  
  output$colAssayHoleIDUI <- renderUI({ req(assay_data()); selectInput("col_assay_holeid", "Kolom Hole ID:", choices = names(assay_data()), selected = find_col(c("hole", "holeid", "bhid"), names(assay_data()))) })
  output$colAssayFromUI <- renderUI({ req(assay_data()); selectInput("col_assay_from", "Kolom From:", choices = names(assay_data()), selected = find_col(c("from", "dari"), names(assay_data()))) })
  output$colAssayToUI <- renderUI({ req(assay_data()); selectInput("col_assay_to", "Kolom To:", choices = names(assay_data()), selected = find_col(c("to", "sampai"), names(assay_data()))) })
  
  output$colLithoHoleIDUI <- renderUI({ req(litho_data()); selectInput("col_litho_holeid", "Kolom Hole ID:", choices = names(litho_data()), selected = find_col(c("hole", "holeid", "bhid"), names(litho_data()))) })
  output$colLithoFromUI <- renderUI({ req(litho_data()); selectInput("col_litho_from", "Kolom From:", choices = names(litho_data()), selected = find_col(c("from", "dari"), names(litho_data()))) })
  output$colLithoToUI <- renderUI({ req(litho_data()); selectInput("col_litho_to", "Kolom To:", choices = names(litho_data()), selected = find_col(c("to", "sampai"), names(litho_data()))) })
  output$colLithoUI <- renderUI({ req(litho_data()); selectInput("col_litho", "Kolom Litologi:", choices = names(litho_data()), selected = find_col(c("litho", "rock", "deskripsi", "lithology"), names(litho_data()))) })
  
  # --- Proses Penggabungan Data Inti ---
  combinedData <- reactive({
    req(collar_data(), assay_data(), litho_data(),
        input$col_collar_holeid, input$col_x, input$col_y, input$col_z,
        input$col_assay_holeid, input$col_assay_from, input$col_assay_to,
        input$col_litho_holeid, input$col_litho_from, input$col_litho_to, input$col_litho)
    
    tryCatch({
      # <<< PERBAIKAN KUNCI: Siapkan data collar dengan memilih & menstandardisasi nama kolom >>>
      collar_clean <- collar_data() %>%
        select(
          hole_id = !!sym(input$col_collar_holeid),
          x_coord = !!sym(input$col_x),
          y_coord = !!sym(input$col_y),
          z_coord = !!sym(input$col_z)
        ) %>%
        mutate(hole_id = as.character(hole_id)) %>%
        distinct(hole_id, .keep_all = TRUE)
      
      # Siapkan data assay dan litho
      assay <- assay_data() %>% rename(hole_id = !!sym(input$col_assay_holeid), from = !!sym(input$col_assay_from), to = !!sym(input$col_assay_to))
      litho <- litho_data() %>% rename(hole_id = !!sym(input$col_litho_holeid), from = !!sym(input$col_litho_from), to = !!sym(input$col_litho_to), litho = !!sym(input$col_litho))
      
      # Konversi tipe data dan pembulatan
      assay <- assay %>%
        mutate(across(where(is.double) | where(is.integer), as.numeric)) %>%
        mutate(hole_id = as.character(hole_id),
               from = round(as.numeric(from), 3),
               to = round(as.numeric(to), 3))
      
      litho <- litho %>%
        mutate(hole_id = as.character(hole_id),
               from = round(as.numeric(from), 3),
               to = round(as.numeric(to), 3))
      
      # Lakukan join dengan data collar yang sudah bersih
      data_merged_1 <- left_join(assay, collar_clean, by = "hole_id")
      data_final <- left_join(data_merged_1, litho, by = c("hole_id", "from", "to"))
      
      return(data_final)
      
    }, error = function(e) {
      showNotification(paste("Error saat menggabungkan data: ", e$message), type = "error", duration = 15)
      return(NULL)
    })
  })
  
  # --- Sisa Logika Server (Plot, Tabel, dll.) ---
  
  output$combinedDataTable <- renderDT({
    validate(need(!is.null(combinedData()), "Gagal memuat data. Periksa file input dan definisi sheet/kolom."))
    datatable(head(combinedData(), 100), options = list(pageLength = 10, scrollX = TRUE, scrollCollapse = TRUE))
  })
  
  output$mapColorSelectUI <- renderUI({
    req(assay_data())
    numeric_cols <- names(assay_data())[sapply(assay_data(), is.numeric)]
    req(input$col_assay_from, input$col_assay_to)
    exclude_cols <- c(input$col_assay_from, input$col_assay_to)
    color_choices <- setdiff(numeric_cols, exclude_cols)
    selectInput("mapColorColumn", "Warnai Peta Berdasarkan Rata-rata:", choices = c("Tidak Ada" = "", color_choices))
  })
  
  output$drillholeMap <- renderPlotly({
    validate(need(combinedData(), "Unggah dan definisikan file untuk melihat peta."))
    
    plot_data <- combinedData() %>% distinct(hole_id, .keep_all = TRUE)
    
    # <<< PERBAIKAN: Gunakan nama kolom standar (x_coord, y_coord) untuk plot >>>
    p <- plot_ly(data = plot_data, 
                 x = ~x_coord, 
                 y = ~y_coord, 
                 type = 'scatter', mode = 'markers', 
                 text = ~hole_id, hoverinfo = 'text')
    
    if (!is.null(input$mapColorColumn) && input$mapColorColumn != "" && input$mapColorColumn %in% names(combinedData())) {
      avg_grades <- combinedData() %>%
        group_by(hole_id) %>%
        summarise(avg_grade = mean(.data[[input$mapColorColumn]], na.rm = TRUE))
      
      plot_data <- left_join(plot_data, avg_grades, by = "hole_id")
      
      p <- plot_ly(data = plot_data, 
                   x = ~x_coord, 
                   y = ~y_coord, 
                   type = 'scatter', mode = 'markers', color = ~avg_grade, colors = "viridis",
                   marker = list(showscale = TRUE, colorbar = list(title = gsub("_", " ", input$mapColorColumn, fixed=TRUE))),
                   text = ~paste("Hole_ID:", hole_id, "<br>Avg", input$mapColorColumn, ":", round(avg_grade, 2)),
                   hoverinfo = "text")
    }
    
    p %>% layout(title = "Peta Interaktif Lokasi Lubang Bor", 
                 xaxis = list(title = "Koordinat X"), 
                 yaxis = list(title = "Koordinat Y", scaleanchor = "x", scaleratio = 1))
  })
  
  output$gradeSelectInput <- renderUI({
    req(assay_data())
    numeric_cols <- names(assay_data())[sapply(assay_data(), is.numeric)]
    req(input$col_assay_holeid, input$col_assay_from, input$col_assay_to)
    exclude_cols <- c(input$col_assay_holeid, input$col_assay_from, input$col_assay_to)
    grade_cols <- setdiff(numeric_cols, exclude_cols)
    selectInput("selectedGrades", "Pilih Grade untuk Analisis:", choices = grade_cols, multiple = TRUE, selected = grade_cols[1])
  })
  
  output$boxplotGradeSelectUI <- renderUI({
    req(input$selectedGrades)
    selectInput("selectedBoxplotGrade", "Pilih Grade untuk Boxplot:", choices = input$selectedGrades, selected = input$selectedGrades[1])
  })
  
  output$summaryStatsTable <- renderDT({
    req(combinedData(), input$selectedGrades)
    validate(need(length(input$selectedGrades) > 0, "Pilih setidaknya satu grade."))
    
    summary_df <- combinedData() %>%
      select(all_of(input$selectedGrades)) %>%
      pivot_longer(everything(), names_to = "Grade", values_to = "Value") %>%
      group_by(Grade) %>%
      summarise(Mean = round(mean(Value, na.rm = TRUE), 3), Median = round(median(Value, na.rm = TRUE), 3), Min = round(min(Value, na.rm = TRUE), 3), Max = round(max(Value, na.rm = TRUE), 3), `Std Dev` = round(sd(Value, na.rm = TRUE), 3), Count = n())
    datatable(summary_df)
  })
  
  output$gradeHistogram <- renderPlotly({
    req(combinedData(), input$selectedGrades)
    validate(need(length(input$selectedGrades) > 0, "Pilih setidaknya satu grade."))
    
    data_plot <- combinedData() %>%
      select(all_of(input$selectedGrades)) %>%
      pivot_longer(everything(), names_to = "Grade", values_to = "Value")
    
    plot_ly(data_plot, x = ~Value, color = ~Grade, type = "histogram", alpha = 0.7) %>%
      layout(title = "Histogram Distribusi Grade", xaxis = list(title = "Nilai"), yaxis = list(title = "Frekuensi"), barmode = "overlay")
  })
  
  output$lithologyBoxplot <- renderPlotly({
    req(combinedData(), input$selectedBoxplotGrade)
    validate(need("litho" %in% colnames(combinedData()), "Kolom 'litho' tidak ditemukan."))
    
    p <- ggplot(combinedData(), aes(x = litho, y = .data[[input$selectedBoxplotGrade]], fill = litho)) +
      geom_boxplot() +
      labs(title = paste("Boxplot", input$selectedBoxplotGrade, "Berdasarkan Litologi"), x = "Litologi", y = "Nilai") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$holeSelectInput <- renderUI({
    req(combinedData())
    hole_ids <- unique(combinedData()$hole_id)
    selectInput("selectedHole", "Pilih Hole ID:", choices = sort(hole_ids), selected = hole_ids[1])
  })
  
  output$downholePlot <- renderPlotly({
    req(combinedData(), input$selectedHole, input$selectedGrades)
    
    hole_data <- combinedData() %>% filter(hole_id == input$selectedHole)
    validate(need(nrow(hole_data) > 0, "Data tidak ditemukan untuk Hole ID ini."))
    
    p_litho <- ggplot(hole_data) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = from, ymax = to, fill = litho, text = paste("From:", from, "<br>To:", to, "<br>Litho:", litho))) +
      scale_y_reverse(name = "Kedalaman (m)") +
      theme_minimal() + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank()) + labs(fill = "Litologi")
    
    grade_data_long <- hole_data %>%
      select(from, to, all_of(input$selectedGrades)) %>%
      pivot_longer(cols = -c(from, to), names_to = "Grade", values_to = "Value")
    
    p_grades <- ggplot(grade_data_long, aes(y = (from + to) / 2, x = Value, color = Grade, text = paste("Depth:", (from+to)/2, "<br>Value:", Value))) +
      geom_line() + geom_point() + scale_y_reverse() + theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + labs(x = "Nilai Grade", color = "Grade")
    
    subplot(ggplotly(p_litho, tooltip = "text"), ggplotly(p_grades, tooltip = "text"), nrows = 1, shareY = TRUE, widths = c(0.2, 0.8), titleX = TRUE) %>%
      layout(title = paste("Downhole Plot untuk", input$selectedHole))
  })
}

# --- 4. Jalankan Aplikasi ---
shinyApp(ui, server)