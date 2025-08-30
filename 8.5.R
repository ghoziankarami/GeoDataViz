# app.R
# Versi 8.5: "User-Friendly UI Edition"
# Oleh: Gemini (Geologist & Informatics Specialist)
#
# Changelog v8.5:
# - PEROMBAKAN TOTAL UI PENGATURAN WARNA:
#   - Menghilangkan tabel DT yang dapat diedit yang tidak stabil.
#   - Menggantinya dengan UI yang lebih intuitif menggunakan dropdown (selectInput)
#     dan pemilih warna (colourInput).
#   - Alur kerja baru: Pilih litologi -> Pilih warna baru -> Warna otomatis diperbarui.
# - STABILITAS MAKSIMAL:
#   - UI baru ini menghilangkan bug inkonsistensi input dan jauh lebih ramah
#     terhadap memori, memastikan aplikasi berjalan lancar di RStudio dan shinyapps.io.
# - PRATINJAU LANGSUNG:
#   - Menambahkan tabel pratinjau statis yang langsung menunjukkan perubahan warna.

# --- 1. Memuat Library ---
# Pastikan semua library ini sudah terinstall:
# install.packages(c("shiny", "dplyr", "tidyr", "plotly", "DT", "ggplot2", "rlang", "readxl", "janitor", "shinyjs", "RColorBrewer", "colourpicker"))

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

# --- 2. Definisi UI (User Interface) ---
ui <- fluidPage(
  tags$div(
    style = "position: relative; min-height: 100vh;",
    div(style="padding-bottom: 50px;",
        navbarPage(
          "Analisis Data Bor Geologi v8.5 (Stable)",
          
          # Tab 1: Unggah Data & Definisi
          tabPanel("Unggah & Integrasi Data",
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
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
                       )
                     ),
                     mainPanel(
                       width = 8,
                       wellPanel(
                         style = "background-color: #f0f8ff;",
                         h3("Mengapa Menggunakan Aplikasi Ini?"),
                         tags$ul(
                           tags$li(icon("check-circle"), "Mengintegrasikan data Collar, Assay, dan Litologi dari Excel atau CSV."),
                           tags$li(icon("check-circle"), "Visualisasi interaktif: peta lokasi, plot statistik, dan log downhole."),
                           tags$li(icon("check-circle"), "Pemetaan kolom dinamis untuk beradaptasi dengan struktur data Anda."),
                           tags$li(icon("check-circle"), "Tidak perlu instalasi, bekerja penuh di browser Anda."),
                           tags$li(icon("check-circle"), "Ideal untuk ahli geologi, mahasiswa, dan analis data di bidang pertambangan.")
                         ),
                         hr(),
                         p(strong("Privasi Data:"), 
                           em("Semua data yang diunggah hanya tersimpan sementara di memori dan akan otomatis hilang setelah sesi ditutup.")),
                         hr(),
                         p(strong("Author:"), "Ghozian Islam Karami"),
                         p(strong("Kontak:"), "ghoziankarami@gmail.com | ", 
                           tags$a(href="https://www.linkedin.com/in/ghozian-islam-karami/", target="_blank", "LinkedIn"))
                       ),
                       hr(),
                       h3("Pratinjau Data Gabungan"),
                       DTOutput("combinedDataTable")
                     )
                   )
          ),
          tabPanel("Definisi Nama Kolom",
                   fluidPage(
                     h3("4. Definisi Nama Kolom"),
                     p("Pilih nama kolom yang sesuai dari data yang diunggah."),
                     hr(),
                     fluidRow(
                       column(4, h4("Data Collar"), uiOutput("colCollarHoleIDUI"), uiOutput("colXUI"), uiOutput("colYUI"), uiOutput("colZUI")),
                       column(4, h4("Data Assay"), uiOutput("colAssayHoleIDUI"), uiOutput("colAssayFromUI"), uiOutput("colAssayToUI")),
                       column(4, h4("Data Litologi"), uiOutput("colLithoHoleIDUI"), uiOutput("colLithoFromUI"), uiOutput("colLithoToUI"), uiOutput("colLithoUI"))
                     )
                   )
          ),
          
          # Tab 2: Peta Lokasi Bor
          tabPanel("Peta Lokasi Bor",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("Pengaturan Warna Peta"),
                       uiOutput("mapColorSelectUI"),
                       selectInput("scale_type", "Tipe Skala Warna:",
                                   choices = c("Kontinu" = "continuous", "Diskret" = "discrete")),
                       conditionalPanel(
                         condition = "input.scale_type == 'discrete'",
                         hr(),
                         h5("Atur Interval & Warna (Diskret)"),
                         textInput("manual_breaks", "Masukkan Batas Atas Interval (pisahkan koma)", placeholder = "Contoh: 1, 1.5, 2"),
                         selectInput("discrete_palette", "Pilih Palet Warna:",
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
          
          # Tab 3: Analisis Statistik
          tabPanel("Analisis Statistik",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       uiOutput("gradeSelectInput"),
                       hr(),
                       uiOutput("boxplotGradeSelectUI")
                     ),
                     mainPanel(
                       width = 9,
                       h3("Ringkasan Statistik"),
                       DTOutput("summaryStatsTable"),
                       hr(),
                       plotlyOutput("gradeHistogram"),
                       hr(),
                       plotlyOutput("lithologyBoxplot")
                     )
                   )
          ),
          
          # Tab 4: Plot Downhole
          tabPanel("Plot Downhole",
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
                       uiOutput("holeSelectInput"),
                       hr(),
                       h4("Pilih Kolom untuk Ditampilkan (bisa > 1):"),
                       uiOutput("downholeColumnSelectUI")
                     ),
                     mainPanel(
                       width = 8,
                       plotlyOutput("downholePlot", height = "800px")
                     )
                   )
          ),
          
          # Tab 5: Pengaturan Warna
          tabPanel("Pengaturan Warna Litologi",
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
                       h3("Pengaturan Warna Litologi"),
                       helpText("Pilih litologi yang warnanya ingin diubah, lalu pilih warna baru. Perubahan akan diterapkan secara otomatis."),
                       hr(),
                       uiOutput("selectLithoToColorUI"),
                       uiOutput("pickNewColorUI"),
                       hr(),
                       uiOutput("copyLithoColorUI")
                     ),
                     mainPanel(
                       width = 8,
                       h4("Pratinjau Warna Saat Ini"),
                       DTOutput("lithoColorPreviewTable")
                     )
                   )
          )
        )
    ),
    tags$footer(
      style="position: absolute; bottom: 0; width: 100%; height: 50px; color: white; background-color: #333; text-align: center; padding: 15px;",
      "Dikembangkan untuk penggunaan edukasi dan profesional. Open-source untuk semua orang, Indonesia tanpa Software Bajakan."
    )
  )
)

# --- 3. Definisi Server (Logika Aplikasi) ---
server <- function(input, output, session) {
  
  useShinyjs()
  
  # Reactive value untuk menyimpan pemetaan warna
  rv <- reactiveValues(
    litho_colors = setNames(character(0), character(0)) # inisialisasi sebagai named character vector
  )
  
  # --- BAGIAN 1: PEMBACAAN DAN PERSIAPAN DATA ---
  
  excel_sheets <- reactive({
    req(input$inputType == "excel", input$excelFile)
    tryCatch(readxl::excel_sheets(input$excelFile$datapath),
             error = function(e) {
               showNotification("File Excel tidak valid atau rusak.", type = "error"); NULL
             })
  })
  
  output$collarSheetUI <- renderUI({ selectInput("collar_sheet", "Pilih Sheet Collar:", choices = excel_sheets()) })
  output$assaySheetUI <- renderUI({ selectInput("assay_sheet", "Pilih Sheet Assay:", choices = excel_sheets()) })
  output$lithoSheetUI <- renderUI({ selectInput("litho_sheet", "Pilih Sheet Litologi:", choices = excel_sheets()) })
  
  read_data <- function(type, file_input, sheet_input = NULL, separator = ";") {
    req(input$inputType)
    df <- if (input$inputType == "excel") {
      req(input$excelFile, sheet_input)
      readxl::read_excel(input$excelFile$datapath, sheet = sheet_input)
    } else {
      req(file_input)
      read.csv(file_input$datapath, sep = separator, stringsAsFactors = FALSE, check.names = FALSE)
    }
    df %>% janitor::clean_names()
  }
  
  collar_data <- reactive({ read_data(input$inputType, input$collarFile, input$collar_sheet, input$sep) })
  assay_data <- reactive({ read_data(input$inputType, input$assayFile, input$assay_sheet, input$sep) })
  litho_data <- reactive({ read_data(input$inputType, input$lithoFile, input$litho_sheet, input$sep) })
  
  find_col <- function(possible_names, actual_names) { intersect(possible_names, actual_names)[1] }
  
  output$colCollarHoleIDUI <- renderUI({ req(collar_data()); selectInput("col_collar_holeid", "Kolom Hole ID:", choices = names(collar_data()), selected = find_col(c("hole_id", "holeid", "bhid", "hole"), names(collar_data()))) })
  output$colXUI <- renderUI({ req(collar_data()); selectInput("col_x", "Kolom X (Easting):", choices = names(collar_data()), selected = find_col(c("x", "easting", "east"), names(collar_data()))) })
  output$colYUI <- renderUI({ req(collar_data()); selectInput("col_y", "Kolom Y (Northing):", choices = names(collar_data()), selected = find_col(c("y", "northing", "north"), names(collar_data()))) })
  output$colZUI <- renderUI({ req(collar_data()); selectInput("col_z", "Kolom Z (RL):", choices = names(collar_data()), selected = find_col(c("z", "rl", "elevation"), names(collar_data()))) })
  
  output$colAssayHoleIDUI <- renderUI({ req(assay_data()); selectInput("col_assay_holeid", "Kolom Hole ID:", choices = names(assay_data()), selected = find_col(c("hole_id", "holeid", "bhid", "hole"), names(assay_data()))) })
  output$colAssayFromUI <- renderUI({ req(assay_data()); selectInput("col_assay_from", "Kolom From:", choices = names(assay_data()), selected = find_col(c("from", "dari"), names(assay_data()))) })
  output$colAssayToUI <- renderUI({ req(assay_data()); selectInput("col_assay_to", "Kolom To:", choices = names(assay_data()), selected = find_col(c("to", "sampai"), names(assay_data()))) })
  
  output$colLithoHoleIDUI <- renderUI({ req(litho_data()); selectInput("col_litho_holeid", "Kolom Hole ID:", choices = names(litho_data()), selected = find_col(c("hole_id", "holeid", "bhid", "hole"), names(litho_data()))) })
  output$colLithoFromUI <- renderUI({ req(litho_data()); selectInput("col_litho_from", "Kolom From:", choices = names(litho_data()), selected = find_col(c("from", "dari"), names(litho_data()))) })
  output$colLithoToUI <- renderUI({ req(litho_data()); selectInput("col_litho_to", "Kolom To:", choices = names(litho_data()), selected = find_col(c("to", "sampai"), names(litho_data()))) })
  output$colLithoUI <- renderUI({ req(litho_data()); selectInput("col_litho", "Kolom Litologi:", choices = names(litho_data()), selected = find_col(c("litho", "lithology", "rock", "deskripsi"), names(litho_data()))) })
  
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
  
  combinedData <- reactive({
    tryCatch({
      data_merged_1 <- left_join(assay_std(), collar_std(), by = "hole_id")
      left_join(data_merged_1, litho_std(), by = c("hole_id", "from", "to"))
    }, error = function(e) {
      showNotification(paste("Error saat menggabungkan data:", e$message), type = "error", duration = 15); NULL
    })
  })
  
  output$combinedDataTable <- renderDT({
    validate(need(!is.null(combinedData()), "Gagal memuat data. Periksa file input dan definisi kolom."))
    datatable(head(combinedData(), 100), options = list(pageLength = 10, scrollX = TRUE, scrollCollapse = TRUE))
  })
  
  # --- BAGIAN 2: VISUALISASI DAN ANALISIS ---
  
  numeric_assay_cols <- reactive({
    req(assay_std())
    assay_std() %>% 
      select(where(is.numeric), -any_of(c("from", "to"))) %>% 
      names()
  })
  
  # --- 2b. Peta Lokasi Bor ---
  output$mapColorSelectUI <- renderUI({
    selectInput("mapColorColumn", "Warnai Peta Berdasarkan Rata-rata:", 
                choices = c("Tidak Ada" = "", numeric_assay_cols()))
  })
  
  output$drillholeMap <- renderPlotly({
    validate(need(combinedData(), "Unggah dan definisikan file untuk melihat peta."))
    
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
                                 colorbar = list(title = paste("Rata-rata", input$mapColorColumn))),
                   text = paste("Hole ID:", plot_data$hole_id, "<br>Rata-rata", input$mapColorColumn, ":", round(plot_data$avg_grade, 3)),
                   hoverinfo = "text")
      } else {
        req(input$manual_breaks != "") 
        
        breaks_num <- as.numeric(unlist(strsplit(input$manual_breaks, ",")))
        breaks_num <- sort(na.omit(breaks_num))
        validate(need(length(breaks_num) > 0, "Masukkan setidaknya satu angka valid untuk interval."))
        
        num_intervals <- length(breaks_num)
        palette_info <- RColorBrewer::brewer.pal.info[input$discrete_palette, ]
        validate(need(num_intervals <= palette_info$maxcolors, 
                      paste("Palet", input$discrete_palette, "hanya mendukung maks.", palette_info$maxcolors, "warna.")))
        
        colors <- RColorBrewer::brewer.pal(max(3, num_intervals), input$discrete_palette)[1:num_intervals]
        breaks_vec <- c(-Inf, breaks_num)
        
        plot_data$grade_category <- cut(plot_data$avg_grade, breaks = breaks_vec, labels = FALSE)
        plot_data <- plot_data %>% filter(!is.na(grade_category))
        validate(need(nrow(plot_data) > 0, "Tidak ada data yang masuk dalam rentang interval yang diberikan."))
        
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
                                   text = ~paste("Hole ID:", hole_id, "<br>Rata-rata", input$mapColorColumn, ":", round(avg_grade, 3)),
                                   hoverinfo = "text")
          }
        }
      }
    }
    
    p %>% layout(title = "Peta Interaktif Lokasi Lubang Bor",
                 xaxis = list(title = "Koordinat X"),
                 yaxis = list(title = "Koordinat Y", scaleanchor = "x", scaleratio = 1),
                 legend = list(title = list(text = paste("<b> Rata-rata", input$mapColorColumn, "</b>"))))
  })
  
  # --- 2c. Analisis Statistik ---
  output$gradeSelectInput <- renderUI({
    selectInput("selectedGrades", "Pilih Grade untuk Analisis:", 
                choices = numeric_assay_cols(), multiple = TRUE, selected = numeric_assay_cols()[1])
  })
  
  output$boxplotGradeSelectUI <- renderUI({
    req(input$selectedGrades)
    selectInput("selectedBoxplotGrade", "Pilih Grade untuk Boxplot:", 
                choices = input$selectedGrades, selected = input$selectedGrades[1])
  })
  
  output$summaryStatsTable <- renderDT({
    req(combinedData(), input$selectedGrades)
    validate(need(length(input$selectedGrades) > 0, "Pilih setidaknya satu grade."))
    validate(need(all(input$selectedGrades %in% names(combinedData())), "Satu atau lebih grade yang dipilih tidak valid. Mohon pilih ulang."))
    
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
    validate(need(length(input$selectedGrades) > 0, "Pilih setidaknya satu grade."))
    validate(need(all(input$selectedGrades %in% names(combinedData())), "Satu atau lebih grade yang dipilih tidak valid. Mohon pilih ulang."))
    
    data_plot <- combinedData() %>%
      select(all_of(input$selectedGrades)) %>%
      pivot_longer(everything(), names_to = "Grade", values_to = "Value")
    
    plot_ly(data_plot, x = ~Value, color = ~Grade, type = "histogram", alpha = 0.7, colors = "viridis") %>%
      layout(title = "Histogram Distribusi Grade", xaxis = list(title = "Nilai"), yaxis = list(title = "Frekuensi"), barmode = "overlay")
  })
  
  output$lithologyBoxplot <- renderPlotly({
    req(combinedData(), input$selectedBoxplotGrade)
    validate(need("lithology" %in% names(combinedData()), "Kolom 'lithology' tidak ditemukan."))
    validate(need(input$selectedBoxplotGrade %in% names(combinedData()), "Grade yang dipilih untuk boxplot tidak valid."))
    
    # Gunakan rv$litho_colors yang sudah diperbarui
    validate(need(length(rv$litho_colors) > 0, "Warna litologi belum diatur. Silakan atur di tab 'Pengaturan Warna Litologi'."))
    
    p <- ggplot(combinedData(), aes(x = lithology, y = .data[[input$selectedBoxplotGrade]], fill = lithology)) +
      geom_boxplot() +
      scale_fill_manual(values = rv$litho_colors) +
      labs(title = paste("Boxplot", input$selectedBoxplotGrade, "Berdasarkan Litologi"), x = "Litologi", y = "Nilai") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # --- 2d. Plot Downhole ---
  output$holeSelectInput <- renderUI({
    req(collar_std())
    hole_ids <- unique(collar_std()$hole_id)
    selectInput("selectedHole", "Pilih Hole ID:", choices = sort(hole_ids), selected = hole_ids[1])
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
                  "Pilih setidaknya satu kolom data untuk ditampilkan."))
    
    validate(need(length(rv$litho_colors) > 0, "Warna litologi belum diatur. Silakan atur di tab 'Pengaturan Warna Litologi'."))
    
    hole_data <- combinedData() %>%
      filter(hole_id == input$selectedHole) %>%
      arrange(from)
    
    validate(need(nrow(hole_data) > 0, "Data tidak ditemukan untuk Hole ID ini."))
    
    p_litho <- ggplot(hole_data) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = from, ymax = to, fill = lithology,
                    text = paste("From:", from, "<br>To:", to, "<br>Litho:", lithology))) +
      scale_y_reverse(name = "Kedalaman (m)") +
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
      layout(title = paste("Plot Downhole untuk", input$selectedHole))
  })
  
  # --- 2e. Logika Pengaturan Warna Kustom (STABIL & EFISIEN) ---
  
  unique_lithos_reactive <- reactive({
    req(combinedData())
    unique_lithos <- unique(sort(combinedData()$lithology))
    unique_lithos[!is.na(unique_lithos)]
  })
  
  # Membuat warna default saat data pertama kali dimuat
  observeEvent(unique_lithos_reactive(), {
    req(unique_lithos_reactive())
    
    current_lithos <- names(rv$litho_colors)
    new_lithos <- unique_lithos_reactive()
    
    # Hanya perbarui jika ada litologi baru
    if(!identical(current_lithos, new_lithos)){
      palette <- colorRampPalette(brewer.pal(8, "Set2"))(length(new_lithos))
      rv$litho_colors <- setNames(palette, new_lithos)
    }
  }, once = TRUE) # Hanya dijalankan sekali saat data dimuat
  
  # UI untuk memilih litologi
  output$selectLithoToColorUI <- renderUI({
    req(rv$litho_colors)
    selectInput("selected_litho_color", "1. Pilih Litologi untuk Diubah:",
                choices = names(rv$litho_colors))
  })
  
  # UI untuk memilih warna baru
  output$pickNewColorUI <- renderUI({
    req(input$selected_litho_color)
    
    colourpicker::colourInput("new_litho_color", "2. Pilih Warna Baru:",
                              value = rv$litho_colors[input$selected_litho_color])
  })
  
  # UI untuk menyalin warna dari litologi lain
  output$copyLithoColorUI <- renderUI({
    req(rv$litho_colors)
    selectInput("copy_from_litho", "3. Atau, Salin Warna dari Litologi Lain:",
                choices = c("Tidak Ada" = "", names(rv$litho_colors)))
  })
  
  # Observer untuk memperbarui warna
  observe({
    req(input$selected_litho_color, input$new_litho_color, rv$litho_colors)
    
    # Logika untuk pembaruan langsung dari color picker
    if(!is.null(input$new_litho_color)){
      temp_colors <- rv$litho_colors
      temp_colors[input$selected_litho_color] <- input$new_litho_color
      rv$litho_colors <- temp_colors
    }
  })
  
  # Observer untuk menyalin warna
  observeEvent(input$copy_from_litho, {
    req(input$copy_from_litho != "", input$selected_litho_color, rv$litho_colors)
    
    temp_colors <- rv$litho_colors
    temp_colors[input$selected_litho_color] <- rv$litho_colors[input$copy_from_litho]
    rv$litho_colors <- temp_colors
    
    # Update color picker agar sesuai
    updateColourInput(session, "new_litho_color", value = rv$litho_colors[input$selected_litho_color])
    
    # Reset dropdown penyalinan
    updateSelectInput(session, "copy_from_litho", selected = "")
  })
  
  # Menampilkan tabel pratinjau warna
  output$lithoColorPreviewTable <- renderDT({
    req(rv$litho_colors)
    
    df <- data.frame(
      Litologi = names(rv$litho_colors),
      Kode_Warna = unname(rv$litho_colors),
      Pratinjau = paste0("<div style='background-color:", unname(rv$litho_colors), "; width:100%; height:20px;'></div>")
    )
    
    datatable(df, 
              escape = FALSE, # Agar HTML untuk pratinjau bisa dirender
              options = list(dom = 't', ordering = FALSE), 
              rownames = FALSE)
  })
  
}

# --- 4. Jalankan Aplikasi ---
shinyApp(ui, server)

