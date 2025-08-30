# app.R
# Aplikasi R Shiny untuk analisis interaktif data bor geologi.
# Versi ini telah diperbaiki untuk menangani masalah join data akibat presisi floating-point.

# Memuat library yang diperlukan.
library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(ggplot2) # Diperlukan untuk downhole plot
library(RColorBrewer) # Tambahan untuk palet warna
library(viridisLite) # Tambahan untuk palet kontinu
library(rlang) # Diperlukan untuk sym() dan .data

# --- Definisi UI (User Interface) ---
ui <- navbarPage(
  "Analisis Data Bor Geologi", # Judul utama aplikasi
  
  # Tab 1: Unggah Data, Integrasi, & Definisi Kolom
  tabPanel("Unggah & Integrasi Data",
           sidebarLayout(
             sidebarPanel(
               h3("Unggah File CSV"),
               helpText("1. Unggah keempat file CSV Anda."),
               fileInput("collarFile", "a. Unggah collar.csv", accept = ".csv"),
               fileInput("assayFile", "b. Unggah assay.csv", accept = ".csv"),
               fileInput("lithoFile", "c. Unggah litho.csv", accept = ".csv"),
               fileInput("surveyFile", "d. Unggah survey.csv (Opsional)", accept = ".csv"),
               
               radioButtons("sep", "Pilih Pemisah CSV (Separator):",
                            choices = c(Koma = ",",
                                        Titik_Koma = ";"),
                            selected = ";"), # Diubah default ke titik koma sesuai file Anda
               
               hr(),
               h3("Definisi Nama Kolom"),
               helpText("2. Pilih nama kolom yang sesuai dari file yang diunggah. Dropdown akan muncul setelah Anda mengunggah file."),
               
               # Kolom untuk file Collar
               uiOutput("colHoleIDUI"),
               uiOutput("colXUI"),
               uiOutput("colYUI"),
               uiOutput("colZUI"),
               
               # Kolom untuk file Assay dan Litho (diasumsikan sama)
               uiOutput("colFromUI"),
               uiOutput("colToUI"),
               
               # Kolom khusus Litho
               uiOutput("colLithoUI"),
               
               # Kolom khusus Survey
               uiOutput("colSurveyDepthUI"),
               uiOutput("colSurveyAzimuthUI"),
               uiOutput("colSurveyDipUI")
             ),
             mainPanel(
               h3("Pratinjau Data Gabungan"),
               helpText("Tabel ini akan terisi setelah semua file yang diperlukan diunggah dan kolom-kolom didefinisikan."),
               DTOutput("combinedDataTable")
             )
           )
  ),
  
  # Tab 2: Peta Lokasi Bor
  tabPanel("Peta Lokasi Bor",
           sidebarLayout(
             sidebarPanel(
               h3("Opsi Plot Peta"),
               helpText("Plot peta dasar akan muncul secara otomatis. Untuk mewarnai titik-titik, definisikan kolom yang relevan di tab pertama."),
               uiOutput("mapColorSelectUI")
             ),
             mainPanel(
               h3("Peta Interaktif Lokasi Lubang Bor"),
               plotlyOutput("drillholeMap", height = "600px")
             )
           )
  ),
  
  # Tab 3: Analisis Statistik & Distribusi Grade
  tabPanel("Analisis Statistik",
           sidebarLayout(
             sidebarPanel(
               h3("Opsi Grade"),
               uiOutput("gradeSelectInput"), # Input untuk memilih grade (multi-select)
               helpText("Pilih satu atau lebih grade untuk analisis statistik dan histogram."),
               hr(),
               uiOutput("boxplotGradeSelectUI"), # Input untuk boxplot (single-select)
               helpText("Pilih satu grade untuk boxplot berdasarkan litologi.")
             ),
             mainPanel(
               h3("Ringkasan Statistik Deskriptif"),
               DTOutput("summaryStatsTable"),
               hr(),
               h3("Histogram Distribusi Grade"),
               plotlyOutput("gradeHistogram"),
               hr(),
               h3("Boxplot Grade Berdasarkan Litologi"),
               plotlyOutput("lithologyBoxplot")
             )
           )
  ),
  
  # Tab 4: Plot Batang (Downhole Plot) & Penampang
  tabPanel("Plot Downhole",
           sidebarLayout(
             sidebarPanel(
               h3("Pilih Lubang Bor"),
               uiOutput("holeSelectInput"), # Input yang dibuat secara dinamis
               helpText("Pilih satu Hole_ID untuk menampilkan downhole plot.")
             ),
             mainPanel(
               h3("Downhole Plot Interaktif"),
               plotlyOutput("downholePlot", height = "800px")
             )
           )
  )
)

# --- Definisi Server (Logika Aplikasi) ---
server <- function(input, output, session) {
  
  # Objek reaktif untuk menyimpan data yang diunggah
  collar_data <- reactive({
    req(input$collarFile)
    read.csv(input$collarFile$datapath, stringsAsFactors = FALSE, sep = input$sep)
  })
  
  assay_data <- reactive({
    req(input$assayFile)
    read.csv(input$assayFile$datapath, stringsAsFactors = FALSE, sep = input$sep)
  })
  
  litho_data <- reactive({
    req(input$lithoFile)
    read.csv(input$lithoFile$datapath, stringsAsFactors = FALSE, sep = input$sep)
  })
  
  survey_data <- reactive({
    if (is.null(input$surveyFile)) return(NULL)
    read.csv(input$surveyFile$datapath, stringsAsFactors = FALSE, sep = input$sep)
  })
  
  # UI Dinamis untuk Definisi Kolom
  output$colHoleIDUI <- renderUI({
    req(collar_data())
    selectInput("col_holeid", "Kolom Hole_ID:", choices = names(collar_data()), selected = "Hole")
  })
  
  output$colXUI <- renderUI({
    req(collar_data())
    selectInput("col_x", "Kolom X (Easting):", choices = names(collar_data()), selected = "Easting")
  })
  
  output$colYUI <- renderUI({
    req(collar_data())
    selectInput("col_y", "Kolom Y (Northing):", choices = names(collar_data()), selected = "Northing")
  })
  
  output$colZUI <- renderUI({
    req(collar_data())
    selectInput("col_z", "Kolom Z (RL):", choices = names(collar_data()), selected = "Z")
  })
  
  output$colFromUI <- renderUI({
    req(assay_data())
    selectInput("col_from", "Kolom From (Dari):", choices = names(assay_data()), selected = "Dari")
  })
  
  output$colToUI <- renderUI({
    req(assay_data())
    selectInput("col_to", "Kolom To (Sampai):", choices = names(assay_data()), selected = "Sampai")
  })
  
  output$colLithoUI <- renderUI({
    req(litho_data())
    selectInput("col_litho", "Kolom Litologi:", choices = names(litho_data()), selected = "Litho")
  })
  
  output$colSurveyDepthUI <- renderUI({
    if (is.null(survey_data())) return(NULL)
    selectInput("col_survey_depth", "Kolom Kedalaman (Survey):", choices = names(survey_data()))
  })
  
  output$colSurveyAzimuthUI <- renderUI({
    if (is.null(survey_data())) return(NULL)
    selectInput("col_survey_azimuth", "Kolom Azimuth (Survey):", choices = names(survey_data()))
  })
  
  output$colSurveyDipUI <- renderUI({
    if (is.null(survey_data())) return(NULL)
    selectInput("col_survey_dip", "Kolom Dip (Survey):", choices = names(survey_data()))
  })
  
  # Objek reaktif untuk menyimpan data gabungan yang telah dibersihkan
  combinedData <- reactive({
    
    # Validasi input kolom. Aplikasi akan menunggu hingga input kolom tersedia
    req(
      input$col_holeid, input$col_x, input$col_y, input$col_z,
      input$col_from, input$col_to, input$col_litho
    )
    
    tryCatch({
      collar <- collar_data()
      assay <- assay_data()
      litho <- litho_data()
      survey <- survey_data()
      
      # Merename kolom di setiap dataframe.
      collar <- collar %>%
        rename(Hole_ID = !!sym(input$col_holeid), X = !!sym(input$col_x), Y = !!sym(input$col_y), Z = !!sym(input$col_z))
      
      assay <- assay %>%
        rename(Hole_ID = !!sym(input$col_holeid), From = !!sym(input$col_from), To = !!sym(input$col_to))
      
      litho <- litho %>%
        rename(Hole_ID = !!sym(input$col_holeid), From = !!sym(input$col_from), To = !!sym(input$col_to), Litho = !!sym(input$col_litho))
      
      # =========================================================
      # <<< BAGIAN YANG DIPERBARUI >>>
      # =========================================================
      # Konversi tipe data secara eksplisit untuk memastikan konsistensi
      assay$Hole_ID <- as.character(assay$Hole_ID)
      assay$From    <- as.numeric(assay$From)
      assay$To      <- as.numeric(assay$To)
      
      collar$Hole_ID <- as.character(collar$Hole_ID)
      
      litho$Hole_ID <- as.character(litho$Hole_ID)
      litho$From    <- as.numeric(litho$From)
      litho$To      <- as.numeric(litho$To)
      
      # Bulatkan kolom kunci numerik untuk menghindari error presisi floating-point saat join
      assay <- assay %>% mutate(
        From = round(From, 3),
        To = round(To, 3)
      )
      litho <- litho %>% mutate(
        From = round(From, 3),
        To = round(To, 3)
      )
      # =========================================================
      # <<< AKHIR BAGIAN YANG DIPERBARUI >>>
      # =========================================================
      
      # Menggabungkan data
      data_merged_1 <- left_join(assay, collar, by = "Hole_ID")
      data_final <- left_join(data_merged_1, litho, by = c("Hole_ID", "From", "To"))
      
      # Gabungkan data survey jika ada
      if (!is.null(survey)) {
        req(input$col_survey_depth, input$col_survey_azimuth, input$col_survey_dip)
        survey <- survey %>%
          rename(Hole_ID = !!sym(input$col_holeid), DEPTH = !!sym(input$col_survey_depth), AZIMUTH = !!sym(input$col_survey_azimuth), DIP = !!sym(input$col_survey_dip))
        survey$Hole_ID <- as.character(survey$Hole_ID)
        data_final <- left_join(data_final, survey, by = "Hole_ID")
      }
      
      return(data_final)
    }, error = function(e) {
      showNotification(paste("Error saat membaca atau menggabungkan data: ", e$message), type = "error", duration = 15)
      return(NULL)
    })
  })
  
  # ... (Sisa kode server tidak perlu diubah, biarkan seperti semula) ...
  # Objek reaktif untuk data collar yang telah diproses untuk peta
  processed_collar_data <- reactive({
    req(collar_data(), input$col_holeid, input$col_x, input$col_y)
    
    collar_df <- collar_data() %>%
      rename(
        Hole_ID = !!sym(input$col_holeid),
        X = !!sym(input$col_x),
        Y = !!sym(input$col_y)
      )
    
    # Tambahkan rata-rata grade jika data assay dan grade terpilih tersedia
    if (!is.null(assay_data()) && !is.null(input$mapColorColumn) && input$mapColorColumn != "") {
      # Hanya hitung rata-rata jika kolom yang dipilih memang ada di assay
      if(input$mapColorColumn %in% names(assay_data())){
        assay_df <- assay_data() %>%
          rename(Hole_ID = !!sym(input$col_holeid))
        
        avg_grades <- assay_df %>%
          group_by(Hole_ID) %>%
          summarise(Avg_Grade = mean(.data[[input$mapColorColumn]], na.rm = TRUE))
        
        merged_data <- left_join(collar_df, avg_grades, by = "Hole_ID")
        return(merged_data)
      }
    }
    return(collar_df)
  })
  
  # Output: Pratinjau tabel data gabungan
  output$combinedDataTable <- renderDT({
    validate(
      need(!is.null(combinedData()), "Silakan unggah semua file yang diperlukan dan pastikan nama kolom benar.")
    )
    datatable(head(combinedData(), 100), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # UI dinamis untuk memilih kolom warna di peta
  output$mapColorSelectUI <- renderUI({
    req(assay_data())
    # Opsi warna adalah semua kolom numerik di file assay
    numeric_cols <- names(assay_data())[sapply(assay_data(), is.numeric)]
    exclude_cols <- c(input$col_from, input$col_to) # Kolom yang bukan 'grade'
    color_choices <- setdiff(numeric_cols, exclude_cols)
    
    selectInput("mapColorColumn", "Warnai Peta Berdasarkan Rata-rata:", choices = c("Tidak Ada" = "", color_choices))
  })
  
  # Output: Peta lokasi bor
  output$drillholeMap <- renderPlotly({
    validate(
      need(processed_collar_data(), "Silakan unggah file collar.csv untuk melihat peta lokasi bor."),
      need(input$col_x, "Silakan pilih kolom X."),
      need(input$col_y, "Silakan pilih kolom Y.")
    )
    
    plot_data <- processed_collar_data()
    
    validate(
      need(all(c("X", "Y") %in% colnames(plot_data)), "Pastikan file collar.csv memiliki kolom koordinat."),
      need(is.numeric(plot_data$X) && is.numeric(plot_data$Y), "Kolom koordinat harus numerik.")
    )
    
    p <- plot_ly(data = plot_data, x = ~X, y = ~Y, type = 'scatter', mode = 'markers',
                 text = ~Hole_ID, hoverinfo = 'text')
    
    if (!is.null(input$mapColorColumn) && input$mapColorColumn != "" && "Avg_Grade" %in% names(plot_data)) {
      p <- p %>% add_markers(
        color = ~Avg_Grade,
        colorscale = 'Viridis',
        marker = list(showscale = TRUE),
        text = ~paste("Hole_ID:", Hole_ID, "<br>Avg", input$mapColorColumn, ":", round(Avg_Grade, 2)),
        hoverinfo = "text"
      )
    } 
    
    p %>%
      layout(
        title = "Peta Interaktif Lokasi Lubang Bor",
        xaxis = list(title = "Koordinat X"),
        yaxis = list(title = "Koordinat Y", scaleanchor = "x", scaleratio = 1)
      )
  })
  
  # UI dinamis untuk memilih kolom grade
  output$gradeSelectInput <- renderUI({
    req(assay_data())
    
    numeric_cols <- sapply(assay_data(), is.numeric)
    # Asumsikan kolom kunci tidak akan dipilih sebagai grade
    exclude_cols_from_assay <- c(input$col_holeid, input$col_from, input$col_to)
    
    # Handle the case where a user might select non-existent columns initially
    valid_cols <- names(assay_data())[numeric_cols]
    grade_cols <- setdiff(valid_cols, exclude_cols_from_assay)
    
    selectInput("selectedGrades", "Pilih Grade:", choices = grade_cols, multiple = TRUE, selected = grade_cols[1])
  })
  
  # UI dinamis untuk boxplot (single-select)
  output$boxplotGradeSelectUI <- renderUI({
    req(input$selectedGrades)
    selectInput("selectedBoxplotGrade", "Pilih Grade untuk Boxplot:", choices = input$selectedGrades, selected = input$selectedGrades[1])
  })
  
  # Output: Tabel statistik deskriptif
  output$summaryStatsTable <- renderDT({
    req(combinedData(), input$selectedGrades)
    validate(
      need(length(input$selectedGrades) > 0, "Silakan pilih setidaknya satu grade."),
      need(all(input$selectedGrades %in% colnames(combinedData())), "Kolom grade tidak ditemukan di data gabungan.")
    )
    
    summary_df <- combinedData() %>%
      select(all_of(input$selectedGrades)) %>%
      pivot_longer(everything(), names_to = "Grade", values_to = "Value") %>%
      group_by(Grade) %>%
      summarise(
        Mean = round(mean(Value, na.rm = TRUE), 3),
        Median = round(median(Value, na.rm = TRUE), 3),
        Min = round(min(Value, na.rm = TRUE), 3),
        Max = round(max(Value, na.rm = TRUE), 3),
        `Std Dev` = round(sd(Value, na.rm = TRUE), 3),
        `Q1` = round(quantile(Value, 0.25, na.rm = TRUE), 3),
        `Q3` = round(quantile(Value, 0.75, na.rm = TRUE), 3)
      )
    
    datatable(summary_df, options = list(pageLength = 10))
  })
  
  # Output: Histogram
  output$gradeHistogram <- renderPlotly({
    req(combinedData(), input$selectedGrades)
    validate(
      need(length(input$selectedGrades) > 0, "Silakan pilih setidaknya satu grade.")
    )
    
    data_plot <- combinedData() %>%
      select(all_of(input$selectedGrades)) %>%
      pivot_longer(everything(), names_to = "Grade", values_to = "Value")
    
    p <- plot_ly(data_plot, x = ~Value, color = ~Grade, type = "histogram", alpha = 0.7) %>%
      layout(
        title = "Histogram Distribusi Grade",
        xaxis = list(title = "Nilai Grade"),
        yaxis = list(title = "Frekuensi"),
        barmode = "overlay"
      )
    
    return(p)
  })
  
  # Output: Boxplot
  output$lithologyBoxplot <- renderPlotly({
    req(combinedData(), input$selectedBoxplotGrade)
    
    validate(
      need("Litho" %in% colnames(combinedData()), "Kolom 'Litho' tidak ditemukan. Periksa kembali definisi kolom litologi Anda."),
      need(input$selectedBoxplotGrade %in% colnames(combinedData()), "Grade yang dipilih tidak ditemukan di data gabungan.")
    )
    
    p <- ggplot(combinedData(), aes(x = Litho, y = .data[[input$selectedBoxplotGrade]], fill = Litho)) +
      geom_boxplot() +
      labs(
        title = paste("Boxplot", input$selectedBoxplotGrade, "Berdasarkan Litologi"),
        x = "Litologi",
        y = paste("Nilai", input$selectedBoxplotGrade),
        fill = "Litologi"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # UI dinamis untuk memilih Hole_ID
  output$holeSelectInput <- renderUI({
    req(combinedData())
    hole_ids <- unique(combinedData()$Hole_ID)
    selectInput("selectedHole", "Pilih Hole ID:", choices = sort(hole_ids), selected = hole_ids[1])
  })
  
  # Output: Plot downhole
  output$downholePlot <- renderPlotly({
    req(combinedData(), input$selectedHole, input$selectedGrades)
    validate(
      need(input$selectedHole, "Silakan pilih Hole ID.")
    )
    
    hole_data <- combinedData() %>%
      filter(Hole_ID == input$selectedHole)
    
    validate(
      need(nrow(hole_data) > 0, "Data tidak ditemukan untuk Hole ID yang dipilih.")
    )
    
    p_litho <- ggplot(hole_data) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = From, ymax = To, fill = Litho, 
                    text = paste("From:", From, "<br>To:", To, "<br>Litho:", Litho))) +
      scale_y_reverse(name = "Kedalaman (m)") +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank()
      ) +
      labs(fill = "Litologi")
    
    grade_data_long <- hole_data %>%
      select(From, To, all_of(input$selectedGrades)) %>%
      pivot_longer(
        cols = -c(From, To),
        names_to = "Grade",
        values_to = "Value"
      )
    
    p_grades <- ggplot(grade_data_long, aes(y = (From + To) / 2, x = Value, color = Grade,
                                            text = paste("Depth:", (From+To)/2, "<br>Grade:", Grade, "<br>Value:", Value))) +
      geom_line() +
      geom_point() +
      scale_y_reverse() +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
      ) +
      labs(x = "Nilai Grade", color = "Grade")
    
    subplot(
      ggplotly(p_litho, tooltip = "text"), 
      ggplotly(p_grades, tooltip = "text"),
      nrows = 1, 
      shareY = TRUE, 
      widths = c(0.2, 0.8), # Beri porsi lebih besar untuk plot grade
      titleX = TRUE
    ) %>%
      layout(
        title = paste("Downhole Plot untuk", input$selectedHole),
        legend = list(orientation = 'h', y = -0.2)
      )
  })
}


# --- Jalankan aplikasi ---
shinyApp(ui, server)