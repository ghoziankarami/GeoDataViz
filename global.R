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
library(purrr) # ## DEPENDENCY: Ditambahkan untuk alur kerja map()

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
