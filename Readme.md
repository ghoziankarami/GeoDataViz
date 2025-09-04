# GeoDataViz: Geological Drillhole Data Analyzer

## Overview

**GeoDataViz** is a comprehensive, interactive web application built with R Shiny, designed as an all-in-one workbench for geologists, data analysts, and students. It transforms raw drillhole data (collar, assay, lithology) into actionable geological insights through powerful visualization, robust statistical analysis, and interactive data validation tools.

Developed by a Senior Geologist for both educational and professional use, this application provides an open-source solution for the entire initial data analysis workflow, from data loading and cleaning to advanced geostatistical QA/QC, without the need for proprietary software.

## License

This project is licensed under the MIT License. See the [LICENSE](https://www.google.com/search?q=LICENSE "null") file for details.

## Target Audience

This application is ideal for:

-   **Resource & Exploration Geologists:** For quick exploratory data analysis (EDA), identifying correlations between elements, and validating assay data.

-   **Mine Geologists & Grade Control Engineers:** For performing outlier analysis, simulating top cuts, and understanding grade distribution within different geological domains.

-   **Geoscience Students & Academics:** As a powerful, hands-on tool for learning and teaching the principles of geological data analysis and geostatistics.

-   **Data Analysts in the Mining Sector:** For rapidly prototyping analyses and generating key statistical summaries from drillhole datasets.

## How to Run Locally

To run this application on your own computer, you will need R and RStudio.

### 1. Installation

-   **R:** Download and install the base R programming language from the [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/ "null").

-   **RStudio:** Download and install the free version of RStudio Desktop from the [Posit website](https://posit.co/download/rstudio-desktop/ "null").

### 2. Dependencies

Once R and RStudio are installed, you need to install the R packages that this application depends on. Open RStudio and run the following command in the console:

```         
install.packages(c("shiny", "dplyr", "tidyr", "plotly", "DT", "ggplot2", "rlang", "readxl", "janitor", "shinyjs", "RColorBrewer", "colourpicker", "GGally"))  
```

### 3. Running the Application

1.  Download the project files from the GitHub repository, which include `app.R` and the sample data CSV files (`collar.csv`, `assay.csv`, `lithology.csv`, `survey.csv`).

2.  Place all these files together in a single folder on your computer.

3.  Open the `app.R` file in RStudio.

4.  Click the **"Run App"** button that appears at the top of the RStudio editor.

## Key Features by Tab

The application's workflow is logically structured into several tabs, each serving a specific purpose in the analysis pipeline.

### 1. Data Input & Integration

This is the starting point for any analysis. It is designed with transparency and ease of use in mind.

-   **Dual Data Source Mode:** Users can instantly start analyzing with the built-in **sample dataset** or choose to **upload their own data** in either multi-file CSV or single-file Excel formats.

-   **Data Security & Privacy:** A prominent panel assures users that their data is secure. The application **does not store any uploaded data**; all processing happens in a temporary session and is permanently deleted when the browser is closed.

-   **Data Preview:** Immediately upon loading, a preview of the combined dataset is displayed, allowing users to verify that their data has been integrated correctly.

### 2. Column Definitions

This tab provides the crucial step of mapping the user's column names to the application's internal standards, ensuring flexibility for various data formats.

-   **Dynamic Dropdowns:** The application automatically reads the headers from the user's Collar, Assay, and Lithology files and populates dropdown menus.

-   **Smart Suggestions:** It intelligently suggests default mappings by searching for common keywords (e.g., "hole_id", "from", "ni", "lithology").

### 3. Drillhole Location Map

An interactive 2D plan map for visualizing the spatial distribution of drillholes.

-   **Interactive Plot:** Users can pan, zoom, and hover over points to see details like Hole ID and coordinates.

-   **Dynamic Coloring:** Points can be colored by the average grade of any selected numeric variable (e.g., Ni, Fe, Co).

-   **Adjustable Aesthetics:** Users can control the marker size and choose from various continuous or discrete color schemes to enhance visualization.

-   **Data Filtering:** In discrete color mode, users can filter the map to display only specific grade intervals, perfect for identifying target areas.

### 4. Statistical Analysis

This is the core QA/QC and data validation engine of the application.

-   **Comprehensive Statistics:** Provides standard summary statistics, interactive histograms, and boxplots to understand data distributions.

-   **Interactive Outlier Analysis:** Automatically identifies outliers using the 1.5xIQR method and presents them in a detailed summary table. Users can:

    -   View a statistical summary (count, average, etc.) of the outliers.

    -   Select specific outliers from the table and click "Remove" to see a live comparison of the dataset's statistics before and after removal.

    -   Download the "cleaned" dataset as a CSV file.

-   **Interactive Top Cut (Capping) Analysis:** Allows users to simulate the effects of capping high-grade values—a critical step in resource estimation. Users can:

    -   Enter a top cut value for any selected grade.

    -   View a before-and-after comparison of the summary statistics.

    -   Download the top-cut dataset as a CSV file.

### 5. Bivariate Analysis

This advanced tab is dedicated to exploring relationships between variables.

-   **Single Pair Regression:**

    -   **Per-Lithology Analysis:** The scatter plot automatically colors data points by lithology and draws a separate regression line for each, allowing for domain-specific correlation analysis.

    -   **Overall Trend:** A dashed black line shows the overall regression trend for all data points combined.

    -   **Quantitative Summary:** Provides a detailed R-squared summary table, breaking down the correlation strength for the overall dataset and for each individual lithology.

-   **Multivariate Matrix Plot:**

    -   **All-in-One EDA:** Users can select multiple numeric variables to generate a comprehensive scatter plot matrix (`ggpairs`).

    -   **Informative Panels:** The matrix is enhanced to show:

        -   **Diagonal:** Histograms for viewing the distribution of each variable.

        -   **Upper Triangle:** Scatter plots with regression lines to visualize relationships.

        -   **Lower Triangle:** The calculated **R-squared value** for each pair, providing a quick quantitative overview of all correlations.

    -   **Fullscreen View:** A button allows the plot to be launched in a large modal window, perfect for detailed inspection and capturing high-quality screenshots.

### 6. Downhole Plot

Provides a classic drillhole log visualization for individual hole analysis.

-   **Multi-Track Display:** The primary track always shows the **lithology log** with user-defined colors.

-   **Flexible Data Columns:** Users can select multiple additional data columns (numeric or text) to display alongside the lithology log.

    -   **Numeric Data** (e.g., Ni, Fe) is displayed as a bar chart profile.

    -   **Text Data** (e.g., sample IDs) is displayed as labels.

### 7. Lithology Color Settings

Offers full control over the visual representation of geological units.

-   **User-Friendly Interface:** Provides a simple dropdown to select a lithology and an interactive color picker to assign a new color.

-   **Consistent Application:** The chosen colors are automatically and consistently applied across all relevant plots in the application (Downhole Plot, Boxplot, Bivariate Plot).

### 8. Session Management

A powerful feature for workflow continuity, designed for local RStudio users.

-   **Save Session:** Users can save their entire session—including loaded data, all input settings, custom colors, and outlier removals—into a single `.rds` file.

-   **Load Session:** Users can upload a saved session file to instantly restore their work, saving significant time and effort.

-   **Clear Guidance:** Includes a prominent note explaining that this feature is for local use and provides a link to the project's GitHub repository for instructions.

### Citation

If you use GeoDataViz in your research, publication, or report, please cite it as follows. Your citation is the best way to support the development of this open-source tool.

**Plain Text:**
> Karami, G. (2025). GeoDataViz: A comprehensive workbench for geological drillhole data analysis. GitHub Repository. https://github.com/ghoziankarami/GeoDataViz

