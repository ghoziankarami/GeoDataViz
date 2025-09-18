# GeoDataViz: Geological Drillhole Data Analyzer

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17142676.svg)](https://doi.org/10.5281/zenodo.17142676)

Author: **Ghozian Islam Karami**

Senior Geologist & Geospatial Specialist passionate about building open-source solutions to solve challenges in the mining industry.

Contact: [ghoziankarami\@gmail.com](mailto:ghoziankarami@gmail.com){.email} \| [LinkedIn](https://www.linkedin.com/in/ghoziankarami/)

------------------------------------------------------------------------

## Overview

**GeoDataViz** is a comprehensive, interactive web application built with R Shiny, designed as an all-in-one workbench for geologists, data analysts, and students. It transforms raw drillhole data (collar, assay, lithology) into actionable geological insights through powerful visualization, robust statistical analysis, and interactive data validation tools.

Developed for both educational and professional use, this application provides an open-source solution for the entire initial data analysis workflow, from data loading and cleaning to advanced QA/QC, without the need for proprietary software.

## Target Audience

This application is ideal for:

-   **Resource & Exploration Geologists:** For quick exploratory data analysis (EDA), identifying correlations between elements, and validating assay data.
-   **Mine Geologists & Grade Control Engineers:** For performing outlier analysis, simulating top cuts, and understanding grade distribution within different geological domains.
-   **Geoscience Students & Academics:** As a powerful, hands-on tool for learning and teaching the principles of geological data analysis and geostatistics.
-   **Data Analysts in the Mining Sector:** For rapidly prototyping analyses and generating key statistical summaries from drillhole datasets.

------------------------------------------------------------------------
## Project Versions

This repository contains multiple versions of the application managed via Git branches, each serving a different purpose:

-   **`main` (Local Free Version):** This is the primary, stable, open-source version intended for local use. It includes all core features, including **Save & Load Session**. This is the recommended version for most users.

-   **`web-version` (Live Demo):** This branch contains the code deployed to the public Shinyapps.io server. It has the **Save & Load Session** feature removed for simplicity and security in a multi-user web environment.

-   **`pro-version` (Development):** This is a development branch for future premium features, such as automated PDF report generation. It may be unstable and is not intended for general use yet.

## How to Run Locally

To run this application on your own computer, you will need R and RStudio.

### 1. Installation

-   **R:** Download and install the base R programming language from the [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/).
-   **RStudio:** Download and install the free version of RStudio Desktop from the [Posit website](https://posit.co/download/rstudio-desktop/).

### 2. Dependencies

Once R and RStudio are installed, you need to install the R packages that this application depends on. Open RStudio and run the following command in the console:

```{r}
install.packages(c("shiny", "dplyr", "tidyr", "plotly", "DT", "ggplot2", "rlang", "readxl", "janitor", "shinyjs", "RColorBrewer", "colourpicker", "GGally"))
```

### 3. Running the Application

1.  Download the project files from this GitHub repository, which include `app.R` and the sample data CSV files (`collar.csv`, `assay.csv`, `lithology.csv`).

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

-   **Smart Suggestions:** It intelligently suggests default mappings by searching for common keywords (e.g., "hole_id", "from", "lithology").

### 3. Data Validation Summary

This tab is a dedicated QA/QC dashboard for performing a "health check" on the loaded data before analysis.

-   **Record Counts:** Displays a simple table showing the total number of rows for each uploaded file.

-   **Cross-File Record Matching:** Provides two tables to instantly find mismatches:

    -   A list of `Hole ID`s that exist in the **Collar** file but are missing from the **Assay** file.

    -   A list of `Hole ID`s that exist in the **Assay** file but are missing from the **Collar** file.

-   **Interval Error Detection:** Automatically scans the assay table for a given `Hole ID` and reports any intervals where the `From` value does not equal the previous `To` value, instantly highlighting gaps or overlaps.

### 4. Drillhole Location Map

An interactive 2D plan map for visualizing the spatial distribution of drillholes.

-   **Interactive Plot:** Users can pan, zoom, and hover over points to see details like Hole ID and coordinates.

-   **Dynamic Coloring:** Points can be colored by the average grade of any selected numeric variable (e.g., Ni, Fe, Co), providing an immediate visual cue for spatial trends.

-   **Adjustable Aesthetics:** Users can control the marker size and choose from various color schemes.

### 5. Statistical Analysis

This is the core analytical engine of the application.

-   **Comprehensive Statistics:** Provides standard summary statistics, interactive histograms, and boxplots to understand data distributions.

-   **Interactive Outlier Analysis:** Automatically identifies outliers using the 1.5xIQR method and presents them in a summary table. Users can:

    -   View a statistical summary (count, average, etc.) of the outliers.

    -   Select specific outliers and click "Remove" to see a live comparison of the dataset's statistics before and after removal.

    -   Download the "cleaned" dataset as a CSV file.

-   **Interactive Top Cut (Capping) Analysis:** Allows users to simulate the effects of capping high-grade values—a critical step in resource estimation. Users can:

    -   Enter a top cut value for any selected grade.

    -   View a before-and-after comparison of the summary statistics.

### 6. Bivariate Analysis

This advanced tab is dedicated to exploring relationships between variables.

-   **Single Pair Regression:**

    -   **Per-Lithology Analysis:** The scatter plot automatically colors data points by lithology and draws a separate regression line for each, allowing for domain-specific correlation analysis.

    -   **Quantitative Summary:** Provides a detailed R-squared summary table, breaking down the correlation strength for each individual lithology.

-   **Multivariate Matrix Plot:**

    -   **All-in-One EDA:** Users can select multiple numeric variables to generate a comprehensive scatter plot matrix (`ggpairs`).

    -   **Informative Panels:** The matrix shows histograms, scatter plots with regression lines, and the calculated R-squared value for each pair.

### 7. Downhole Plot

Provides a classic drillhole log visualization for individual hole analysis.

-   **Multi-Track Display:** The primary track always shows the **lithology log** with user-defined colors.

-   **Flexible Data Columns:** Users can select multiple additional data columns (numeric or text) to display as separate tracks alongside the lithology log. Numeric data is shown as a bar chart profile, while text data is shown as labels.

### 8. Lithology Color Settings

Offers full control over the visual representation of geological units.

-   **User-Friendly Interface:** Provides a simple dropdown to select a lithology and an interactive color picker to assign a new color.

-   **Consistent Application:** The chosen colors are automatically and consistently applied across all relevant plots in the application (Downhole Plot, Boxplot, Bivariate Plot).

## License

This project is licensed under the MIT License. See the [LICENSE](https://github.com/ghoziankarami/GeoDataViz/blob/main/LICENSE.md) file for details.

## Citation

If you use GeoDataViz in your research, publication, or report, please support this open-source project by citing it.

**Preferred Format (Zenodo):**

> ghoziankarami. (2025). ghoziankarami/GeoDataViz: v1.0.0 - Public Release (v1.0.0). Zenodo. <https://doi.org/10.5281/zenodo.17142676>

**BibTeX format:**

Code snippet

```         
@software{ghoziankarami_2025_17142676,   author       = {ghoziankarami},   title        = {ghoziankarami/GeoDataViz: v1.0.0 - Public Release},   month        = sep,   year         = 2025,   publisher    = {Zenodo},   version      = {v1.0.0},   doi          = {10.5281/zenodo.17142676},   url          = {[https://doi.org/10.5281/zenodo.17142676](https://doi.org/10.5281/zenodo.17142676)} } 
```
