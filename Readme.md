# GeoDataViz: Geological Drillhole Data Analyzer

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17142676.svg)](https://doi.org/10.5281/zenodo.17142676)

**Author: Ghozian Islam Karami** \| **Contact: [ghoziankarami\@gmail.com](mailto:ghoziankarami@gmail.com)** \| [**LinkedIn Profile**](https://www.linkedin.com/in/ghoziankarami/)

------------------------------------------------------------------------

## Overview

**GeoDataViz** is an interactive web application built with R Shiny, designed as an all-in-one workbench for geologists, data analysts, and students. It transforms raw drillhole data (collar, assay, lithology) into actionable insights through powerful visualization, robust statistical analysis, and data validation tools.

This open-source solution covers the entire initial data analysis workflow, from data loading to advanced QA/QC, without the need for proprietary software.

------------------------------------------------------------------------

## How to Get Started

There are two ways to use GeoDataViz:

#### 1. Live Web App (Recommended for Quick Preview)

The easiest way to try GeoDataViz is through the public web app. No installation is required. This version is ideal for demonstration and quick analysis.

> [**🚀 Launch the GeoDataViz Web App**](https://ghoziankarami.shinyapps.io/GeodataViz/)

#### 2. Run Locally (for Full Features)

For the complete experience, including the **Save & Load Session** feature, it is recommended to run the app on your own computer.

**Prerequisites:** - **R:** Install from [CRAN](https://cran.r-project.org/). - **RStudio:** Install the free Desktop version from [Posit](https://posit.co/download/rstudio-desktop/).

**Steps:** 1. **Download:** Download the project files (`app.R`, `collar.csv`, `assay.csv`, `lithology.csv`) from the `main` branch of this repository.

2\. **Install Packages:** Open RStudio and run the following command in the console to install all dependencies: `r     install.packages(c("shiny", "dplyr", "tidyr", "plotly", "DT", "ggplot2", "rlang", "janitor", "shinyjs", "RColorBrewer", "colourpicker", "GGally", "ggrepel", "purrr"))`

3\. **Run:** Open the **`app.R`** file in RStudio and click the **"Run App"** button at the top of the script editor.

------------------------------------------------------------------------

## Core Features

-   **Streamlined CSV Input:** A simplified and robust data loading process focused exclusively on CSV files (collar, assay, lithology), ensuring stability and performance.
-   **Data Validation:** An automated QA/QC dashboard to check for common errors like interval gaps/overlaps and mismatched `Hole ID`s between collar and assay files.
-   **Statistical Analysis:** A comprehensive toolkit including summary statistics (with Coefficient of Variation), interactive histograms with adjustable bins, boxplots by lithology (with zoom), and interactive outlier and top-cut analysis.
-   **Interactive Visualizations:**
    -   **2D Collar Map:** Visualize drillhole locations, color-coded by average grade to identify spatial trends.
    -   **Downhole Plots:** A classic multi-track log display showing lithology and selected grade profiles for any individual drillhole.
    -   **Bivariate Analysis:** Generate scatter plots with regression lines, R-squared values, and equations calculated for both the overall dataset and for each individual lithology.
    -   **Multivariate Matrix:** A `ggpairs` matrix for a rapid overview of correlations and distributions between multiple elements.
-   **Session Management:** Save your entire session state (settings, colors, outlier removals) to a file and load it later to continue your work seamlessly (**local version only**).

------------------------------------------------------------------------

## Project Versions

This repository uses Git branches to manage different versions:

-   **`main` (Local Free Version):** The primary, stable, open-source version with all features, including **Save & Load Session**.
-   **`web-version` (Live Demo):** The code deployed to the public web app, with session management removed.
-   **`pro-version` (Development):** A private branch for developing future premium features.

------------------------------------------------------------------------

## License & Citation

This project is licensed under the **MIT License**.

If you use GeoDataViz in your research, publication, or report, please support this open-source project by citing it using the official DOI:

> ghoziankarami. (2025). ghoziankarami/GeoDataViz: v1.0.0 - Public Release (v1.0.0). Zenodo. <https://doi.org/10.5281/zenodo.17142676>
