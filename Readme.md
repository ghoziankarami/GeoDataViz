# GeoDataViz - A Shiny App

A web app for rapid validation and 3D visualization of geological drillhole data.

**[Try the Live App Here!](https://ghoziankarami.shinyapps.io/GeodataViz/)**

## Key Features
- **Interactive Dashboard:** Data summary & 2D drillhole collar map.
- **Automated Data Validation:** Detects common errors like overlaps & gaps.
- **Interactive 3D Visualization:** View and rotate drillhole traces in 2D downhole, with columns that can be customized for color-coding (e.g., by grade or lithology).

## Built With
- R
- Shiny
- Plotly
- Tidyverse

## How to Run Locally
1. Clone this repository.
2. Open the project in RStudio.
3. Run `renv::restore()` to install all required packages.
4. Run the app with `shiny::runApp()`.