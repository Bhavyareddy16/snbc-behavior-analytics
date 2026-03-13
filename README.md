# MSNBC Behavior Analytics
Intelligent Customer Behavior Analytics for UX Optimization and Security Risk Detection

This R project processes the MSNBC Anonymous Web Dataset containing approx 989,818 user sessions, extracts behavioral features, models a UX Friction Score, generates User Segments using K-Means clustering, and detects Security Anomalies with an Isolation Forest.

The final output is an interactive Shiny application.

## Quick Start

1. Install dependencies: `shiny`, `shinydashboard`, `ggplot2`, `dplyr`, `DT`, `isotree`, `cluster`, `tidyr`
2. Run `data_prep.R` to download processing data and export `.rds` objects.
3. Keep the `.rds` files in `/data/` and run `app.R` or `run_app.R` to view the Shiny Dashboard.
