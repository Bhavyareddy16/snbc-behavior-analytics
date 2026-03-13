# run_app.R
# Utility script to run the UX & Security Analytics Shiny app
options(shiny.port = 3838, shiny.host = "127.0.0.1")
cat("Starting Shiny application on http://127.0.0.1:3838 ...\n")
shiny::runApp("app.R", launch.browser = FALSE)
