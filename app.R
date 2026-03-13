library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

if (file.exists("data/behavior_data.rds") && file.exists("data/confusing_pages.csv")) {
  behavior_data <- readRDS("data/behavior_data.rds")
  confusing_pages <- read.csv("data/confusing_pages.csv", stringsAsFactors = FALSE)
} else {
  stop("Processed data not found! Please run data_prep.R first.")
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "UX & Security Analytics", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("UX Analytics", tabName = "ux", icon = icon("users")),
      menuItem("User Segmentation", tabName = "segmentation", icon = icon("project-diagram")),
      menuItem("Security Risk", tabName = "security", icon = icon("shield-alt")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); border-top: 3px solid #3c8dbc; }
        .info-box { border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      "))
    ),
    tabItems(
      tabItem(tabName = "overview",
        h2("System Overview"),
        fluidRow(
          valueBoxOutput("totalSessionsBox", width = 4),
          valueBoxOutput("highFricBox", width = 4),
          valueBoxOutput("suspiciousBox", width = 4)
        ),
        fluidRow(
          box(
            title = "Behavioral Sentiment Distribution", status = "primary", solidHeader = TRUE, width = 6,
            plotOutput("sentimentPlot", height = 300)
          ),
          box(
            title = "Top Confusing Pages", status = "warning", solidHeader = TRUE, width = 6,
            plotOutput("confusingPagesPlotSmall", height = 300)
          )
        )
      ),
      tabItem(tabName = "ux",
        h2("UX Friction & Page Confusion"),
        fluidRow(
          box(
            title = "UX Friction Score Distribution", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("frictionDistPlot", height = 400),
            p("Higher scores indicate greater difficulty in navigation (more page repetition, longer paths).")
          )
        ),
        fluidRow(
          box(
            title = "Most Confusing Navigation Areas", status = "warning", solidHeader = TRUE, width = 12,
            plotOutput("confusingPagesPlotLarge", height = 400),
            p("Based on pages frequently visited by high-friction sessions.")
          )
        )
      ),
      tabItem(tabName = "segmentation",
        h2("Behavioral Clusters (K-Means)"),
        fluidRow(
          box(
            title = "User Segmentation based on Behavior", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("clusterPlot", height = 500),
            p("Users are grouped based on Session Duration, Navigation Depth, Target Repetition, and Time Deviations.")
          )
        )
      ),
      tabItem(tabName = "security",
        h2("Security & Anomaly Detection (Isolation Forest)"),
        fluidRow(
          box(
            title = "Session Classification Results", status = "danger", solidHeader = TRUE, width = 6,
            plotOutput("anomalyBarPlot", height = 400)
          ),
          box(
            title = "Anomaly Score Distribution", status = "danger", solidHeader = TRUE, width = 6,
            plotOutput("anomalyDistPlot", height = 400),
            p("Extreme behaviors (automation, bot-like activity) are classified as Suspicious.")
          )
        )
      ),
      tabItem(tabName = "data",
        h2("Interactive Data Table"),
        fluidRow(
          box(width = 12,
            dataTableOutput("dataTable")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  ux_colors <- c("Positive UX" = "#2ecc71", "Neutral UX" = "#f1c40f", "Negative UX" = "#e74c3c")
  cluster_colors <- c("Good UX" = "#3498db", "Poor UX" = "#e67e22")
  sec_colors <- c("Normal session" = "#34495e", "Suspicious session" = "#c0392b")
  
  output$totalSessionsBox <- renderValueBox({
    valueBox(
      format(nrow(behavior_data), big.mark = ","), "Total Administered Sessions",
      icon = icon("users"), color = "blue"
    )
  })
  
  output$highFricBox <- renderValueBox({
    n_high <- sum(behavior_data$is_high_friction, na.rm=TRUE)
    pct <- round((n_high / nrow(behavior_data)) * 100, 1)
    valueBox(
      paste0(format(n_high, big.mark = ","), " (", pct, "%)"), "High Friction Users",
      icon = icon("exclamation-triangle"), color = "yellow"
    )
  })
  
  output$suspiciousBox <- renderValueBox({
    n_susp <- sum(behavior_data$suspicious == "Suspicious session", na.rm=TRUE)
    pct <- round((n_susp / nrow(behavior_data)) * 100, 1)
    valueBox(
      paste0(format(n_susp, big.mark = ","), " (", pct, "%)"), "Suspicious Sessions",
      icon = icon("shield-alt"), color = "red"
    )
  })
  
  output$sentimentPlot <- renderPlot({
    sentiment_counts <- behavior_data %>% 
      count(sentiment) %>%
      mutate(sentiment = factor(sentiment, levels = c("Negative UX", "Neutral UX", "Positive UX")))
    ggplot(sentiment_counts, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_manual(values = ux_colors) + theme_minimal(base_size = 14) +
      labs(x = NULL, y = "Number of Sessions") + theme(legend.position = "none")
  })
  
  output$confusingPagesPlotSmall <- renderPlot({
    top_confusing <- head(confusing_pages, 10)
    ggplot(top_confusing, aes(x = reorder(CategoryName, Visits), y = Visits)) +
      geom_bar(stat = "identity", fill = "#f39c12", width = 0.7) +
      coord_flip() + theme_minimal(base_size = 14) + labs(x = "Page Category", y = "Total Visits by High-Friction Users")
  })
  
  output$frictionDistPlot <- renderPlot({
    plot_data <- behavior_data
    if (nrow(plot_data) > 50000) plot_data <- sample_n(plot_data, 50000)
    ggplot(plot_data, aes(x = friction_score)) +
      geom_histogram(bins = 100, fill = "#3498db", color = "white") +
      geom_vline(aes(xintercept = quantile(friction_score, 0.95, na.rm=T)), color = "red", linetype = "dashed", size = 1) +
      theme_minimal(base_size = 15) + labs(x = "UX Friction Score", y = "Frequency", title = "Distribution of UX Friction (Red line: 95th Percentile Threshold)")
  })
  
  output$confusingPagesPlotLarge <- renderPlot({
    ggplot(confusing_pages, aes(x = reorder(CategoryName, Visits), y = Visits)) +
      geom_bar(stat = "identity", fill = "#e67e22", width = 0.7) +
      coord_flip() + theme_minimal(base_size = 15) + labs(x = "Page Category", y = "Total Visits among High-Friction Users")
  })
  
  output$clusterPlot <- renderPlot({
    plot_data <- behavior_data
    if (nrow(plot_data) > 10000) plot_data <- sample_n(plot_data, 10000)
    ggplot(plot_data, aes(x = session_duration, y = action_repetition_rate, color = cluster_label)) +
      geom_point(alpha = 0.6, size = 2) + scale_color_manual(values = cluster_colors) +
      theme_minimal(base_size = 15) + labs(x = "Session Duration", y = "Action Repetition Rate", color = "Behavioral Cluster") +
      scale_x_log10() + scale_y_continuous(trans="pseudo_log")
  })
  
  output$anomalyBarPlot <- renderPlot({
    sec_counts <- behavior_data %>% count(suspicious)
    ggplot(sec_counts, aes(x = suspicious, y = n, fill = suspicious)) +
      geom_bar(stat = "identity", width = 0.5) + scale_fill_manual(values = sec_colors) +
      theme_minimal(base_size = 15) + labs(x = NULL, y = "Count") + theme(legend.position = "none") +
      geom_text(aes(label = format(n, big.mark=",")), vjust = -0.5, size = 5)
  })
  
  output$anomalyDistPlot <- renderPlot({
    plot_data <- behavior_data
    if (nrow(plot_data) > 50000) plot_data <- sample_n(plot_data, 50000)
    ggplot(plot_data, aes(x = anomaly_score)) +
      geom_histogram(bins = 50, fill = "#95a5a6", color = "white") +
      geom_vline(aes(xintercept = quantile(anomaly_score, 0.98, na.rm=T)), color = "#c0392b", linetype = "dashed", size = 1) +
      theme_minimal(base_size = 15) + labs(x = "Isolation Forest Anomaly Score", y = "Frequency", title = "(Red line: 98th Percentile Threshold)")
  })
  
  output$dataTable <- renderDataTable({
    display_data <- behavior_data %>%
      select(user_id, session_duration, navigation_depth, friction_score, sentiment, cluster_label, suspicious, pages_seq) %>% head(2000)
    datatable(display_data, options = list(pageLength = 15, scrollX = TRUE)) %>% formatRound("friction_score", 2)
  })
}

shinyApp(ui = ui, server = server)
