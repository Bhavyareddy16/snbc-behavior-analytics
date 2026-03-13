###########################################################
# PROJECT: Intelligent Customer Behavior Analytics
# PART 1: UX Optimization Analysis
###########################################################

##############################
# 1. Load Required Libraries
##############################

library(dplyr)
library(tidyr)
library(ggplot2)

##############################
# 2. Load Dataset
##############################

# Set working directory where dataset is stored
# Using the project data directory instead of absolute path for portability
# setwd("~/Downloads/msnbc+com+anonymous+web+data")

# Read compressed dataset
raw_lines <- readLines(gzfile("data/msnbc990928.seq.gz"))

# Check number of lines
length(raw_lines)

###########################################################
# 3. Remove Metadata / Non-Data Lines
###########################################################

# Keep only numeric session sequences
raw_lines <- raw_lines[grep("^[0-9]", raw_lines)]

# Check cleaned dataset size
length(raw_lines)

###########################################################
# 4. Create Session Dataframe
###########################################################

sessions <- data.frame(
  user_id = 1:length(raw_lines),
  sequence = raw_lines
)

# View first rows
head(sessions)

###########################################################
# 5. Split Navigation Sequences
###########################################################

sessions$pages <- strsplit(sessions$sequence, " ")

# View structure
head(sessions$pages)

###########################################################
# 6. Feature Engineering
###########################################################

# 6.1 Session Duration (number of page visits)

sessions$session_duration <- sapply(
  sessions$pages,
  length
)

# 6.2 Click Frequency
# In this dataset clicks = page visits

sessions$click_frequency <- sessions$session_duration


# 6.3 Navigation Depth (unique pages)

sessions$navigation_depth <- sapply(
  sessions$pages,
  function(x) length(unique(x))
)


# 6.4 Action Repetition Rate
# Measures repeated navigation

sessions$action_repetition_rate <-
  sessions$session_duration -
  sessions$navigation_depth
sessions$action_repetition_rate

# 6.5 Login Time Deviation
# Deviation from average session duration

mean_duration <- mean(sessions$session_duration)
mean_duration

sessions$login_time_deviation <-
  abs(sessions$session_duration - mean_duration)
sessions$login_time_deviation

###########################################################
# 7. Create UX Feature Dataset
###########################################################

ux_features <- sessions %>%
  select(
    user_id,
    session_duration,
    click_frequency,
    navigation_depth,
    action_repetition_rate,
    login_time_deviation
  )

head(ux_features)

###########################################################
# 8. Compute UX Friction Score
###########################################################

# Normalize behavioral features

scaled_features <- scale(
  ux_features %>%
    select(
      session_duration,
      navigation_depth,
      action_repetition_rate,
      login_time_deviation
    )
)

# Combine normalized features

ux_features$ux_friction_score <- rowSums(scaled_features)

head(ux_features)

###########################################################
# 9. Visualize UX Friction Distribution
###########################################################

ggplot(ux_features, aes(x = ux_friction_score)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  labs(
    title = "UX Friction Score Distribution",
    x = "UX Friction Score",
    y = "Number of Sessions"
  )

###########################################################
# 10. Identify High-Friction Users
###########################################################

# Calculate 95th percentile

threshold_95 <- quantile(
  ux_features$ux_friction_score,
  0.95
)

# Extract high-friction sessions

high_friction_users <- ux_features %>%
  filter(ux_friction_score >= threshold_95)

# Number of users with severe difficulty

nrow(high_friction_users)

head(high_friction_users)

###########################################################
# 11. Page Confusion Analysis
###########################################################

# Convert pages into long format

page_visits <- sessions %>%
  select(user_id, pages) %>%
  unnest(pages)

# Count page visits

page_counts <- page_visits %>%
  group_by(pages) %>%
  summarise(
    visit_count = n(),
    unique_users = n_distinct(user_id)
  ) %>%
  arrange(desc(visit_count))

head(page_counts)

###########################################################
# 12. Identify Most Confusing Pages
###########################################################

confusing_pages <- page_visits %>%
  filter(user_id %in% high_friction_users$user_id) %>%
  group_by(pages) %>%
  summarise(confusion_count = n()) %>%
  arrange(desc(confusion_count))

head(confusing_pages)

###########################################################
# 13. User Segmentation Using Clustering
###########################################################

cluster_data <- ux_features %>%
  select(
    session_duration,
    navigation_depth,
    action_repetition_rate,
    login_time_deviation
  )

# Normalize data

cluster_data_scaled <- scale(cluster_data)

# Apply K-Means clustering

set.seed(123)

kmeans_result <- kmeans(
  cluster_data_scaled,
  centers = 2
)

# Assign cluster labels

ux_features$cluster <- kmeans_result$cluster

###########################################################
# 14. Cluster Summary
###########################################################

table(ux_features$cluster)

aggregate(
  cluster_data,
  by = list(Cluster = ux_features$cluster),
  mean
)

###########################################################
# 15. Cluster Visualization
###########################################################

ggplot(ux_features,
       aes(
         x = session_duration,
         y = action_repetition_rate,
         color = factor(cluster)
       )) +
  geom_point(alpha = 0.3) +
  labs(
    title = "User Behavior Clusters",
    x = "Session Duration",
    y = "Action Repetition Rate",
    color = "Cluster"
  )

#Security Risk Analysis
#Step 1: Generate Synthetic Timestamps
library(dplyr)
library(lubridate)

set.seed(123)

# Generate random start time for each session
sessions$session_start <- as.POSIXct("2024-01-01 00:00:00") +
  runif(nrow(sessions), 0, 86400)

# Create timestamps for each page visit
sessions$page_timestamps <- lapply(
  sessions$pages,
  function(p) {
    cumsum(sample(1:10, length(p), replace = TRUE))
  }
)
# install.packages("lubridate") # Assumed installed from standard data science environment

#Step 2: Compute Security Behavior Features

#Now we derive time-based security features.
sessions$avg_click_interval <- sapply(
  sessions$page_timestamps,
  function(x) mean(diff(c(0, x)))
)

sessions$session_time_span <- sapply(
  sessions$page_timestamps,
  max
)

sessions$click_speed <- sessions$session_duration /
  (sessions$session_time_span + 1)

security_features <- sessions %>%
  select(
    session_duration,
    click_frequency,
    navigation_depth,
    action_repetition_rate,
    login_time_deviation,
    avg_click_interval,
    session_time_span,
    click_speed
  )
library(isotree)

iso_model <- isolation.forest(
  as.matrix(security_features),
  ntrees = 100
)

scores <- predict(iso_model, as.matrix(security_features))

sessions$security_score <- scores

threshold <- quantile(scores, 0.98)

sessions$security_risk <- ifelse(
  scores >= threshold,
  "Suspicious",
  "Normal"
)
library(ggplot2)

ggplot(sessions, aes(x = security_risk)) +
  geom_bar(fill = "red") +
  labs(
    title = "Security Risk Detection",
    x = "Session Type",
    y = "Number of Sessions"
  )

# Add behavioral_sentiment before referencing it in the table
# Map friction_score to behavioral sentiment
ux_features$behavioral_sentiment <- ifelse(
  ux_features$ux_friction_score > quantile(ux_features$ux_friction_score, 0.8), "Negative UX",
  ifelse(ux_features$ux_friction_score < quantile(ux_features$ux_friction_score, 0.4), "Positive UX", "Neutral UX")
)

table(
  ux_features$behavioral_sentiment,
  sessions$security_risk
)
