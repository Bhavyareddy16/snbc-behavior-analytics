library(dplyr)
library(tidyr)
library(cluster)
library(isotree)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

dataset_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/msnbc-mld/msnbc990928.seq.gz"
dataset_path <- "data/msnbc990928.seq.gz"

if (!file.exists(dataset_path)) {
  message("Downloading dataset...")
  download.file(dataset_url, destfile = dataset_path, mode = "wb")
}

message("Reading and cleaning data...")
raw_lines <- readLines(dataset_path)

first_data_idx <- which(grepl("^[0-9 ]+$", raw_lines))[1]
seq_lines <- raw_lines[first_data_idx:length(raw_lines)]
seq_lines <- seq_lines[seq_lines != ""]

message("Parsing sessions...")
sessions_list <- strsplit(trimws(seq_lines), "\\s+")

users_df <- data.frame(user_id = seq_along(sessions_list))
users_df$pages <- lapply(sessions_list, as.integer)

message("Engineering features...")
session_duration <- lengths(users_df$pages)
navigation_depth <- vapply(users_df$pages, function(x) length(unique(x)), integer(1))

features_df <- data.frame(
  user_id = users_df$user_id,
  session_duration = session_duration,
  click_frequency = session_duration,
  navigation_depth = navigation_depth,
  action_repetition_rate = session_duration - navigation_depth
)

features_df <- features_df %>%
  mutate(
    login_time_deviation = abs(session_duration - mean(session_duration, na.rm=TRUE))
  )
features_df$pages <- users_df$pages

message("Computing UX Friction Score...")
features_df <- features_df %>%
  mutate(
    z_duration = scale(session_duration)[,1],
    z_depth = scale(navigation_depth)[,1],
    z_repetition = scale(action_repetition_rate)[,1],
    z_deviation = scale(login_time_deviation)[,1],
    friction_score = z_duration + z_depth + z_repetition + z_deviation
  )

threshold <- quantile(features_df$friction_score, 0.95, na.rm=TRUE)

features_df <- features_df %>%
  mutate(
    is_high_friction = friction_score > threshold,
    sentiment = case_when(
      friction_score > quantile(friction_score, 0.80, na.rm=TRUE) ~ "Negative UX",
      friction_score < quantile(friction_score, 0.40, na.rm=TRUE) ~ "Positive UX",
      TRUE ~ "Neutral UX"
    )
  )

message("Performing Page Confusion Analysis...")
high_friction_users <- features_df %>% filter(is_high_friction)

confusing_pages <- unlist(high_friction_users$pages)
confusing_pages_counts <- as.data.frame(table(PageCategory = confusing_pages)) %>%
  rename(Visits = Freq) %>%
  mutate(PageCategory = as.integer(as.character(PageCategory)))

page_map <- c(
  "1" = "Front Page", "2" = "News", "3" = "Tech", "4" = "Local", 
  "5" = "Opinion", "6" = "On-Air", "7" = "Misc", "8" = "Weather", 
  "9" = "MSN News", "10" = "Health", "11" = "Living", "12" = "Business", 
  "13" = "MSN Sports", "14" = "Sports", "15" = "Summary", "16" = "BBS", "17" = "Travel"
)

confusing_pages_counts <- confusing_pages_counts %>%
  mutate(CategoryName = page_map[as.character(PageCategory)]) %>%
  arrange(desc(Visits))

# Changed to CSV for human-readable output
write.csv(confusing_pages_counts, "data/confusing_pages.csv", row.names = FALSE)

message("Performing User Segmentation...")
cluster_vars <- features_df %>%
  select(session_duration, navigation_depth, action_repetition_rate, login_time_deviation) %>%
  scale()

set.seed(42)
km_res <- kmeans(cluster_vars, centers = 2, iter.max = 30, nstart = 1)
features_df$cluster <- km_res$cluster

c1_friction <- mean(features_df$friction_score[features_df$cluster == 1])
c2_friction <- mean(features_df$friction_score[features_df$cluster == 2])

features_df$cluster_label <- ifelse(features_df$cluster == (if(c1_friction > c2_friction) 1 else 2), "Poor UX", "Good UX")

message("Fitting Isolation Forest model...")
iso_vars <- features_df %>%
  select(session_duration, click_frequency, navigation_depth, action_repetition_rate, login_time_deviation)

iso_model <- isolation.forest(iso_vars, ntrees = 100)
features_df$anomaly_score <- predict(iso_model, iso_vars)

anomaly_threshold <- quantile(features_df$anomaly_score, 0.98, na.rm=TRUE)
features_df <- features_df %>%
  mutate(
    suspicious = ifelse(anomaly_score > anomaly_threshold, "Suspicious session", "Normal session")
  )

message("Formatting final dataset...")
features_df$pages_seq <- vapply(features_df$pages, function(x) {
  if(length(x) > 20) {
    paste0(paste(head(x, 20), collapse=" "), " ...")
  } else {
    paste(x, collapse=" ")
  }
}, character(1))

# Drop temporary columns to keep CSV clean
features_df <- features_df %>% select(-pages, -z_duration, -z_depth, -z_repetition, -z_deviation)

message("Saving processed data...")
# Keep the full dataset as optimized binary so the Shiny app is fast and GitHub accepts it (< 100MB)
saveRDS(features_df, "data/behavior_data.rds")

# Export a random 10,000 row sample to CSV so the user can easily read/view the data structure!
message("Exporting CSV sample for viewing...")
sample_df <- features_df %>% sample_n(min(n(), 10000))
write.csv(sample_df, "data/behavior_data_sample.csv", row.names = FALSE)

message("Data prep complete!")
