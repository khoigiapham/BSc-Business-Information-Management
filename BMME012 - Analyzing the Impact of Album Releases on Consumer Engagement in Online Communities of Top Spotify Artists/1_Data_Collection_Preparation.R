# This script collects data and prepares it to be used for analysis

#Load require library
library(spotifyr)
library(dplyr)
library(tidyverse)
library(RedditExtractoR)
library(readxl)
library(tidyverse)
library(tidytext)
library(readr)
library(syuzhet)
library(xlsx)

#Set working directory
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
#Get access to Spotify public data
Sys.setenv(SPOTIFY_CLIENT_ID = "9ed8f73ef63c4f6aaf199ac48609a099")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "bec2d98c460146e897edb4831e6b8592")
access_token <- get_spotify_access_token()

## 1. Prepare the 1st dataset "text", which is used for sentiment analysis----
# Prepare an excel file that have the list of top 50 most followed artists
d.artists <- read.csv("artists.csv")
top_50 <- d.artists %>% 
  arrange(desc(followers)) %>% 
  slice(1:50) %>%
  subset(select = "name")
#write.xlsx(top_50, "top_50.xlsx", sheetName = "Sheet1")

# After we get the list, we manually searcged and fill in the community name
# of the the respective artists in that excel file and load it back. 
community <- read_excel("top_50_community.xlsx")
## Use (RedditExtractoR) package to get data from reddit
# Create an empty list to store tables
threads_list <- list()
# Iterate through each row of the 'community' dataframe. 
# This will take some time.
for (i in 1:nrow(community)) {
  # Check if the 'community' value is NA.
  if (is.na(community$community[i])) {
    # Call the 'find_thread_urls' function using the 'name' value and subreddit 'Music'.
    current_table <- find_thread_urls(keywords = community$name[i], sort_by = "new", subreddit = "Music", period = "all")
  } else {
    # Call the 'find_thread_urls' function using the current 'community' value.
    current_table <- find_thread_urls(subreddit = community$community[i], sort_by = "new", period = "all")
  }
  # Add an additional column to the current_table with the artist's name.
  current_table$artist <- community$name[i]
    # Store the table in the 'threads_list' list with the name 'artist_i'.
  threads_list[[paste0("artist_", i)]] <- current_table
}
# Bind the rows from all the tables in thread_list
threads <- do.call("rbind", threads_list)
# Get contents from each threads,the cotent includes threads and comments
# this will take very long ~ 4000 minutes,
# please go to line 72 and load data, noted that I saved as RDATA object.
contents_list <- get_thread_content(threads$url)
## save(contents_list, file = "contents_list.RData")
# Load the saved data
contents_list <-load("contents_list.RData")

# I also saved the 2 tables from this list 
#Get the table threads and comments from the list
##final_threads <- contents_list$threads
##final_comments <- contents_list$comments
##saveRDS(final_threads,"final_threads.RDS")
##saveRDS(final_comments,"final_comments.RDS")
final_threads <- readRDS("final_threads.RDS")
final_comments <- readRDS("final_comments.RDS")
# Aggregate title and text column of threads to column self_text
final_threads <- final_threads %>% 
  unite("self_text", c("title", "text"), sep = " ")
# Change the column 'comment' in comment table to self_text
colnames(final_comments)[9] <- "self_text"
# Select columns of interest from the two tables
final_threads <- final_threads %>%
  select(url, author, date, self_text, subreddit)
final_comments <- final_comments %>%
  select(url, author, date, self_text)
# Perform a left join to add the subreddit column to final_comments
final_comments <- final_comments %>%
  left_join(final_threads %>% select(url, subreddit), by = "url",relationship = "many-to-many")
# Combine the rows of both datasets
self_text <- bind_rows(final_threads, final_comments)
# Remove duplicates
self_text <- distinct(self_text)


## 1b. Sentiment analysis----
# We think it make sense to do the sentiment analysis here because we will merge the sentiment
# column with the album data set, to have a complete final data
# Cleaning the text in the self_text 
self_text$self_text <- gsub("[^[:alpha:][:space:]]", "", self_text$self_text) # removes everything except letters and spaces
self_text$self_text <- tolower(trimws(self_text$self_text)) # Convert to lowercase and trim whitespace
self_text$self_text <- removeWords(self_text$self_text, stopwords("english")) #Remove stopword
self_text$self_text <- stemDocument(self_text$self_text) #Stem word
# Sentiment analysis
# Get sentiment of text, this will take very long, please go to 103 and load the data
#sentiment <- get_nrc_sentiment(self_text$self_text)
##saveRDS(sentiment, "sentiment.RDS")
sentiment <- readRDS("sentiment.RDS")
self_text$sentiment <- sentiment$positive - sentiment$negative
self_text_final <- self_text
saveRDS(self_text_final, "self_text_final.rds")

## 2. Prepare the second dataset "album_release" ----
#Get the artists id of top 50 artist
top_50 <- d.artists %>% 
  arrange(desc(followers)) %>% 
  slice(1:50) %>%
  subset(select = c("name","id"))
# Loop through all artist and get audio features, this will take long time so,
# please go to line 130 and load data
# Create and empty dataframe before a loop
top_50_album <- data.frame()
# Loop through the top 50 artist to get their audio features
for (i in 1:length(top_50$id)) {
    try({
    current <- get_artist_audio_features(artist = top_50$id[i], include_groups = "album")
    top_50_album <- rbind(top_50_album, current)
    print(i)
    flush.console()
  })
}
# Save the data
##saveRDS(top_50_album, "top_50_album.rds")
# Load data
top_50_album <- readRDS("top_50_album.rds")
#Select column of interest
top_50_album <- top_50_album %>% 
  subset(select = c("artist_name","artist_id","album_id","album_name",
                    "album_release_date", "available_markets",
                    "danceability", "energy", "key", "loudness",
                    "mode", "speechiness", "acousticness", 
                    "instrumentalness","liveness", "valence", "tempo"))
# Create a new column with the number of available markets in each row
top_50_album$num_markets <- sapply(top_50_album$available_markets, length)
# Select only the albums with more than 100 available market:
top_50_album <- top_50_album[top_50_album$num_markets > 100, ]
# Delete available market column
top_50_album$available_markets <- NULL
# Group by artist_id and album_id and get the mean value of each sonic
# characteristic for each album, and keep the same columns as before
top_50_album_grouped <- top_50_album %>%
  group_by(artist_id, album_id) %>%
  summarize(across(c(danceability, energy, key, loudness, mode, 
                     speechiness, acousticness, instrumentalness, 
                     liveness, valence, tempo), mean), 
            album_name = first(album_name),
            album_release_date = first(album_release_date),
            num_markets = first(num_markets),
            artist_name = first(artist_name)) %>%
  select(artist_name, artist_id, album_id, album_name, album_release_date, 
         danceability, energy, key, loudness, mode, speechiness,
         acousticness, instrumentalness, liveness, valence, tempo, num_markets)
# For some albums although they have different album_id, they have the same
# name. So we left out duplicated albums_name, and select the oldest name
# version, which logically the "original" version.
top_50_album_grouped <- top_50_album_grouped %>%
  arrange(album_release_date) %>%
  distinct(album_name, .keep_all = TRUE)
# For each of the artist select at most the 10 most recent album
top_50_album_recent <- top_50_album_grouped %>%
  arrange(desc(album_release_date)) %>%
  group_by(artist_id) %>%
  slice_head(n = 10) %>%
  ungroup()
# Save the data
# saveRDS(top_50_album_recent, "top_50_album_recent.rds")


# 3. Prepare the final data for analysis----
# Load data
top_50_album_recent <- readRDS("top_50_album_recent.rds")
# Subset album_id, release date and artist name
album_release <- as.data.frame(top_50_album_recent) %>%
  subset(select = c("album_id","album_release_date","artist_name"))
# get the relevant community
album_release <- album_release %>%
  left_join(community, by = c("artist_name" = "name"))
# artist without community change into "Music" community
album_release$community <- ifelse(album_release$community == "NA", 
                                  "Music", album_release$community)
# rename community to subreddit
album_release <- album_release %>%
  rename(subreddit = community)
##Get the sentiment before and after album_release----
# Convert date columns to Date format
album_release$album_release_date <- as.Date(album_release$album_release_date)
self_text_final$date <- as.Date(self_text_final$date)
# Function to get total sentiment within 90 days before album release date
get_total_sentiment <- function(subreddit_name, album_data, text_data) {
  # Filter data for the given subreddit
  album_subreddit <- album_data %>% filter(subreddit == subreddit_name)
  text_subreddit <- text_data %>% filter(subreddit == subreddit_name)
  # Initialize an empty dataframe to store results
  results <- data.frame()
  # Loop through each album
  for (i in 1:nrow(album_subreddit)) {
    album_row <- album_subreddit[i, ]
    release_date <- album_row$album_release_date
    # Filter self_text_final within 90 days before album release date
    text_filtered <- text_subreddit %>% filter(date >= (release_date - 90) & date < release_date)
    # Calculate total sentiment
    total_sentiment <- sum(text_filtered$sentiment)
    # Store the result
    result_row <- data.frame(
      album_id = album_row$album_id,
      album_release_date = album_row$album_release_date,
      artist_name = album_row$artist_name,
      subreddit = album_row$subreddit,
      total_sentiment = total_sentiment
    )
    # Append the result row to the results dataframe
    results <- rbind(results, result_row)
  }
  return(results)
}
# Calculate total sentiment for each subreddit
subreddits <- unique(album_release$subreddit)
results_list <- lapply(subreddits, get_total_sentiment, album_data = album_release, text_data = self_text_final)
results <- do.call(rbind, results_list)

# Function to get total sentiment within 90 days after album release date
get_total_sentiment_after_release <- function(subreddit_name, album_data, text_data) {
  # Filter data for the given subreddit
  album_subreddit <- album_data %>% filter(subreddit == subreddit_name)
  text_subreddit <- text_data %>% filter(subreddit == subreddit_name)
  # Initialize an empty dataframe to store results
  results <- data.frame()
  # Loop through each album
  for (i in 1:nrow(album_subreddit)) {
    album_row <- album_subreddit[i, ]
    release_date <- album_row$album_release_date
    # Filter self_text_final within 90 days after album release date
    text_filtered <- text_subreddit %>% filter(date > release_date & date <= (release_date + 90))
    # Calculate total sentiment
    total_sentiment <- sum(text_filtered$sentiment)
    # Store the result
    result_row <- data.frame(
      album_id = album_row$album_id,
      album_release_date = album_row$album_release_date,
      artist_name = album_row$artist_name,
      subreddit = album_row$subreddit,
      total_sentiment = total_sentiment
    )
    # Append the result row to the results dataframe
    results <- rbind(results, result_row)
  }
  return(results)
}
# Calculate total sentiment for each subreddit after the album release date
subreddits <- unique(album_release$subreddit)
results_list_after_release <- lapply(subreddits, get_total_sentiment_after_release, album_data = album_release, text_data = self_text_final)
results_after_release <- do.call(rbind, results_list_after_release)
#Aggregate the total sentiment results and rename the columns
results$sentiment_score_after <- results_after_release$total_sentiment
results <- results %>%
  rename(sentiment_score_before = total_sentiment)

# The release_effects column will have "positive"
# values if the difference between sentiment_score_after and 
#sentiment_score_before is greater than 0, and "negative" otherwise.
results$release_effects <- ifelse(results$sentiment_score_after - results$sentiment_score_before >= 0, "positive", "negative")
results$release_effects <- ifelse(results$score >= 0, "positive", "negative")
# Delete rows where results$sentiment_score_after and results$sentiment_score_bofore equals to 0
# Because this is very likely that we cannot get the reddit data for this album
results <- results[!(results$sentiment_score_after == 0 & results$sentiment_score_before == 0), ]
# Merge the dataframes on 'album_id'
df_final <- merge(top_50_album_recent, 
                   results[, c("album_id", "release_effects")], 
                   by = "album_id")
# Reorder column album_id
df_final <- df_final[, c(1, 4, 2, 3, 5:length(df_final))]
# Save final prepared dataset----
saveRDS(df_final,file = "data_final/data_prepared.RDS")
