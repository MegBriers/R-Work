# lesson 4 - tidyverse and twitter API

# json support
library(rjson)
library(jsonlite)

# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
library(tidyr)

# text mining library
library(tidytext)
library(tm)

# plotting packages
library(igraph)
library(ggraph)

options(stringsAsFactors = FALSE)

# creating the file path
json_file <- "/Users/Meg/Downloads/boulder_flood_geolocated_tweets.json"

# importing json file line by line to avoid syntax errors
boulder_flood_tweets <- stream_in(file(json_file))

# need to create a new dataframe that contains:
# - the date
# - the twitter handle
# - the tweet text

# view the first 6 usernames
head(boulder_flood_tweets$user$screen_name)

# create new df with the tweet text & usernames
tweet_data <- data.frame(date_time = boulder_flood_tweets$created_at,
                         username = boulder_flood_tweets$user$screen_name,
                         tweet_text = boulder_flood_tweets$text)
head(tweet_data)


# format = "%Y-%m-%d %H:%M:%s"
# flood start date sept 13 - 24 (end of incident)
start_date <- as.POSIXct('2013-09-13 00:00:00')
end_date <- as.POSIXct('2013-09-24 00:00:00')

# cleanup

# flood tweets will only contain the data from the tweets in the required time period 
flood_tweets <- tweet_data %>%
  mutate(date_time = as.POSIXct(date_time, format = "%a %b %d %H:%M:%S +0000 %Y"))%>%
  filter(date_time >= start_date & date_time <= end_date)


min(flood_tweets$date_time)
max(flood_tweets$date_time)

options(stringsAsFactors = FALSE)
# exploring common words
# get a list of words
flood_tweet_messages <- flood_tweets %>%
  dplyr::select(tweet_text) %>%
  unnest_tokens(word, tweet_text)


head(flood_tweet_messages)

# plot the top 15 words
flood_tweet_messages %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col()+
  xlab(NULL)+
  coord_flip()+
    labs(x="Count",
    y = "Unique word",
    title = "Count of unique words found in tweets")


# removing stop words
data("stop_words")
# how many words do you have including the stop words
nrow(flood_tweet_messages)

flood_tweet_clean <- flood_tweet_messages %>% 
  anti_join(stop_words) %>%
  filter(!word == "rt")

# how many words after removing the stop words
nrow(flood_tweet_clean)


# plotting the data again
# plot the top 15 words -- notice any issues?
flood_tweet_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

# removing all of those pesky urls

# cleanup
flood_tweet_clean <- tweet_data %>%
  mutate(date_time = as.POSIXct(date_time, format = "%a %b %d %H:%M:%S +0000 %Y"),
         tweet_text =  gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                            "", tweet_text)) %>% 
  filter(date_time >= start_date & date_time <= end_date ) %>% 
  dplyr::select(tweet_text) %>%
  unnest_tokens(word, tweet_text) %>% 
  anti_join(stop_words) %>%
  filter(!word == "rt") # remove all rows that contain "rt" or retweet


# plotting the top 15 words
flood_tweet_clean %>%
  count(word, sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x=word, y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip() +
    labs(x="Count",
         y= "Unique words",
         title= "Count of unique words found in tweets,")


# paired word analysis
flood_tweets_paired <- flood_tweets %>%
  dplyr::select(tweet_text) %>%
  mutate(tweet_text = removeWords(tweet_text, stop_words$word)) %>%
  mutate(tweet_text = gsub("\\brt\\b|\\bRT\\b", "", tweet_text)) %>%
  mutate(tweet_text = gsub("http://*", "", tweet_text)) %>%
  unnest_tokens(paired_words, tweet_text, token = "ngrams", n = 2)

flood_tweets_paired %>%
  count(paired_words, sort = TRUE)
        


# seperating the words into columns and counting the unique combinations of words
flood_tweets_separated <- flood_tweets_paired %>%
  separate(paired_words, c("word1","word2"), sep = " ")

# new bigram counts
flood_word_counts <- flood_tweets_separated %>%
  count(word1, word2, sort = TRUE)
flood_word_counts

# plot the colorado flood word network
flood_word_counts %>%
  filter(n >= 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_diagonal(aes(edge_width = n)) +
  # alternatives
  geom_edge_density() +
  #geom_edge_fan(aes(edge_width = n)) +
# does not work  
#  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets during the 2013 Colorado Flood Event",
       subtitle = "September 2013 - Text mining twitter data ",
       x = "", y = "") +
  theme_void()



