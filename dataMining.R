# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)

hades_tweets <- search_tweets(q="hadestown", n=10000,
                                lang = "en",
                                include_rts = FALSE)

head(hades_tweets$text)


# removing urls
hades_tweets$stripped_text <- gsub("http.*","", hades_tweets$text)
hades_tweets$stripped_text <- gsub("https.*","", hades_tweets$stripped_text)


# note the words that are recognized as unique by R
a_list_of_words <- c("Dog", "dog", "dog", "cat","cat",",")
unique(a_list_of_words)

if ("dplyr" %in% installed.packages()[, "Package"]){ 
  cat("'dplyr' is installed.")
} else {
  install.packages("dplyr",dependencies=T)
}
library(dplyr)

# remove punctuation, convert to lowercase, add id for each tweet!
hades_tweets_clean <- hades_tweets %>% 
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)



# plot the top 15 words -- notice any issues?
hades_tweets_clean %>%
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


#load list of stop words - from the tidytext package
data("stop_words")
# view first 6 words
head(stop_words)

nrow(hades_tweets_clean)

cleaned_tweets_words <- hades_tweets_clean %>% anti_join(stop_words)
nrow(cleaned_tweets_words)


# plot the top 15 words -- notice any issues?
cleaned_tweets_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")


library(devtools)
install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
hades_tweets_paired_words <- hades_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

hades_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
hades_tweets_separated_words <- hades_tweets_paired_words %>% separate(paired_words, c("word1","word2"), sep = " ")

hades_tweets_filtered <- hades_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
hades_tweets_counts <- hades_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(hades_tweets_counts)

library(igraph)
library(ggraph)

# plot climate change word network
hades_tweets_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - Hadestown",
       subtitle = "Text mining twitter data ",
       x = "", y = "")
