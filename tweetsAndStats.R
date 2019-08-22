# load twitter library - the rtweet library is recommended now over twitterR
library(rtweet)
# plotting and pipes - tidyverse
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

# assigning the app name
appname <- "megs_first_app"

# api key
key <- "Sf50z8oDswAEahl2QzTVuAxc1"

# api secret key
secret <- "gBHwqKXswrzSNE5JLBoaV2eqfXZD98Lw9JZH6YAQwQQnWdfpEy"


# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

library(rtweet)

is_tweet_length <- function(.x, n = 280) {
  .x <- gsub("https?://[[:graph:]]+\\s?", "", .x)
  while (grepl("^@\\S+\\s+", .x)) {
    .x <- sub("^@\\S+\\s+", "", .x)
  }
  !(nchar(.x) <= n)   # here's the fix
}


assignInNamespace("is_tweet_length", is_tweet_length, ns = "rtweet")

# post a tweet from R
post_tweet("Look, I'm tweeting from R in my #rstats #earthanalytics class! @EarthLabCU")
## your tweet has been posted!

## SEARCH TWITTER FOR TWEETS

# search for 500 tweets using the #rstats hashtag
rstats_tweets <- search_tweets(q="#rstats", n=500)

head(rstats_tweets, n=3)

# find recent tweets with #rstats but ignore retweets
rstats_tweets <- search_tweets("#rstats", n=500, include_rts = FALSE)
head(rstats_tweets, n=2)

# view column with screen names - top 6
head(rstats_tweets$screen_name)

# get a list of unique usernames
unique(rstats_tweets$screen_name)


# what users are tweeting with #rstats
users <- search_users("#rstats", n=500)
# just view the first 2 users - the data frame is large
head(users, n=2)

# how many locations are represented
length(unique(users$location))

users %>%
  ggplot(aes(location))+
  geom_bar() + coord_flip()+
  labs(x = "Count", 
       y = "Location", 
       title = "Twitter users - unique locations "
       )

users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")


users %>% na.omit() %>%
  ggplot(aes(time_zone)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Time Zone",
       title = "Twitter users - unique time zones ")
