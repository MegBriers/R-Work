# it was showing me an animated graph and now it isn't






# load twitter libraries
library(rjson)
library(jsonlite)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
library(tidyr)

# animated maps
library(gganimate)
library(leaflet)
library(lubridate)
library(maps)
library(ggthemes)

options(stringsAsFactors = FALSE)

# create file path
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
                         tweet_text = boulder_flood_tweets$text,
                         coords = boulder_flood_tweets$coordinates)
head(tweet_data)


# format = "%Y-%m-%d %H:%M:%s"
# flood start date sept 13 - 24 (end of incident)
start_date <- as.POSIXct('2013-09-13 00:00:00')
end_date <- as.POSIXct('2013-09-24 00:00:00')

# cleanup & and filter to just the time period around the flood
flood_tweets <- tweet_data %>%
  mutate(coords.coordinates = gsub("\\)|c\\(", "", coords.coordinates),
         date_time = as.POSIXct(date_time, format = "%a %b %d %H:%M:%S +0000 %Y")) %>%
  separate(coords.coordinates, c("long", "lat"), sep = ", ") %>%
  mutate_at(c("lat", "long"), as.numeric) %>%
  filter(date_time >= start_date & date_time <= end_date )
  
# create basemap of the globe
  world_basemap <- ggplot() +
    borders("world",colour = "gray85", fill = "gray80") +
    theme_map()
world_basemap  
  
# removing na values
tweet_locations <- flood_tweets %>% 
  na.omit()
head(tweet_locations)
 world_basemap +
  geom_point(data = tweet_locations, aes(x = long, y = lat),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "Tweet Locations During the Boulder Flood Event")

 
 # plot points on top of a leaflet basemap
 site_locations <- leaflet(tweet_locations) %>%
   addProviderTiles("CartoDB.Positron") %>%
   addCircleMarkers(lng = ~long, lat = ~lat, popup = ~tweet_text,
                    radius = 3, stroke = FALSE)
site_locations
 

# summarize data by day
# perhaps round the lat long and then do it?
# since it's all in sept


tweet_locations_grp <- tweet_locations %>%
  mutate(day = day(date_time),
         long_round = round(long,2),
         lat_round = round(lat,2)) %>%
  group_by(day, long_round, lat_round) %>%
  summarise(total_count = n())

# this also works -- plotting across the world here...
grouped_tweet_map <- world_basemap + geom_point(data = tweet_locations_grp,
                                                aes(long_round, lat_round, frame = day, size = total_count),
                                                    color = "purple", alpha = .5) + coord_fixed() +
                                                  labs(title = "Twitter Activity during the 2013 Colorado Floods")
grouped_tweet_map
# https://github.com/thomasp85/gganimate/releases/tag/v0.1.1
devtools::install_github("https://github.com/thomasp85/gganimate/releases/tag/v0.1.1")
library(gganimate)
library(magick)
library(gapminder)
gganimate(grouped_tweet_map)

# create animated maps
gg_animate_save(grouped_tweet_map,
                filename = "data/week-13/flood_tweets.gif",
                fps =  1, loop = 0,
                width = 1280,
                height = 1024)

anim_save(gganimate(grouped_tweet_map))
