# load libraries
library(plyr); library(dplyr)
library(ggplot2)
library(ggmap)
library(sp)
library(grid)
library(geosphere)

# get data
# source: https://www.kaggle.com/pronto/cycle-share-dataset
station_data <- read.csv('~/Downloads/cycle-share-dataset/station.csv',
                         header = TRUE, stringsAsFactors=F)
trip_data <- read.csv('~/Downloads/cycle-share-dataset/trip.csv', 
                      header = TRUE, stringsAsFactors=F)
weather_data <- read.csv('~/Downloads/cycle-share-dataset/weather.csv', 
                         header = TRUE, stringsAsFactors=F)


# get long & lat for start station
trip_station_data <- merge(station_data[c('long', 'lat', 'station_id')], 
                           trip_data[c('trip_id', 'starttime', 'from_station_id', 'to_station_id')], 
                           by.x = 'station_id', by.y = 'from_station_id')
# set col names
colnames(trip_station_data) <- c('from_station_id', 'from_long', 'from_lat', 
                                 'trip_id', 'stattime', 'to_station_id')

# get long & lat for end station
trip_station_data <- merge(trip_station_data, 
                           station_data[c('long', 'lat', 'station_id')], 
                           by.x = 'to_station_id', by.y = 'station_id')
# set col names
colnames(trip_station_data) <- c('to_station_id', 'from_station_id', 'from_long', 
                                 'from_lat', 'trip_id', 'stattime', 'to_long', 'to_lat')
head(trip_station_data)

# top n routes
n = 1500
grp_cols <- c('from_station_id', 'to_station_id')
# convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)
# frequency counts
trip_count <- trip_station_data %>%
  group_by_(.dots=dots) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
# get n top routes
top_routes <- head(trip_count, n)
# get top trips
top_trips <- merge(trip_station_data, top_routes, by = c('from_station_id', 'to_station_id'))

# get intermediate points between the two stations
edges <- gcIntermediate(top_trips[c('from_long', 'from_lat')],
                       top_trips[c('to_long', 'to_lat')],
                       n = 10,
                       addStartEnd = TRUE, 
                       sp = TRUE)
# http://docs.ggplot2.org/0.9.3.1/fortify.map.html
edges_fortified <- ldply(edges@lines, fortify)

# finally, we can plot
# source for the theme_map for ggplot2
source("https://dl.dropboxusercontent.com/u/2364714/theme_map.R")

plot <- ggplot() +
          geom_line(aes(long, lat, group = group), data = edges_fortified, alpha=0.011, 
                    size = 0.5, colour = "darkseagreen1") +
          coord_cartesian(ylim = c(47.595, 47.67), xlim = c(-122.355, -122.28)) + 
          geom_point(aes(from_long, from_lat), data = top_trips, alpha = 0.8, 
                     size = 0.4, colour = "white") +
          geom_point(aes(to_long, to_lat), data = top_trips, alpha = 0.8, 
                     size = 0.4, colour = "white") +
          theme_map()
plot
