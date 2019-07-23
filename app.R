
library(jsonlite)
library(ggplot2)
library(lubridate)
setwd("~/ladeplan/google_maps")

system.time(x <- fromJSON("mydata.json"))

# extracting the locations dataframe
loc <- x$locations

# converting time column from posix milliseconds into a readable time scale
loc$time <- as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")

# converting longitude and latitude from E7 to GPS coordinates
loc$lat <- loc$latitudeE7 / 1e7
loc$lon <- loc$longitudeE7 / 1e7

##########################################3

library(maps)
library(tidyverse)
loc$country <- map.where(database="world",loc$lon, loc$lat)
#cleaning NA
loc <- loc %>% filter(!is.na(country))

#cleaning ATTENTION
badvalues<-loc %>% group_by(country) %>% summarize(frequency = n_distinct(time)) %>% filter(frequency<50)
loc<-loc %>% filter(!country %in% badvalues$country)

#print remaining
loc %>% group_by(country) %>% summarize(frequency = n_distinct(time)) %>% arrange(frequency)



plot(loc %>% group_by(week=floor_date(time, "7 days")) %>% summarize(countries = n_distinct(country)))

a<- loc %>% group_by(day=floor_date(time,unit="day")) %>% distinct(country) %>% arrange(country) %>% ggplot(aes(country))
a + geom_bar()


library(geosphere)
home_lon<- median(loc$lon)
home_lat<- median(loc$lat)
#calculate distance from home (median position)
loc <- loc %>% mutate(distance_from_home = pmap_dbl(., ~ distm(x=c(home_lon,home_lat),y=c(..12,..11))))


loc <- loc %>% mutate(home = ifelse(distance_from_home < 5000,TRUE,FALSE))

#calendar graph
xts_heatmap <- function(x){
  #data.frame(Date=as.Date(index(x)), x[,1]) %>%
  data.frame(Date=x$day,x$value) %>%
    setNames(c("Date","Value")) %>%
    dplyr::mutate(
      Year=lubridate::year(Date),
      Month=lubridate::month(Date),
      # I use factors here to get plot ordering in the right order
      # without worrying about locale
      MonthTag=factor(Month,levels=as.character(1:12),
                      labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE),
      # week start on Monday in my world
      Wday=lubridate::wday(Date,week_start=1),
      # the rev reverse here is just for the plotting order
      WdayTag=factor(Wday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE),
      Week=as.numeric(format(Date,"%W"))
    ) %>%
    # ok here we group by year and month and then calculate the week of the month 
    # we are currently in
    dplyr::group_by(Year,Month) %>% 
    dplyr::mutate(Wmonth=1+Week-min(Week)) %>% 
    dplyr::ungroup() %>% 
    ggplot(aes(x=Wmonth, y=WdayTag, fill = Value)) + 
    geom_tile(colour = "white") + 
    facet_grid(Year~MonthTag) + 
    scale_fill_gradient(low="green", high="white") +
    labs(x="Week of Month", y=NULL)
}

#mark days when you stayed at home
graph <- loc %>% group_by(day = ymd(floor_date(time,"1 day"))) %>% summarize (value = tanh(sum(!home)))
xts_heatmap(graph)


#mark days away from home
graph2 <- loc %>% group_by(day = ymd(floor_date(time,"1 day"))) %>% summarize (value = tanh(sum(home)))
xts_heatmap(graph2)

#days further away from home as defined in range
range <- 200 * 1000 #in meters
loc <- loc %>% mutate(inrange = ifelse(distance_from_home < range,TRUE,FALSE))
graph3 <- loc %>% group_by(day = ymd(floor_date(time,"1 day"))) %>% summarize (value = tanh(sum(!inrange)))
xts_heatmap(graph3)

#largest distance between two points per day  
year <- loc %>% group_by(day = ymd(floor_date(time,"1 day"))) %>% filter(day >= as.POSIXct('2018-01-01 0:00:00') & day < as.POSIXct('2019-01-01 0:00:00')) %>% summarize(max(distm(cbind(lon,lat)))/1000)
year$value <- tanh(year$`max(distm(cbind(lon, lat)))/1000`/200) #200 KM range
xts_heatmap(year)


##########################################3

loc3 <- with(loc, subset(loc, loc$time > as.POSIXct('2019-01-01 0:00:01')))
loc3 <- with(loc, subset(loc3, loc$time < as.POSIXct('2019-12-22 23:59:59')))

activities <- loc3$activity

list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
activities  <- activities[list.condition]

df <- do.call("rbind", activities)
main_activity <- sapply(df$activity, function(x) x[[1]][1][[1]][1])

activities_2 <- data.frame(main_activity = main_activity, 
                           time = as.POSIXct(as.numeric(df$timestampMs)/1000, origin = "1970-01-01"))

head(activities_2)


ggplot(activities_2, aes(x = main_activity, group = main_activity, fill = main_activity)) + 
  geom_bar()  + 
  guides(fill = FALSE) +
  labs(
    x = "",
    y = "Count",
    title = "Main activities in 2016",
    caption = "Associated activity for recorded positions in 2016. 
    Because Google records activity probabilities for each position, 
    only the activity with highest likelihood were chosen for each position."
  )
