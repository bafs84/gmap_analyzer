## gmap.analyzer

## Copyright 2019, Benjamin Sawicki, All rights reserved.

## CONCEPT

## The original idea to this project was the question 
## "What distance do I drive per day with my car?" 
## and the idea to look at my google timeline. 
## Google allows me to look at single days, 
## but since GDPR we have the right to takeout 
## our data and process it however we want.

## GOAL

## Provide a script that takes a google takeout dataset 
## as input and outputs the distance driven per day reliably.

## Required libraries

  # Check packages, install if necessary, and load 
    packages.installed <- rownames(installed.packages())
  	package.check <- function(pck.name) {
  	  if(!any(pck.name == packages.installed)) {
  	    install.packages(paste0(pck.name), repos="https://cloud.r-project.org")
  	    }
      }
    package.check("jsonlite")
    package.check("ggplot2")
    package.check("lubridate")
    package.check("maps")
    package.check("tidyverse")
    package.check("geosphere")
  
    library(jsonlite)
    library(ggplot2)
    library(lubridate)
    library(maps)
    library(tidyverse)
    library(geosphere)

## Load data
  system.time(x <- fromJSON("mydata.json"))

# Extract the locations dataframe
 loc <- x$locations
# Convert time column from posix milliseconds into a readable time scale.
  loc$time <- as.POSIXct(as.numeric(x$locations$timestampMs) / 1000, 
   origin = "1970-01-01")
# Convert longitude and latitude from E7 to GPS coordinates.
  loc$lat <- loc$latitudeE7 / 1e7
  loc$lon <- loc$longitudeE7 / 1e7
# Extract the locations dataframe
  loc$country <- map.where(database="world", loc$lon, loc$lat)
# Remove NAs.
  loc <- loc %>% filter(!is.na(country))
# Cleaning NA.
  badvalues <- loc %>% group_by(country) %>% 
    summarize(frequency = n_distinct(time)) %>% filter(frequency<50)
  loc <- loc %>% filter(!country %in% badvalues$country)
# Print remaining countries.
  loc %>% group_by(country) %>% summarize(frequency = n_distinct(time)) %>% 
    arrange(desc(frequency))
# Plot countries by date.
  plot(loc %>% group_by(week=floor_date(time, "7 days")) %>% 
    summarize(countries = n_distinct(country)))
# Plot countries by date with ggplot.
  plot.by.country <- loc %>% group_by(day = floor_date(time, unit = "day")) %>% 
    distinct(country) %>% arrange(country) %>% ggplot(aes(country))
  plot.by.country + geom_bar()
# Extract longitude and latitude data.
  home.lon <- median(loc$lon)
  home.lat <- median(loc$lat)
# Calculate distance from home (median position).
  loc <- loc %>% mutate(distance_from_home = 
    pmap_dbl(., ~ distm(x = c(home.lon, home.lat), y = c(..12, ..11))))
  loc <- loc %>% mutate(home = ifelse(distance_from_home < 5000, TRUE, FALSE))

# Calendar graph.
  xts.heatmap <- function(heatmap.input) {
    # Data.frame(Date=as.Date(index(heatmap.input)), x[,1]) %>% 
    ## MNP [Is this comment above needed?]
    data.frame(Date = heatmap.input$day, heatmap.input $value) %>% 
      setNames(c("Date", "Value")) %>% 
      dplyr::mutate(Year = lubridate::year(Date), 
        Month = lubridate::month(Date),
        # I use factors here to get plot ordering in the right order
        # without worrying about locale.
        MonthTag = factor(Month,levels = as.character(1:12),  
        labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
        "Oct", "Nov", "Dec"), ordered = TRUE),
        # The week starts on Monday in my world.
        Wday = lubridate::wday(Date,week_start = 1),
        # The 'rev' reverse here is just for the plotting order.
        WdayTag = factor(Wday,levels = rev(1:7), 
        labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
        ordered=TRUE), Week = as.numeric(format(Date, "%W"))
        ) %>%
      # Okay here we group by year and month 
      # and then calculate the week of the month we are currently in.
      dplyr::group_by(Year, Month) %>% 
      dplyr::mutate(Wmonth = 1 + Week - min(Week)) %>% 
      dplyr::ungroup() %>% 
      ggplot(aes(x = Wmonth, y = WdayTag, fill = Value)) + 
      geom_tile(colour = "white") + 
      facet_grid(Year ~ MonthTag) + 
      scale_fill_gradient(low = "green", high = "white") +
      labs(x = "Week of Month", y = NULL)
  }

# Mark the days when you stayed at home.
  graph.at.home <- loc %>% group_by(day = ymd(floor_date(time, "1 day"))) %>% 
    summarize (value = tanh(sum(!home)))
# Plot heatmap of days at home.  
  xts.heatmap(graph.at.home)

# Mark days away from home.
  graph.away.from.home <- loc %>% 
    group_by(day = ymd(floor_date(time, "1 day"))) %>% 
    summarize (value = tanh(sum(home)))
  xts.heatmap(graph.away.from.home)

# Days further away from home as defined in range.
  range.in.meters <- 200 * 1000 # In meters.
  loc <- loc %>% mutate(inrange = ifelse(distance_from_home < range.in.meters, 
    TRUE,FALSE))
  graph.away.from.home.range <- loc %>% 
    group_by(day = ymd(floor_date(time, "1 day"))) %>% 
    summarize (value = tanh(sum(!inrange)))
  xts.heatmap(graph.away.from.home.range)

# Largest distance between two points per day.
  year <- loc %>% group_by(day = ymd(floor_date(time, "1 day"))) %>% 
    filter(day >= as.POSIXct('2018-01-01 0:00:00') & day < 
    as.POSIXct('2019-01-01 0:00:00')) %>% 
    summarize(max(distm(cbind(lon, lat))) / 1000)
# 200 KM range.
  year$value <- tanh(year$`max(distm(cbind(lon, lat)))/1000` / 200) 
  xts.heatmap(year)

# Data from 2019
  loc3 <- with(loc, subset(loc, loc$time > as.POSIXct('2019-01-01 0:00:01')))
  loc3 <- with(loc, subset(loc3, loc$time < as.POSIXct('2019-12-22 23:59:59')))
  activities <- loc3$activity
# Remove null entries
  list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
  activities  <- activities[list.condition]
  df.activities <- do.call("rbind", activities)
  main_activity <- sapply(df.activities$activity, function(x) x[[1]][1][[1]][1])
  activities.2 <- data.frame(main_activity = main_activity, 
    time = as.POSIXct(as.numeric(df.activities$timestampMs) / 1000, 
    origin = "1970-01-01"))
  head(activities.2)

  ggplot(activities.2, aes(x = main_activity, group = main_activity, 
    fill = main_activity)) + geom_bar()  +  guides(fill = FALSE) +
    labs(x = "", y = "Count", title = "Main activities in 2016",
    caption = "Associated activity for recorded positions in 2016. 
      Because Google records activity probabilities for each position, 
      only the activity with highest likelihood were chosen for each position."
  )

## Functions computing distance in meters between two GPS coordinates.

  distance.2.points.EP <- function(lat1, lon1, lat2, lon2) {
    #convert radian
    theta1 <- lat1 * pi / 180
    phi1 <- lon1 * pi / 180
    theta2 <- lat2 * pi / 180
    phi2 <- lon2 * pi / 180
    dtheta <- theta2 - theta1
    dphi <- phi2 - phi1
    # compute angle.
    t.angle <- sin(dtheta) ^ 2 + sin(dphi) ^ 2 * cos(theta1) ^ 2 * 
      cos(theta2) ^ 2 + 4 * cos(theta1) * cos(theta2) * sin(theta1) * 
      sin(theta2) * sin((dphi) / 2) ^ 2
    t.angle <- sqrt(t.angle)
    angle.rad.EP <- asin(t.angle)
    if (cos(theta1) * cos(theta2) * cos(dphi) + sin(theta1) * sin(theta2) < 0) {
      angle.rad.EP <- pi - angle.rad.EP
    }
    # Use earth radius in meter to return distance.
    radius <- 6371 * 1e3
    return(radius * angle.rad.EP)
  }

  distance.2.points.hs <- function(lat1, lon1, lat2, lon2) {
    theta1 <- lat1 * pi / 180
    phi1 <- lon1 * pi / 180
    theta2 <- lat2 * pi / 180
    phi2 <- lon2 * pi / 180
    dtheta <- theta2 - theta1
    dphi <- phi2 - phi1
    # Compute angle.
    t.angle <- sin(dtheta / 2) ^ 2 + cos(theta1) * cos(theta2) * sin(dphi/2) ^ 2
    angle.rad <- 2 * atan2(sqrt(t.angle), sqrt(1 - t.angle))
    # Use earth radius in meter to return distance .
    radius <- 6371 * 1e3
    return(radius * angle.rad)
  }

# Vectorize functions.
  distance.2.points.vect <- Vectorize(distance.2.points.hs)

# Create shift function
  shift <- function(x, n) {
    c(x[-(seq(n))], rep(NA, n))
  }

# Extracting the locations dataframe.
  loc <- x$locations

# Converting time column from posix milliseconds into a readable time scale.
loc$time <- as.POSIXct(as.numeric(x$locations$timestampMs)/1000, 
  origin = "1970-01-01",tz="")  # Change tz to "GT [Incomplete?]

# Converting longitude and latitude from E7 to GPS coordinates.
  loc$lat <- loc$latitudeE7 / 1e7
  loc$lon <- loc$longitudeE7 / 1e7

# Add columns with shifted values (note that the dataframe is ) [Incomplete?]
  loc$lat.old <- shift(loc$lat, 1)
  loc$lon.old <- shift(loc$lon, 1)
  loc$time.old <- shift(loc$time, 1)
# Remove last line which is undefined in the columns just created.
  loc <- head(loc, - 1)

  loc$delta.time <- as.numeric(loc$time - loc$time.old, units = "secs")
  loc$distance <- distance.2.points.vect(loc$lat.old, loc$lon.old, 
    loc$lat, loc$lon)
  loc$computed.velocity <- loc$distance / loc$delta.time
  loc$hour.in.day <- hour(loc$time) + minute(loc$time) / 60 + second(loc$time) / 
    3600

# We consider the timestamp is in the middle of the interval.
  loc$hour.in.day <- loc$hour.in.day - loc$delta.time / 3600 

# Make sure it stays in [0, 24] interval.
  loc$hour.in.day[which(loc$hour.in.day < 0)] <- 24 + 
    loc$hour.in.day[which(loc$hour.in.day < 0)]  

# Filter timeframe.
  start.analysis <- as.POSIXct('2018-02-02 00:50:00')
  end.analysis <- as.POSIXct('2019-02-02 23:59:59')

  nb.day.analysis <- as.numeric(end.analysis - start.analysis, units = "days")
  loc <- with(loc, subset(loc, loc$time >= start.analysis))
  loc <- with(loc, subset(loc, loc$time < end.analysis))
  print(paste0("Number of time.stamps in selected period: " , length(loc$time)))

# Add activity types.
  list.with.act <- sapply(loc$activity, function(x) !is.null(x[[1]]))
  activities <- loc$activity[list.with.act]
  print(paste0("Number of time.stamps with activity type in selected period: ",
    length(activities)))

  loc$main.activity[list.with.act] <- sapply(activities, function(x) { 
    x$activity[[1]]$type[1]
  	}
  )
  loc$sub.activity[list.with.act] <- sapply(activities, function(x) {
    x$activity[[1]]$type[2]
  	}
  )

# Fill the blanks with next non-NA value.
  goodIdx <- !is.na(loc$sub.activity)
  goodVals <- c(NA, loc$sub.activity[goodIdx])
  fillIdx <- cumsum(goodIdx) + 1
  loc$sub.activity <- goodVals[fillIdx]

# Check if second activity is the relevant parameter 
# when the main activity is IN_VEHICLE
  pos.road <- which(loc$sub.activity == "IN_ROAD_VEHICLE")
  check.fun <- which(loc$main.activity[pos.road] != "IN_VEHICLE")
  if(length(check.fun) == 0) {
    print("The main activity is always IN_VEHICLE when the second 
    main activity is IN_ROAD_VEHICLE")
  } else {
    print(paste0("!!! BEWARE !!! The main activity is NOT always 
    IN_VEHICLE when the second main activity is IN_ROAD_VEHICLE in ", 
    length(check.fun), " lines"))
  }

# Print activity distribution.
  print("Main activity distribution over time period")
  table(loc$main.activity)
  print("Second main activity distribution over time period")
  table(loc$sub.activity)

## [Not run] Display main type when subtype is in vehicle.
# table(loc$main.activity[which(loc$sub.activity == "IN_VEHICLE")])
##  [Not run] Plot to have an idea of the speed distribution.
# hist(loc$computed.velocity[which(loc$sub.activity == "IN_VEHICLE" & 
# loc$computed.velocity < 50 & loc$computed.velocity > 1)])

## START OF ROAD VEHICLE DATA ANALYSIS.
# Extract vehicle data.
# Velocity value (in m/s) beyond which we will consider the data to 
# be irrelevant (and thus exclude it from the analysis) => proposed value: 50
  speed.threshold <- 50
  day.subd <- 24 # The number of subdivision in day for the histogram.
  df.in.road.vehicle <- loc[which(loc$sub.activity == "IN_ROAD_VEHICLE"), ]
  nb.pts <- length(df.in.road.vehicle$time)
  df.in.road.vehicle <- df.in.road.vehicle[
    which(df.in.road.vehicle$computed.velocity < speed.threshold), ] 
  print(paste0("Fraction of IN_ROAD_VEHICLE points above speed threshold ", 
    round(100 * (1 - length(df.in.road.vehicle$time) / nb.pts)), " %"))

  time.bounds <- seq(0, 24, length.out = day.subd + 1)
  nb.processed <- 0
  daily.dist <- sapply(1:day.subd, function(k) {
    start.hour <- time.bounds[k]
    end.hour <- time.bounds[k + 1]
    extr.hour <- which(df.in.road.vehicle$hour.in.day >= start.hour & 
      df.in.road.vehicle $hour.in.day < end.hour)
    sum(df.in.road.vehicle$distance[extr.hour]) / nb.day.analysis
    } 
  )
  
  print(paste0("Number of road_vehicle points processed: ",
    length(df.in.road.vehicle$hour.in.day)))
  print(paste0("Total driving distance during analysis period: ", 
    round(sum(df.in.road.vehicle$distance) / 1000)," km"))
  print(paste0("Total driving distance during analysis period without filter: ",
    round(sum(loc$distance) / 1000)," km"))
  print(paste0("Average speed during analysis period: ", 
    round(mean(df.in.road.vehicle$computed.velocity)) * 3.6, " km/h"))

  val <- 250 # km / h
  print(paste0("Total driving distance during analysis period below ", 
    val, " m/s: ", 
    round(sum(loc$distance[which(loc$computed.velocity < (val / 3.6))]) / 1000), 
    "km"))

  absc <- sapply(head(time.bounds, - 1), toString)
  barplot(daily.dist / 1000, names.arg = time.bounds[-1], 
    main = "Car use around the clock", xlab = "Hour of the day", ylab = "km")

##################################

# Jahresansicht
df.in.road.vehicle %>% mutate(date_ = date(time)) %>% 
  group_by(date_) %>% summarise(daily.sum = sum(distance / 1000)) %>% 
  arrange(desc(daily.sum)) %>% ggplot(., aes(x=1:nrow(.), y = daily.sum)) + 
  geom_point()

# Monatsansicht
df.in.road.vehicle %>% 
  mutate(date_ = date(time)) %>% group_by(date_) %>% 
  summarise(daily_sum = sum(distance / 1000)) %>% mutate(m = month(date_)) %>% 
  arrange(desc(daily_sum)) %>% ggplot(., aes(x=1:nrow(.), y = daily_sum)) + 
  geom_point() + facet_wrap(~ m)