
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
loc %>% group_by(country) %>% summarize(frequency = n_distinct(time)) %>% arrange(desc(frequency))



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





####################################3



#functions computing distance in meters between two GPS coordinates

distance_2_points_EP <- function(lat1, lon1, lat2, lon2) {
  #convert radian
  theta1 <- lat1 * pi / 180
  phi1 <- lon1 * pi / 180
  theta2 <- lat2 * pi / 180
  phi2 <- lon2 * pi / 180
  dtheta <- theta2 - theta1
  dphi <- phi2- phi1
  #compute angle
  t <- sin(dtheta)^2 + sin(dphi)^2*cos(theta1)^2*cos(theta2)^2 + 4*cos(theta1)*cos(theta2)*sin(theta1)*sin(theta2)*sin((dphi)/2)^2
  t <- sqrt(t)
  angle_rad <- asin(t)
  if (cos(theta1)*cos(theta2)*cos(dphi)+sin(theta1)*sin(theta2)<0){
    angle_rad <- pi - angle_rad 
  }
  #use earth radius in meter to return distance
  radius <- 6371*1e3
  return(radius*angle_rad)
}

distance_2_points_hs <- function(lat1, lon1, lat2, lon2) {
  theta1 <- lat1 * pi / 180
  phi1 <- lon1 * pi / 180
  theta2 <- lat2 * pi / 180
  phi2 <- lon2 * pi / 180
  dtheta <- theta2 - theta1
  dphi <- phi2- phi1
  #compute angle
  t <- sin(dtheta/2)^2 + cos(theta1) * cos(theta2) * sin(dphi/2)^2;
  angle_rad <- 2 * atan2(sqrt(t), sqrt(1-t));
  #use earth radius in meter to return distance 
  radius <- 6371*1e3
  return(radius*angle_rad)
}

#vectorize functions
distance_2_points_vect <- Vectorize(distance_2_points_hs)

#create shift function
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}


# extracting the locations dataframe
loc <- x$locations

# converting time column from posix milliseconds into a readable time scale
loc$time <- as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01",tz="")  #change tz to "GT

# converting longitude and latitude from E7 to GPS coordinates
loc$lat <- loc$latitudeE7 / 1e7
loc$lon <- loc$longitudeE7 / 1e7

#add columns with shifted values (note that the dataframe is )
loc$lat_old <- shift(loc$lat,1)
loc$lon_old <- shift(loc$lon,1)
loc$time_old <- shift(loc$time,1)
#get rid of last line which is undefined in the columns just created
loc <- head(loc,-1)

loc$delta_time <- as.numeric(loc$time - loc$time_old, units="secs")
loc$distance <- distance_2_points_vect(loc$lat_old,loc$lon_old,loc$lat,loc$lon)
loc$computed_velocity <- loc$distance/loc$delta_time

loc$hour_in_day <- hour(loc$time)+minute(loc$time)/60+second(loc$time)/3600
loc$hour_in_day <- loc$hour_in_day-loc$delta_time/3600 #we consider the timestamp is in the middle of the interval
loc$hour_in_day[which(loc$hour_in_day<0)] <- 24 + loc$hour_in_day[which(loc$hour_in_day<0)]  #make sure it stays in [0,24] interval

#filter timeframe
start_analysis = as.POSIXct('2018-02-02 00:50:00')
end_analysis = as.POSIXct('2019-02-02 23:59:59')

nb_day_analysis = as.numeric(end_analysis - start_analysis,units="days")
loc <- with(loc, subset(loc, loc$time >= start_analysis))
loc <- with(loc, subset(loc, loc$time < end_analysis))
print(paste0("Number of time_stamps in selected period: ",length(loc$time)))

#add activity types
list_with_act <- sapply(loc$activity, function(x) !is.null(x[[1]]))
activities <- loc$activity[list_with_act]
print(paste0("Number of time_stamps with activity type in selected period: ",length(activities)))

loc$main_activity[list_with_act] <- sapply(activities, function(x) x$activity[[1]]$type[1])
loc$sub_activity[list_with_act] <- sapply(activities, function(x) x$activity[[1]]$type[2])


#fill the blanks with next not NA value
goodIdx <- !is.na(loc$sub_activity)
goodVals <- c(NA, loc$sub_activity[goodIdx])
fillIdx <- cumsum(goodIdx)+1
loc$sub_activity <- goodVals[fillIdx]

#check if second activity is the relevant parameter when the main activity is IN_VEHICLE
pos_road <- which(loc$sub_activity == "IN_ROAD_VEHICLE")
check <- which(loc$main_activity[pos_road] != "IN_VEHICLE")
if(length(check)==0){
  print("The main activity is always IN_VEHICLE when the second main activity is IN_ROAD_VEHICLE ")
}else{
  print(paste0("!!! BEWARE !!! The main activity is NOT always IN_VEHICLE when the second main activity is IN_ROAD_VEHICLE in ", length(check), " lines"))
}

#print activity distribution
print("Main activity distribution over time period")
table(loc$main_activity)
print("Second main activity distribution over time period")
table(loc$sub_activity)

#display main type when subtype is in vehicle
#table(loc$main_activity[which(loc$sub_activity=="IN_VEHICLE")])
#plot to have an idea of the speed distribution
#hist(loc$computed_velocity[which(loc$sub_activity=="IN_VEHICLE"&loc$computed_velocity<50&loc$computed_velocity>1)])

#
#START OF ROAD VEHICLE DATA ANALYSIS 
#

#extract vehicle data
speed_threshold = 50  #velocity value (in m/s) beyond which we will consider the data to be irrelevant (and thus exclude it from the analysis) => proposed value: 50
day_subd = 24 #number of subdivision in day for the histogram

df <- loc[which(loc$sub_activity=="IN_ROAD_VEHICLE"),]
nb_pts <- length(df$time)
df <- df[which(df$computed_velocity<speed_threshold),] 
print(paste0("Fraction of IN_ROAD_VEHICLE points above speed threshold ", round(100*(1-length(df$time)/nb_pts)), " %"))

time_bounds <- seq(0, 24, length.out=day_subd+1)
daily_dist <- rep(0,day_subd)
nb_processed <- 0
for (k in 1:day_subd){
  start_hour <- time_bounds[k]
  end_hour <- time_bounds[k+1]
  extr_hour <- which(df$hour_in_day>=start_hour&df$hour_in_day<end_hour)
  daily_dist[k] <- sum(df$distance[extr_hour])/nb_day_analysis
}

print(paste0("Number of road_vehicle points processed: ",length(df$hour_in_day)))
print(paste0("Total driving distance during analysis period: ",round(sum(df$distance)/1000)," km"))
print(paste0("Total driving distance during analysis period without filter: ",round(sum(loc$distance)/1000)," km"))
print(paste0("Average speed during analysis period: ",round(mean(df$computed_velocity))*3.6," km/h"))


val=250 #km/h
print(paste0("Total driving distance during analysis period below ", val, " m/s: ", round(sum(loc$distance[which(loc$computed_velocity<(val/3.6))])/1000),"km"))

absc=sapply(head(time_bounds,-1),toString)
barplot(daily_dist/1000, names.arg = time_bounds[-1], main = "Car use around the clock",xlab = "Hour of the day", ylab = "km")


##################################

#Jahresansicht
df %>% 
  mutate(date_ = date(time)) %>% 
  group_by(date_) %>% 
  summarise(daily_sum = sum(distance/1000)) %>% 
  arrange(desc(daily_sum)) %>% 
  ggplot(., aes(x=1:nrow(.), y=daily_sum)) + 
  geom_point()

#Monatsansicht
df %>% 
  mutate(date_ = date(time)) %>% 
  group_by(date_) %>% 
  summarise(daily_sum = sum(distance/1000)) %>% 
  mutate(m = month(date_)) %>% 
  arrange(desc(daily_sum)) %>% 
  ggplot(., aes(x=1:nrow(.), y=daily_sum)) + 
  geom_point() + 
  facet_wrap(~ m)



