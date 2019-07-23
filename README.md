# gmap_analyzer
The original idea to this project was the question "What distance do I drive per day with my car?" and the idea to look at my google timeline. Google allows me to look at single days, but since GDPR we have the right to takeout our data and process it however we want.

## Goal
Provide a script that takes a google takeout dataset as input and outputs the distance driven per day reliably.


## How to takeout your google timeline
1. You must have opted-in to allow Google to track your location data (e.g. on your Android phone)
1. Go to https://takeout.google.com/
1. Select only "Location History"
1. Choose JSON format (to include the likelihood of which transportation type was taken)
1. Export the file

## Output

### Descriptive Statistics
Analyse the data, e.g. how often can coordinates be linked to certain countries:

```
# A tibble: 10 x 2
   country                 frequency
   <chr>                       <int>
 1 Switzerland                531806
 2 Germany                     60687
 3 Austria                     10649
 4 Italy                        5952
 5 France                       2841
 6 Canary Islands:Tenerife      1190
 7 China:Hong Kong:1             810
 8 China                         316
```
### Vizualization
We can find the Home (Median of the Longitude/Latitude) and calculate days when we have stayed at home or never took the car:

![Image of Graph](http://ladeplan.ch/wp-content/uploads/2019/07/example_days_home.png)

### Time of Usage
We can tell when we tend to drive, when we are at home or somewhere. This could be used to predict how much photovoltaic production can be stored in an electric car battery:

![Image of Graph](http://ladeplan.ch/wp-content/uploads/2019/07/example_hour_of_drive.png)
