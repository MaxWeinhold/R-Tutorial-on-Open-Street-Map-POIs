#This is a script for a tutorial
#You can learn to get the coordinates of points of interested by collecting data via open street map.

#For that purpose we will use the osmdata package.
if(!require("osmdata")) install.packages("osmdata")
library(osmdata)

#Do not forget to give credit to the creators.
citation ("osmdata")

#The sf we will need to make geometrical calculations.
if(!require("sf")) install.packages("sf")
library(sf)

#Further we need to access tidyverse.
if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("geosphere")) install.packages("geosphere")
library(geosphere)#package for calculating distance using longitude and latitude

#This tutorial is based on this page, but I would like to go one step further:
#https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/

#This may be cinemas, schools, traffic lights, stores and the like.
#Open Street Map OSM offers a wide range of categories here. You can get an overview of them on this page:
#https://wiki.openstreetmap.org/wiki/Map_features

#You can get a visual representation of the data we will ask for here on this page.
#http://overpass-turbo.eu

#First we determine which city we want to study.
city="Berlin"

#Then we choose a coordinate point that is inside this city.
#As an example I have chosen a point at the Brandenburg Gate.
#This is what you can do on Google Maps by right clicking.
#Or you use another data source.

Brandenburg_Gate=c(13.377336846520663,52.516264818429924)

#First is longitude second is latitude

#As second we build a query asking for traffic signals in Berlin.
q <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", "traffic_signals")

#Read the osm data format as a list in R.
signals <- osmdata_sf(q)

#If you access signals:
signals
#You will see it includes osm_points.
#osm-points store positions.

#access osm_points of signals and take a look.
signals$osm_points
#You will the this includes different features likes names, education or the address.

#But we are only intrerested in the points stored in the geometry.
signals$osm_points$geometry

#take a look how many signals we have in Berlin.
length(signals$osm_points$osm_id)

#let us create an vector for the distance between every signal and our point at the Brandenburg Gate.
distances=c(1:length(signals$osm_points$osm_id))

#We will fill this vector using this for loop.
#In each loop we will calculate the distance between one signal and our point.
for(i in 1:length(distances)){
  distances[i]=distm(Brandenburg_Gate, c(signals$osm_points$geometry[[i]][1],signals$osm_points$geometry[[i]][2]), fun=distGeo)
}

#Now let us the how large is the distance to the closest signal near the Brandenburg Gate
min(distances)
#Distance is measured in meters

#Now let us see how many signals we will find in a 3km radius around the Brandenburg Gate.
sum(distances < 1000)

#Next, lets see how close the nearest main road is
#For that purpose we create a new query

q2 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", "primary")

primary <- osmdata_sf(q2)

#Since now we are handeling street, we are not longer interested in osm_points but osm_lines
Lines_primary = st_transform(primary$osm_lines$geometry,4269)

#We need to convert our point at Brandenburg Gate to another data format
POINT_Brandenburg_Gate = as.data.frame(rbind(Brandenburg_Gate))
names(POINT_Brandenburg_Gate)[1]="long1"
names(POINT_Brandenburg_Gate)[2]="lat1"
POINT_Brandenburg_Gate = st_as_sf(POINT_Brandenburg_Gate, coords = c("long1","lat1"))
POINT_Brandenburg_Gate <- st_set_crs(POINT_Brandenburg_Gate, 4269)

#now we can use the st_distance() function to calculate each distance from our Point to each line in our primary street network.
#The smallest distance is:
min(st_distance(POINT_Brandenburg_Gate$geometry, Lines_primary))
