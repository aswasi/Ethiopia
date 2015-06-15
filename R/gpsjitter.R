# Purpose: Crack open LSMS data, offset GPS, and plot 
# Author: Tim Essam (GeoCenter / OakStream Systems, LLC)
# Required packages: lots of spatial package
# Date: 3/31/2015

# Clear the workspace
remove(list=ls())

# load libraries for use in tinkering with data
libs <- c ("geoR", "akima", "leaflet", "dplyr", "lattice",
           "sp", "maptools", "raster", "rgdal", "maps", "mapdata",
           "RgoogleMaps", "mapproj", "RColorBrewer", "ape", "haven")

# Load required libraries
lapply(libs, library, character.only=T)
search()

# Set working directory to Ethiopia project
wdw <- c("U:/Ethiopia/Export")
wdh <- c("c:/Users/Tim/Documents/Ethiopia/dataout")
setwd(wdh)

d <- read_dta("geovars.dta")

# Use geoR package to jitter the stacked coordinates
gps <- subset(d, select = c(latitude, longitude))
jitgps <- jitter2d(gps, max=0.01)
names(jitgps)[names(jitgps) == "latitude"] <- "lat"
names(jitgps)[names(jitgps) == "longitude"] <- "lon"

# Subset data to be recobined with both sets of GIS info
geo <- cbind(d, jitgps)


# Assign a reversed color brewer palette to the map
cols <- rev(brewer.pal(length(levels(geo$svy_status)), "Set2"))
geo$colors <- cols[unclass(geo$svy_status)]

# Define map calling leaflet function
map <- leaflet(geo) 

pal <- colorNumeric(
  palette = "Reds",
  domain = geo$geo_merge
  )


map %>%
  addCircles(lat = ~lat, lng = ~ lon, 
             color = ~colors, radius = ~svy_status
             ) %>%
  addLegend("bottomright", pal = pal, values = ~geo_merge,
            title = "Survey Status"
            ) %>%
  addTiles()