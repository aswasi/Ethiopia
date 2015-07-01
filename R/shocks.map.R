# --- Plotting Ethiopia ESS (LSMS) panel data for map products
# Date: 2015.06
# By: Tim Essam, Phd
# For: USAID GeoCenter

# --- Clear workspace, set library list
remove(list = ls())
libs <- c ("ggplot2", "dplyr", "RColorBrewer", "leaflet")

# --- Load required libraries
lapply(libs, require, character.only=T)

# --- Get data, load it, jitter it and map it
setwd("C:/Users/Tim/Documents/Ethiopia/Export/")
setwd("C:/Users/t/Documents/Ethiopia/Export")
d <- tbl_df(read.csv("LSMS.shocks.exploratory2014.csv"))

# --- Filter on major shocks that are to be mapped
shocks <- c()
d.sub <- select(d, household_id2, region, hazardShk, healthShk, 
                priceShk, goodcope, badcope, totShocks, lat_dd_mod, lon_dd_mod)

# Jitter lat long slightly
# Use geoR package to jitter the stacked coordinates
gps <- subset(d, select = c(lat_dd_mod, lon_dd_mod))
jitgps <- geoR::jitter2d(gps, max=0.01)
names(jitgps)[names(jitgps) == "lat_dd_mod"] <- "lat"
names(jitgps)[names(jitgps) == "lon_dd_mod"] <- "lon"

# Subset data to be recobined with both sets of GIS info
geo <- cbind(d.sub, jitgps)


# Make map a little prettier and add in legend
# Create a palette that maps factor levels to colors

geo$priceShk <- factor(geo$priceShk, levels = c(0, 1), 
                      labels = c("No Shock", "Shock"))

pal <- colorBin("Greens", domain = 0:1)

basemap <- leaflet(geo) %>%   addProviderTiles("Acetate.terrain") %>%
  addMarkers(clusterOptions = markerClusterOptions()) 
basemap


pal <- colorNumeric(palette = "Reds", domain = geo$totShocks)
eth.map <- leaflet(geo) %>% addProviderTiles("Acetate.terrain") %>%  
  addLegend(pal = pal, values = ~ totShocks, opacity = 1) %>%
  addCircleMarkers(lat = ~ lat, lng = ~ lon, color = ~pal(totShocks),
             radius = ~(geo$totShocks)*2, 
             group = "Total", popup = ~ region,
             fill = TRUE, fillColor = ~pal(totShocks), 
             stroke = FALSE, fillOpacity = 0.5)  %>%
  addMarkers(clusterOptions = markerClusterOptions()) 
eth.map



pal <- colorFactor(c("blue", "red"), domain = c("No Shock", "Shock"))
pr.map <- leaflet(geo) %>% addProviderTiles("Acetate.terrain") %>%  
  addLegend(pal = pal, values = ~ priceShk, opacity = 1) %>%
  addCircleMarkers(lat = ~ lat, lng = ~ lon, color = ~pal(priceShk),
                   radius =  ~ifelse(priceShk == "Shock", 7, 2),
                   group = "Total", popup = ~ region,
                   fill = TRUE, fillColor = ~pal(priceShk), 
                   stroke = FALSE, fillOpacity = 0.5)
pr.map


geo$healthShk <- factor(geo$healthShk, levels = c(0, 1), 
                       labels = c("No Shock", "Shock"))

hlth.map <- leaflet(geo) %>% addProviderTiles("Acetate.terrain") %>%  
  addLegend(pal = pal, values = ~ healthShk, opacity = 1) %>%
  addCircleMarkers(lat = ~ lat, lng = ~ lon, color = ~pal(healthShk),
                   radius =  ~ifelse(healthShk == "Shock", 7, 2),
                   group = "Total", popup = ~ region,
                   fill = TRUE, fillColor = ~pal(healthShk), 
                   stroke = FALSE, fillOpacity = 0.5)
hlth.map


geo$goodcope <- factor(geo$goodcope, levels = c(0, 1), 
                        labels = c("Not good", "Good"))

cope.map <- leaflet(geo) %>% addProviderTiles("Acetate.terrain") %>%  
  addLegend(pal = pal, values = ~ goodcope, opacity = 1) %>%
  addCircleMarkers(lat = ~ lat, lng = ~ lon, color = ~pal(goodcope),
                   radius =  ~ifelse(goodcope == "Good", 7, 2),
                   group = "Total", popup = ~ region,
                   fill = TRUE, fillColor = ~pal(goodcope), 
                   stroke = FALSE, fillOpacity = 0.5)
cope.map  




  