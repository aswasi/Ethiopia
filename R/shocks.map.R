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
map <- leaflet(geo) %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("CartoDB.Positron", group = "Carto-DB") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner") %>%
  addCircles(lat = ~lat, lng = ~ lon, color = ~priceShk, group = "Price") %>%
    addLayersControl(
    baseGroups = c("Base Map", "Carto-DB", "Toner"),
    overlayGroups = c("Price"),
    options = layersControlOptions(collapsed = FALSE)
  ) 
map




  