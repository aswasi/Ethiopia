# Purpose: Crack open LSMS data, offset GPS, and plot a bit and look at spat. corr.
# Author: Tim Essam (GeoCenter / OakStream Systems, LLC)
# Required packages: lots of spatial package
# Date: 1/9/2015

# Clear the workspace
remove(list=ls())

# load libraries for use in tinkering with data
libs <- c ("geoR", "akima", "leaflet", "dplyr", "lattice",
	"sp", "maptools", "raster", "rgdal", "maps", "mapdata",
	"RgoogleMaps", "mapproj", "RColorBrewer", "ape")

# Load required libraries
lapply(libs, require, character.only=T)

# Set working directory to Ethiopia project
wdw <- c("U:/Ethiopia/Export")
setwd(wdw)

# Read in data; subset GPS info and jitter for no overlap
d <- read.csv("shocks.exploratory.csv", header=TRUE)

#Drop records with missing lat/lon values
d <- na.omit(d)

gps <- subset(d, select = c(lon, lat))
jitgps <- jitter2d(gps, max=0.01)

# Subset data to be recobined with GIS info
data <- subset(d, select = c(ag, conflict, disaster, financial, health,
	other, priceup, pricedown, totShock, hhid))
geo <- cbind(jitgps, data)

# Export jittered data to GIS/export folder
write.csv(geo, "jittered.LSMS.csv")

# Convert data frame to geodata
dgeo <- as.geodata(geo, coords.col = 1:2, data.col = 3:12)

# Plot data
plot(dgeo)

# create a basic plot of totShocks (using sp package)
coordinates(geo) <- c("lon", "lat")

# For downloading a static webmap from server
latb <- c(min(geo$lat), max(geo$lat))
lonb <- c(min(geo$lon), max(geo$lat))
center <- c(mean(latb), mean(lonb))
zoom <- min(MaxZoom(range(geo$lat), range(geo$lon)))
GetMap(center=center, zoom=zoom, maptype= "terrain", destfile = "MyTile1.png")

# Plot total, priceup, ag, health, and disaster
map("worldHires", "Ethiopia") 
points(geo)

spplot(geo, "totShock") 
spplot(geo, "ag")
spplot(geo, "health")
spplot(geo, "disaster")
spplot(geo, "priceup")

# Convert to grid
rast <- raster(ncol = 50, nrow = 50)
extent(rast) <- extent(geo)
r <- rasterize(geo, rast, geo$totShock, fun = mean)
plot(r)


YlOrRd = colorNumeric("Reds", domain = geo$totShock)
m = leaflet(geo) %>% addTiles()
m = m %>% addCircleMarkers(~geo$lon, ~geo$lat, fill = FALSE, radius = ~geo$totShock*5, color = ~YlOrRd (geo$totShock)) 

# Interpolate the surface (http://www.biostat.umn.edu/~brad/8472/slidegeoR.pdf)
int.scp <- interp.new(geo$lon, geo$lat, geo$priceup)
image(int.scp, xlim = range(geo$lon), ylim = range(geo$lat))
contour(int.scp, add = T)

# Variogram fitting
shocks <- as.data.frame(cbind(geo$lon, geo$lat, geo$priceup))
shocks.geo <- as.geodata(shocks, coords.col=1:2, data.col=3) # Create geo-data
shocks.var <- variog(shocks.geo, estimator.type="classical")
plot(shocks.var)

# Generate a distance matrix(http://www.ats.ucla.edu/stat/r/faq/morans_i.htm)
shock.dist <- as.matrix(dist(cbind(geo$long, geo$lat)))

# Invert the matrix, replace diagonals with zero
shock.dist.inv <- 1/shock.dist
diag(shock.dist.inv) <- 0
shock.dist.inv[1:5, 1:5]

# Check for spatial autocorrelation
Moran.I(geo$ag, shock.dist.inv)
Moran.I(geo$disaster, shock.dist.inv)
Moran.I(geo$health, shock.dist.inv)
Moran.I(geo$priceup, shock.dist.inv)
Moran.I(geo$totShock, shock.dist.inv)
