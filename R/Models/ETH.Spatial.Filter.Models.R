# Purpose: Run satial logistic regressions on shock variables
# Author: Tim Essam, PhD (USAID GeoCenter)
# Date: 2015/08/07
# packages: RColorBrewer, spdep, classInt, foreign, MASS, maptools, ggplot2


# Clear the workspace
remove(list = ls())

req_lib <- c("RColorBrewer", "spdep", "classInt", "foreign", "MASS", "maptools", "ggplot2", "dplyr", "dummies", "useful", "coefplot")
lapply(req_lib, library, character.only = TRUE)

# --- Set working directory for home or away
wd <- c("U:/Ethiopia/Export/")
wdw <- c("C:/Users/Tim/Documents/Ethiopia/Export")
wdh <- c("C:/Users/t/Documents/Ethiopia/Export")
setwd(wdw)

file.name = "ETH_201508_2012_analysis.csv"

d  <- read.csv(file.name, header = TRUE, sep = ",")
names(d)
str(d)

# -- Disable scientific notiation
options(scipen = 999)


# Convert factors to numeric so they don't enter regression as dummies
# cols <- c(15, 20, 27, 28, 29, 30)
# names(d[cols])
# d[, cols]  <- apply(d[, cols], 2, function(x) as.numeric(x)) 

# Select only households that have a latitude and longitude
d.gps <- filter(d, !is.na(latitude), !is.na(longitude))

x <- d.gps$latitude
y <- d.gps$longitude

# --- Plotting data to check it's correct
# Plot data to check consistency
eth.coord <- SpatialPoints(d.gps[, c("longitude", "latitude")])
d.gps2 <- SpatialPointsDataFrame(eth.coord, d.gps)
coord_eth <- coordinates(d.gps2)

class(d.gps2)
eth_k2 <- knn2nb(knearneigh(coord_eth, k = 2, longlat = T))

# Plot the results
plot(as(d.gps2, "Spatial"), axes = T)
plot(eth_k2, coord_eth, add = T)
plot(d.gps2[d.gps2$ftfzone == 1, ], col = "red", add = TRUE)

# --- Set up a spatial weights matrix for regressions
# Using a distance threshold of 125 to ensure everyone has a neighbor
xy <- as.matrix(d.gps[5:6])
distThresh <- dnearneigh(xy, 0, 125, longlat = TRUE)

# Set up a distance threshold of a weights matrix within 100km
weights <- nb2listw(distThresh, style = "W")

religDum <- dummy(d.gps$religHoh)
religDum <- as.data.frame(religDum[, 1:4]) # Drop NAs from analysis
religDum <- rename(religDum, Protestant = religHoh3, Muslim = religHoh4, Other = religHoh7)
landqDum <- dummy(d.gps$landQtile)
landqDum <- as.data.frame(landqDum[, 1:4])


# combine vectors of dummies
d.reg <- cbind.data.frame(d.gps, religDum, landqDum)

# Define exogenous paramenters for the model

exog.all <- dplyr::select(d.reg, agehead, ageheadsq, femhead, marriedHoh, vulnHead, 
                          Protestant, Muslim, Other, literateHoh, educAdultM_cnsrd, educAdultF_cnsrd, 
                          gendMix, ae, mlabor, flabor, hhsize,
                          ftfzone, TLUtotal_cnsrd, wealthIndex, landHectares, landQtile2, landQtile3,
                          landQtile4)
exog <- as.matrix(exog.all)


# Run the SAR error model 
## This applies a spatial error model.  The catch is that this essentially treats it as a linear regression, 
## ignoring any complexity from the fact that the shocks are really binary variables.
#sar <- errorsarlm(depvar ~ exog, listw = weights, na.action = na.omit)
#summary(sar)

#Create spatial filter by calculating eigenvectors.
weightsB <- nb2listw(distThresh, style = "B")

## We need a non-row-standardized set of weights here, so style = "B"
n <- length(distThresh)
M <- diag(n) - matrix(1,n,n)/n
B <- listw2mat(weightsB)
MBM <- M %*% B %*% M
eig <- eigen(MBM, symmetric=T)
EV <- as.data.frame( eig$vectors[ ,eig$values/eig$values[1] > 0.25])

# ---- Fitting models; Use a full GLM and a step-wise AIC-based model to selet key filters

# Format a pipeline for the regressions
source("C:/Users/Tim/Documents/GitHub/Ethiopia/R/Models/results.formatter.R")

# Set up formatting for table and alignment of columns
two_digits <- . %>% fixed_digits(2)
table_names <- c("Parameter", "Estimate", "Std. Err.", "_t_", "_p_")
alignment <- c("l", "r", "r", "r", "r")
fix_names <- . %>% str_replace_all("x", "")

# Setup automation to format table as desired
format_model_table <- . %>%
  mutate_each(funs(two_digits), 
              -term, -p.value) %>%
  mutate(term = fix_names(term), 
         p.value = format_pval(p.value)) %>%
  set_colnames(table_names)
# --- Spatial Filter Model function ---
spatReg <- function(y, x) {
  
    ## Use <<- to return a global object that can be called outside function
    full.glm <- glm(y ~ x + ., data = EV, family = binomial)

    # Fit the spatial filter model; Will keep model with lowest AIC
    sp.glm <- stepAIC(glm(y ~ x , data=EV, family=binomial), 
                      scope=list(upper=full.glm), direction="forward")
    
    return(list(full.glm, sp.glm))
}

# ---- Price Shocks ---
spatReg(d.gps$priceShk, exog)
price.res <- sp.glm

priceShk.Result <- tidy(price.res)
morans_test(price.res)

price.res %>% tidy %>% 
  format_model_table %>%
  kable(align = alignment)


# ---- Hazard Shocks ----
spatReg(d.gps$hazardShk, exog)
hzd.res <- sp.glm

hzdShk.Result <- tidy(hzd.res)
morans_test(hzd.res)

hzd.res %>% tidy %>% 
  format_model_table %>%
  kable(align = alignment)

# ---- Health Shocks ----
spatReg(d.gps$healthShk, exog)
hlth.res <- sp.glm

hlthShk.Result <- tidy(hlth.res)
morans_test(hlth.res)

hlth.res %>% tidy %>% 
  format_model_table %>%
  kable(align = alignment)

### -------- End of Binary Analysis ###
# --------------------------------------













