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

file.name = "ETH_201508_2014_analysis.csv"

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

x <- d.gps$longitude
y <- d.gps$latitude

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

# Use dummy to create dummy vars; Not program is a bit buggy hence all the repetition; TODO: redo in dplyr
religDum <- dummy(d.gps$religHoh)
religDum <- as.data.frame(religDum[, 1:4]) # Drop NAs from analysis
religDum <- rename(religDum, Protestant = religHoh3, Muslim = religHoh4, Other = religHoh7)
landqDum <- dummy(d.gps$landQtile_lag)
landqDum <- as.data.frame(landqDum[, 1:4])

landqDum2 <- dummy(d.gps$landQtile)
landqDum2 <- as.data.frame(landqDum2[, 1:4])

educMDum <- dummy(d.gps$educAdultM_cat)
educMDum <- as.data.frame(educMDum)
educMDum <- rename(educMDum, No_educ_male = educAdultM_cat0,
                                               Primary_male = educAdultM_cat1,
                                               Secondary_male = educAdultM_cat2,
                                               Tertiary_male = educAdultM_cat3)
educFDum <- dummy(d.gps$educAdultF_cat)
educFDum <- as.data.frame(educFDum)
educFDum <- rename(educFDum, No_educ_female = educAdultF_cat0,
                   Primary_female = educAdultF_cat1,
                   Secondary_female = educAdultF_cat2,
                   Tertiary_female = educAdultF_cat3)

# combine vectors of dummies
d.reg <- cbind.data.frame(d.gps, religDum, landqDum, landqDum2, educMDum, educFDum)

# Define exogenous paramenters for the model
exog.all <- dplyr::select(d.reg, agehead, ageheadsq, femhead, marriedHoh, vulnHead,
                          Protestant, Muslim, Other, literateHoh,
                          Primary_male, Secondary_male, Tertiary_male,
                          Primary_female, Secondary_female, Tertiary_female,
                          gendMix, ae, mlabor, flabor, hhsize,
                          ftfzone, TLUtotal_cnsrd_lag, wealthIndex_lag, landHectares_lag, landQtile_lag2,
                          landQtile_lag3, landQtile_lag4, iddirMemb_lag)
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
two_digits <- . %>% fixed_digits(3)
table_names <- c("Parameter", "Estimate", "Std. Err.", "_t_", "_p_")
alignment <- c("l", "r", "r", "r", "r")
fix_names <- . %>% str_replace_all("x.vars", "")

# Setup automation to format table as desired
format_model_table <- . %>%
  mutate_each(funs(two_digits),
              -term, -p.value) %>%
  mutate(term = fix_names(term),
         p.value = format_pval(p.value)) %>%
  set_colnames(table_names)

break
# ---- Price Shocks ---
y.vars <- d.gps$priceShk
x.vars <- exog
full.glm <- glm(y.vars ~ x.vars +., data = EV, family = gaussian())
price.res <- stepAIC(glm(y.vars ~ x.vars , data=EV, family=gaussian()),
                     scope=list(upper=full.glm), direction="forward")

priceShk.Result <- tidy(price.res) %>% format_model_table
#morans_test(price.res)

price.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)


# ---- Hazard Shocks ----
y.vars <- d.gps$hazardShk
full.glm <- glm(y.vars ~ x.vars +., data = EV, family = gaussian())
hzd.res <- stepAIC(glm(y.vars ~ x.vars , data=EV, family=gaussian()),
                   scope=list(upper=full.glm), direction="forward")

hzdShk.Result <- tidy(hzd.res)  %>% format_model_table
#morans_test(hzd.res)

hzd.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)

# ---- Health Shocks ----
y.vars <- d.gps$healthShk
full.glm <- glm(y.vars ~ x.vars +., data = EV, family = gaussian())
hlth.res <- stepAIC(glm(y.vars ~ x.vars , data=EV, family= gaussian()),
                   scope=list(upper=full.glm), direction="forward")

hlthShk.Result <- tidy(hlth.res)  %>% format_model_table
morans_test(hlth.res)

hlth.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)


# ---- Illness Shocks ----
y.vars <- d.gps$illnessShk
full.glm <- glm(y.vars ~ x.vars +., data = EV, family = gaussian())
ill.res <- stepAIC(glm(y.vars ~ x.vars , data=EV, family= gaussian()),
                    scope=list(upper=full.glm), direction="forward")

illnessShk.Result <- tidy(ill.res)  %>% format_model_table
morans_test(ill.res)

ill.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)

### -------- End of Binary Analysis ###
# --------------------------------------

# Update explanatory variables for food security analysis

exog.all <- dplyr::select(d.reg, agehead, ageheadsq, femhead, marriedHoh, vulnHead,
                          Protestant, Muslim, Other, literateHoh,
                          Primary_male, Secondary_male, Tertiary_male,
                          Primary_female, Secondary_female, Tertiary_female,
                          gendMix, iddirMemb_lag,ae, mlabor, flabor, hhsize,
                          ftfzone, TLUtotal_cnsrd, wealthIndex_lag, landHectares, landQtile2, landQtile3,
                          landQtile4, priceShk_lag, hazardShk_lag)
x.vars <- as.matrix(exog.all)


y.vars <- d.gps$FCS
full.glm <- glm(y.vars ~ x.vars + ., data = EV, family = gaussian())
fcs.res <- stepAIC(glm(y.vars ~ x.vars , data = EV, family = gaussian()) ,
                       scope = list(upper = full.glm), direction = "forward")

fcs.result <- tidy(fcs.res) %>% format_model_table

fcs.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)


# Diet diversity
y.vars <- d.gps$dd
full.glm <- glm(y.vars ~ x.vars + ., data = EV, family = gaussian)
dd.res <- stepAIC(glm(y.vars ~ x.vars , data = EV, family = gaussian) ,
                   scope = list(upper = full.glm), direction = "forward")


dd.result <- tidy(dd.res)
dd.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)


# Try fitting dd with a possion model
full.glm <- glm(y ~ x + ., data = EV, family = poisson)
dd.pois.res <- stepAIC(glm(y ~ x , data = EV, family = poisson),
                  scope = list(upper = full.glm), direction = "forward")

multiplot(dd.pois.res, dd.res)

# Check for overdispersion
z <- (d.gps$dd - dd.pois.res$fitted.values)/
  sqrt(dd.pois.res$fitted.values)

# Sum of overdispersion / degrees of freedom (great than 2 overdisperson)
sum(z^2) / dd.pois.res$df.residual

# P-value indicates that there is overdispersion
pchisq(sum(z^2), df = dd.pois.res$df.residual)

# Try fitting a negative-binomial (really should use zero-truncated)
full.glm <- glm(y ~ x + ., data = EV, family = quasipoisson(link = "log"))
dd.binom.res <- glm(y ~ x + V7 + V2 + V11 + V1 + V3 + V8 + V10 , data = EV, family = quasipoisson(link = "log"))

dd.binom.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)



# Look at the eigenvectors to see how the patterns play out spatially
EV.gps <- cbind.data.frame(EV, d.gps$latitude, d.gps$longitude, d.gps$ftfzone)
write.csv(EV.gps, file = "Eigenvectors.2014.csv")



# --------------------------------------
### Fit 2012 Data ###
# --------------------------------------


file.name = "ETH_201508_2012_analysis.csv"
d2  <- read.csv(file.name, header = TRUE, sep = ",")
names(d2)
str(d2)

d2.gps <- filter(d2, !is.na(latitude), !is.na(longitude))

x <- d2.gps$latitude
y <- d2.gps$longitude

# --- Set up a spatial weights matrix for regressions
# Using a distance threshold of 125 to ensure everyone has a neighbor
xy <- as.matrix(d2.gps[5:6])
distThresh <- dnearneigh(xy, 0, 125, longlat = TRUE)

# Set up a distance threshold of a weights matrix within 100km
weights <- nb2listw(distThresh, style = "W")

religDum <- dummy(d2.gps$religHoh)
religDum <- as.data.frame(religDum[, 1:4]) # Drop NAs from analysis
religDum <- rename(religDum, Protestant = religHoh3, Muslim = religHoh4, Other = religHoh7)
landqDum <- dummy(d2.gps$landQtile)
landqDum <- as.data.frame(landqDum[, 1:4])

# combine vectors of dummies
d2.reg <- cbind.data.frame(d2.gps, religDum, landqDum)

# Define exogenous paramenters for the model
exog.all <- dplyr::select(d2.reg, agehead, ageheadsq, femhead, marriedHoh, vulnHead,
                          Protestant, Muslim, Other, literateHoh, educAdultM_cnsrd, educAdultF_cnsrd,
                          gendMix, iddirMemb, ae, mlabor, flabor, hhsize,
                          ftfzone, TLUtotal_cnsrd_lag, wealthIndex_lag, landHectares, landQtile2, landQtile3,
                          landQtile4)
exog <- as.matrix(exog.all)


# Run the SAR error model
## This applies a spatial error model.  The catch is that this essentially treats it as a linear regression,
## ignoring any complexity from the fact that the shocks are really binary variables.
#sar <- errorsarlm(depvar ~ exog, listw = weights, na.action = na.omit)
#summary(sar)

#Create spatial filter by calculating eigenvectors.-
weightsB <- nb2listw(distThresh, style = "B")

## We need a non-row-standardized set of weights here, so style = "B"
n <- length(distThresh)
M <- diag(n) - matrix(1,n,n)/n
B <- listw2mat(weightsB)
MBM <- M %*% B %*% M
eig <- eigen(MBM, symmetric=T)
EV <- as.data.frame( eig$vectors[ ,eig$values/eig$values[1] > 0.25])

# Check correlations before plotting
library(corrplot)
dep.vars <- dplyr::select(d2.reg, priceShk, hazardShk, healthShk, fcsMin, dd)
exog.corr <- cor(cbind.data.frame(dep.vars, exog.all))
corrplot(exog.corr, method="color", tl.pos="lt", type="upper", tl.col = "gray50",
         addCoefasPercent = TRUE,
         p.mat = 1-abs(exog.corr), sig.level=0.95, insig = "blank")

# On to the models
# ---- Price Shocks ---
y <- d2.gps$priceShk
x <- exog
full.glm <- glm(y ~ x +., data = EV, family = binomial(link = "logit"))
price2.res <- stepAIC(glm(y ~ x , data=EV, family=binomial(link = "logit")),
                     scope=list(upper=full.glm), direction="forward")

priceShk2.Result <- tidy(price2.res)
morans_test(price2.res)

price2.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)

# ---- Hazard Shocks ----

y <- d2.gps$hazardShk
full.glm <- glm(y ~ x +., data = EV, family = binomial(link = "logit"))
hzdShk2.res <- stepAIC(glm(y ~ x , data=EV, family=binomial(link = "logit")),
                      scope=list(upper=full.glm), direction="forward")

hzdShk2.Result <- tidy(hzdShk2.res)
morans_test(hzdShk2.res)

hzdShk2.res  %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)

# ---- Health Shocks ----

y <- d2.gps$healthShk
full.glm <- glm(y ~ x +., data = EV, family = gaussian)
hlth2.res <- stepAIC(glm(y ~ x , data=EV, family=gaussian),
                     scope=list(upper=full.glm), direction="forward")

hlthShk2.Result <- tidy(hlth2.res)
morans_test(hlth2.res)

hlth.res %>% tidy %>%
  format_model_table %>%
  kable(align = alignment)

coefplot(hlth2.res, predictors=c("(Intercept)", "xagehead"))

# Create theme for plots to limit value of x-axis
mltplot <-  theme_bw()  +
 theme(legend.position = "top", legend.title=element_blank(), panel.border = element_blank(), legend.key = element_blank())

# Rename objects
health.Shock.2012 <- hlth.res
health.Shock.2014 <- hlth2.res
hazard.shock.2012 <- hzd.res
hazard.shock.2014 <- hzdShk2.res
price.shock.2012  <- price.res
price.shock.2014  <- price2.res


multiplot(health.Shock.2012, health.Shock.2014) + mltplot +
  ggtitle("Spatial Regression: Health Shocks") + scale_x_continuous(limits = c(-1, 1))

multiplot(hazard.shock.2012, hazard.shock.2014) + mltplot +
  ggtitle("Spatial Regression: Hazard Shocks")+ scale_x_continuous(limits = c(-2, 2))

multiplot(price.shock.2012, price.shock.2014) + mltplot +
  ggtitle("Spatial Regression: Price Shocks")+ scale_x_continuous(limits = c(-2, 2))





