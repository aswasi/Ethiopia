
# --- Read R-processed data from Ethiopia GitHub respository 
# Date: 2015.07
# By: Tim Essam, Phd
# For: USAID GeoCenter

remove(list = ls())

setwd("C:/Users/Tim/Documents/Ethiopia/Dataout/")
library(curl)
library(foreign)
library(dplyr)
library(haven)
d <- read.csv( curl("https://raw.githubusercontent.com/tessam30/Ethiopia/master/Data/hh_laura.csv") )
dim(d)
d.size = dim(d)[2]

# Read data from GitHub respository, change hh ids to characters for merging.
dhh <- tbl_df(read.csv(curl("https://raw.githubusercontent.com/tessam30/Ethiopia/master/Data/hh_laura.csv"), 
                colClasses = c(rep("factor", d.size)))[ ,(d.size-1):d.size])
dhh[] <- lapply(dhh, as.character)
#names(dhh) <- c("hhid", "hhid2")
d.stata <- cbind(d[,1:163], dhh)

# From the foreign package, write the results as a Stata file.
write.dta(d.stata, "C:/Users/Tim/Documents/Ethiopia/Dataout/hh_laura.dta")
