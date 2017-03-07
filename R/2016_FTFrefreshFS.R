# Feed the Future Ethiopia Analysis -----------------------------------------
#
# 01_plotFtf2016 -- plot the updated food security perceptions questions
#
# Data are from the LSMS
#
# Tim Essam, tessam@usaid.gov 
#
# Copyright 2016 by Tim Essam via MIT License
#
# -------------------------------------------------------------------------

library(forcats)
library(tidyverse)
library(gganimate)
library(cowplot)
library(RColorBrewer)
#devtools::install_github("dgrtwo/gganimate")
library(ggthemes)
#install_github(c("hadley/ggplot2", "jrnold/ggthemes"))

setwd("~/Ethiopia/Export")
data <- read.csv("ftfAnalysis_2016.csv")

# Tidy data one more time to enable animated plots
df = gather(data, 3:12, key = "indic", value = "mean") 

ggplot(filter(df, indic != "totMonFoodlack"), aes(x = year, y = mean, group = ftfzone_2016)) + 
  geom_line(colour = "gray55", linetype = 2) +
  geom_line(data = filter(df, indic != "totMonFoodlack" & ftfzone_2016 == "FTF household"), colour = "gray40")+
  geom_point(aes(colour = -mean), size = 3) +
  geom_point(shape =1, size = 3,colour = "gray55") +
  facet_wrap(~indic, ncol = 3) +
  theme_fivethirtyeight() +
  ggtitle("Households in Feed the Future Zones appear no better off than prior to the program") +
  theme(legend.position="none")
  
