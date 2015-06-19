# --- Plotting LSMS/RIGA panel data for map products
# Date: 2015.04
# By: Tim Essam, Phd
# For: USAID GeoCenter

# --- Clear workspace, set library list
remove(list = ls())
libs <- c ("reshape", "ggplot2", "dplyr", "RColorBrewer", "grid", "scales", "stringr", "directlabels", "gmodels")

# Lab RGB colors
redL   <- c("#B71234")
dredL  <- c("#822443")
dgrayL <- c("#565A5C")
lblueL <- c("#7090B7")
dblueL <- c("#003359")
lgrayL <- c("#CECFCB")

# --- Load required libraries
lapply(libs, require, character.only=T)

# --- Set working directory for home or away
wd <- c("U:/UgandaPanel/Export/")
wdw <- c("C:/Users/Tim/Documents/Ethiopia/Export")
wdh <- c("C:/Users/t/Documents/Ethiopia/Export")
wdgp <- c("C:/Users/Tim/Documents/Ethiopia/Graph")
setwd(wdw)


# Create settings for fitting a smooth trendline
stat.set <- stat_smooth(method = "loess", size = 1, se = "TRUE", span = 1, alpha = 1)


# --- Set plot specifications for reuse throughout file
g.spec <- theme(legend.position = "none", legend.title=element_blank(), 
                panel.border = element_blank(), legend.key = element_blank(), 
                legend.text = element_text(size = 14), #Customize legend
                plot.title = element_text(hjust = 0, size = 17, face = "bold"), # Adjust plot title
                panel.background = element_rect(fill = "white"), # Make background white 
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #remove grid    
                axis.text.y = element_text(hjust = -0.5, size = 14, colour = dgrayL), #soften axis text
                axis.text.x = element_text(hjust = .5, size = 14, colour = dgrayL),
                axis.ticks.y = element_blank(), # remove y-axis ticks
                axis.title.y = element_text(colour = dgrayL),
                #axis.ticks.x=element_blank(), # remove x-axis ticks
                #plot.margin = unit(c(1,1,1,1), "cm"),
                plot.title = element_text(lineheight = 1 ), # 
                panel.grid.major = element_blank(), # remove facet formatting
                panel.grid.minor = element_blank(),
                strip.background = element_blank(),
                strip.text.x = element_text(size = 13, colour = dgrayL, face = "bold"), # format facet panel text
                panel.border = element_rect(colour = "black"),
                panel.margin = unit(2, "lines")) # Move plot title up


# --- Stunting indicators at individual level
# --- Read in as a dplyr data frame tbl
d.ind <- tbl_df(read.csv("ETH_201506_cHealth.csv"))

# Look at stunting by months of age across regions
# First cross-tabulate data to get percentages for each region
library(gmodels)

# Filter out observations with missing stunting data
d.indf <- filter(d.ind, stunted!="NA", region!="", year!="NA") 

CrossTable(d.indf$stunted, d.indf$region, format = "SAS")


# Create labels for year variable
d.indf$year <- factor(d.indf$year, levels = c(2012, 2014), 
                      labels = c("2011/12", "2013/14"))

# Call custom functions so g.spec1 is loaded
source("C:/Users/Tim/Documents/GitHub/Custom.functions/Custom.functions.r")

# Relevel factors for stratum to get order on graphics
d.indf$region <- factor(d.indf$region, levels = c("Tigray", "Amhara", "SNNP", 
                                                      "Other regions", "Oromia", "Addis Ababa"))

setwd(wdgp)
# --- First plot data overtime and ignore age
png("stunting.density.png", width=1000, height=700, res=120)
p <- ggplot(d.indf, aes(x = stunting)) + geom_density(aes(fill = region, y = ..count..)) + 
  facet_wrap(region~year, ncol = 4) +
  geom_vline(xintercept = c(-2.0), alpha = 0.25, linetype ="dotted", size = 1) + 
  scale_fill_brewer(palette = "Accent") + theme(legend.position = "none") +
  labs(x = "\n Stunting Z-score", y = "Number of observations \n")
plot(p)
dev.off()



# Graph smoothed stunting rates with data jittered  
png("stunting.region.png", width=1300, height=700, res=120)
p <- ggplot(d.indf, aes(x = ageMonths, y = stunted, colour = factor(year))) + 
  stat_smooth(method = "loess", span = 1.0, size = 1.15, alpha = 0.1 )+
  facet_wrap(~region, ncol = 3) +
  geom_point(alpha=0.15) + geom_jitter(position = position_jitter(height=0.05), alpha = 0.175) + 
  theme(legend.position="top", legend.key = element_blank(), legend.title=element_blank()) +
  scale_x_continuous(breaks = seq(0, 60, 12)) +
  guides(color = guide_legend(override.aes = list(fill = NA))) + #Remove fill from legend box
  # customize y-axis
  geom_hline(yintercept = c(0.5), linetype = "dotted", size = 1, alpha = .25) +
  labs(x = "Age of child (in months)", y = "Percent stunted\n", # label y-axis and create title
       title = "Stunting rates are highest in Tigray", size = 13)+ 
  scale_colour_brewer(palette = "Dark2") + g.spec1
plot(p)
dev.off()


# Graph smoothed stunting rates with data jittered
png("stunting.gender.png", width=1100, height=600, res=120)
p <- ggplot(d.indf, aes(x = ageMonths, y = stunted, colour = year)) + 
  stat_smooth(method = "loess", se = TRUE, span = 1.0, size = 1.15, alpha = 0.15 )+
  facet_wrap(~region, ncol = 3) +
  geom_point(alpha=0.15) + geom_jitter(position = position_jitter(height=0.05), alpha = 0.10) + 
  theme(legend.position="top", legend.key = element_blank(), legend.title=element_blank())+
  geom_hline(yintercept = c(0.5), linetype = "dotted", size = 1, alpha = .25) +
  scale_x_continuous(breaks = seq(0, 60, 12)) +
  guides(color = guide_legend(override.aes = list(fill = NA))) + 
  # customize y-axis
  labs(x = "Age of child (in months)", y = "Percent stunted\n", # label y-axis and create title
       title = "", size = 13) + g.spec1 + scale_colour_brewer(palette = "Dark2") + g.spec1
plot(p)  
dev.off()

# Scatter the data to see how indicators correlate

# --- First filter data to only get those w/ regional info (doesn't really matter w/ ETH data)
target <- c("Tigray", "Amhara", "SNNP", 
            "Other regions", "Oromia", "Addis Ababa")
ggplot(filter(d.indf, region %in% target), aes(x = stunting, y = underweight)) + 
  geom_point()  + stat_binhex() + stat_smooth(method="loess", span=1, size = 1.25) + facet_wrap(~year) +
  scale_fill_gradientn(colours = c("#FEE0D2", "#DE2D26"), name = "Count", na.value = NA) +g.spec1 +
  labs(x = "\n Stunting Z-score", y = "Underweight Z-score \n", # label y-axis and create title
       title = "", size = 13) 

# Create scatter plots of the data by year, by region of representativeness
png("stunting.underwgt.png", width=1600, height=900, res=120)
p <- ggplot(d.indf, aes(x = stunting, y = underweight, colour = factor(year))) + geom_point(alpha=0.25) + 
  facet_wrap(~region) +
  stat_smooth(method="loess", span=1, size = 1.25, se = TRUE, alpha = 0.10) + 
  guides(color = guide_legend(override.aes = list(fill = NA))) + 
  scale_y_continuous(limits = c(-5,5)) +
  g.spec1 + scale_colour_brewer(palette = "Dark2") +
  labs(x = "\n Stunting Z-score", y = "Underweight Z-score \n", # label y-axis and create title
       title = "Stunting and underweight z-scores are positively correlated across every region", size = 13) 
plot(p)  
dev.off()

png("stunting.wasting.png", width=1600, height=900, res=120)
p <- ggplot(d.indf, aes(x = stunting, y = wasting, colour = factor(year))) + geom_point(alpha=0.25) + 
  facet_wrap(~region) +
  stat_smooth(method="loess", span=1, size = 1.25, se = TRUE, alpha = 0.10) + 
  guides(color = guide_legend(override.aes = list(fill = NA))) + 
  scale_y_continuous(limits = c(-5,5)) +
  g.spec1 + scale_colour_brewer(palette = "Dark2") +
  labs(x = "\n Stunting Z-score", y = "wasting Z-score \n", # label y-axis and create title
       title = "Stunting and wasting z-scores have a slight negative correlation", size = 13) 
plot(p)  
dev.off()

