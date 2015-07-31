# --- Plotting LSMS/RIGA panel data for map products
# Date: 2015.04
# By: Tim Essam, Phd
# For: USAID GeoCenter

# --- Clear workspace, set library list
remove(list = ls())
libs <- c ("reshape", "ggplot2", "dplyr", "RColorBrewer", "grid", "scales", "stringr", "directlabels", "gmodels", "reshape", "SciencesPo")

# --- Load required libraries
lapply(libs, require, character.only=T)

# --- Set working directory for home or away
# wd <- c("U:/UgandaPanel/Export/")
wdw <- c("C:/Users/Tim/Documents/Ethiopia/Export/")
wdh <- c("C:/Users/t/Documents/Ethiopia/Export/")
setwd(wdw)

# --- Read in as a dplyr data frame tbl
d <- tbl_df(read.csv("Panel.wealth.analysis.csv"))

# --- Colors for Shocks ---
# Health: PuRd
# Assets: PuBu
# Prices: Greens
# Hazards: YlOrBr


# plot settings
# Create settings for fitting a smooth trendline
stat.set1 <- stat_smooth(method = "loess", size = 1, se = "TRUE", span = 1, alpha = 1)

# Set alpha settings (for transparency -- below 1 is not good for Adobe illustrator exports)
transp <- c(1)
dpi.out <- c(300)



# Histogram
ggplot(d, aes(x = wealthPanel, colour = factor(year))) + geom_histogram() + facet_wrap(~saq01)
ggplot(d, aes(x = wlthSmooth, y = wealthPanel, colour = saq01)) + geom_point()

tab(d$wlthSmooth, d$saq01)

# --- Shocks versus wealth index ---

# Extract only shocks; Melt data into a quasi-panel and filter by years to plot shocks
d.shocks <- as.data.frame(select(d, HID, assetShk, hazardShk, healthShk, wlthSmooth, priceShk, 
                                 year, saq01, region, femhead, agehead, religHoh, wealthPanel, ftfzone, wealthQuints))

names(d.shocks) <- c("HID", "Asset", "Hazard", "Health", "Wealth", "Price", "Year", "Region", "RegionLSMS",
                     'female', "age", "religion", "WealthIdx", "FtFZone", "Quintiles")

d.shocksm <- melt(d.shocks, id=c("HID", "Year", "Region", "age", "female", 
                                 "RegionLSMS", "Wealth", "religion", "WealthIdx", "FtFZone", "Quintiles"))

# Sort the data for plotting
d.shocksm$RegionLSMS <- factor(d.shocksm$RegionLSMS, levels = c("SNNP", "Other regions",  "Oromia", "Amhara", "Tigray"))

# Call colors for customization to match Laura's color schematics
### Price Shocks ###
brewer.pal(8, "Paired")

p <- ggplot(filter(d.shocksm, variable == "Price"), aes(x = Wealth, y = value, colour = factor(Year))) +
  facet_wrap(~RegionLSMS, ncol = 5) + 
  stat_smooth(method = "loess", alpha = 0.05, size = 1.15, span = 1.5) + 
  g.spec2 +   scale_x_discrete(breaks = c(seq(0, 10, by = 3)), labels=c("", "very poor","poor", "above average")) +
  labs(x = "Household Wealth Score ", y = "Percent of households with shock \n") + 
  ggtitle("Moderately asset poor households report the most price shocks") +
  scale_color_manual(values=brewer.pal(8, "Paired")[3:4])
p


### Health Shocks ###
d.shocksm$RegionLSMS <- factor(d.shocksm$RegionLSMS, levels = c("SNNP", "Oromia", "Amhara", "Other regions", "Tigray"))

p <- ggplot(filter(d.shocksm, variable == "Health"), aes(x = Wealth, y = value, colour = factor(Year))) +
  facet_wrap(~RegionLSMS, ncol = 5) + 
  stat_smooth(method = "loess", alpha = 0.05, size = 1.15, span = 1.5) + 
  g.spec2 + scale_x_discrete(breaks = c(seq(0, 10, by = 3)), labels=c("", "very poor","poor", "above average")) +
  labs(x = "Household Wealth Score ", y = "Percent of households with shock \n") + 
  ggtitle("Health shock occurences tend to decline with increases in asset holdings") +
  scale_color_manual(values=brewer.pal(12, "Paired")[9:10])
p

### Hazard Shocks ###
d.shocksm$RegionLSMS <- factor(d.shocksm$RegionLSMS, levels = c("SNNP", "Tigray", "Other regions", "Oromia", "Amhara"))

p <- ggplot(filter(d.shocksm, variable == "Hazard"), aes(x = Wealth, y = value, colour = factor(Year))) +
  facet_wrap(~RegionLSMS, ncol = 5) + 
  stat_smooth(method = "loess", alpha = 0.05, size = 1.15, span = 1.5) + 
  g.spec2 +   scale_x_discrete(breaks = c(seq(0, 10, by = 3)), labels=c("", "very poor","poor", "above average")) +
  labs(x = "Household Wealth Score ", y = "Percent of households with shock \n") + 
  ggtitle("Asset holdings and hazard shocks are inversely correlated and vary greatly across survey years") +
  scale_color_manual(values=brewer.pal(12, "Paired")[c(7,8)])
p

# NOTE: Not worth doing asset shocks as they are so low; Let's consider FTF v non-ftf zones
p <- ggplot(filter(d.shocksm, FtFZone != "Missing"), aes(x = Wealth, y = value, colour = variable)) +
  facet_grid(Year~RegionLSMS) + 
  stat_smooth(method = "loess", alpha = 0.00, size = 1.15, span = 1.5) + 
  g.spec2+ 
  #geom_jitter(alpha = 0.1, position = position_jitter(height=0.05)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), labels=c("", "very poor","poor", "above average")) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) + 
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Household Wealth Score ", y = "Households reporting shock \n") + 
  ggtitle("Shocks tend to decline as households obtain more assets") +
  scale_color_manual(values=brewer.pal(12, "Paired")[c(1, 7, 9, 3)])
p

p <- ggplot(filter(d.shocksm, religion != ""), aes(x = age, y = value, colour = variable)) +
  facet_grid(~RegionLSMS) + 
  stat_smooth(method = "loess", alpha = 0.1, size = 1.15, span = 1.5) + 
  g.spec2+
  #geom_jitter(alpha = 0.1, position = position_jitter(height=0.05)) +
  scale_x_discrete( limits = c(0, 80)) +
  #scale_y_continuous(limits = c(0,1)) + 
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Age of Household Head ", y = "Percent of households with shock \n") + 
  ggtitle("Households in or near Feed the Future Zones appear to face more price uncertainty") +
  scale_color_manual(values=brewer.pal(12, "Paired")[c(1, 7, 9, 3)])
p


# Look at how asset holdings varying by wealth categories; Focus on lab-centric technologies
# Extract only shocks; Melt data into a quasi-panel and filter by years to plot shocks
d.assets1 <- as.data.frame(select(d, HID, watch, phone, mobile, radio, tv, dvd, sat, wlthSmooth,
                                 year, saq01, region, femhead, agehead, religHoh, wealthPanel, ftfzone, wealthQuints))

names(d.assets1) <- c("HID", "watch", "phone", "mobile", "radio", "tv", "dvd", "sat", 
                     "Wealth", "Year", "Region", "RegionLSMS", 'female', "age", "religion", 
                     "WealthIdx", "FtFZone", "Quintiles")

d.assets1m <- melt(d.assets1, id=c("HID", "Year", "Region", "age", "female", 
                                 "RegionLSMS", "religion", "Wealth",
                                 "WealthIdx", "FtFZone", "Quintiles"))


p <- ggplot(filter(d.assets1m), aes(x = Wealth, y = value, colour = variable)) +
  facet_grid(Year~RegionLSMS) + 
  stat_smooth(method = "loess", alpha = 0.00, size = 1.15, span = 1.5) + 
  g.spec2+ 
  #geom_jitter(alpha = 0.1, position = position_jitter(height=0.05)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), labels=c("", "very poor","poor", "above average")) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Household Wealth Score ", y = "Households owning asset \n") + 
  ggtitle("Mobile phone ownership increased substantially from 2012 to 2014") +
  scale_color_manual(values=brewer.pal(12, "Paired"))
p

# Plot year-to-year change in phone ownership by age group across regions
p <- ggplot(filter(d.assets1m, variable == "mobile", Quintiles != "NA"), aes(x = age, y = value, colour = as.factor(Year))) + facet_grid(~ RegionLSMS) + 
  stat_smooth(method = "loess", alpha = 0.10, size = 1.15, span = 1.5) + 
  g.spec2 +  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Age of Household Head ", y = "Households owning asset \n") + 
  ggtitle("Even with the overall gains in mobile ownership, older households are less likely to own a phone") +
  scale_color_manual(values=brewer.pal(12, "Paired"))
p


# Agriculture Assets #
d.agassets <- as.data.frame(select(d, HID, ax, plough, cart, sickle, pump, well, wlthSmooth,
                                  year, saq01, region, femhead, agehead, religHoh, wealthPanel, ftfzone, wealthQuints))

names(d.agassets) <- c("HID", "ax", "plough", "cart", "sickle", "pump", "well", 
                      "Wealth", "Year", "Region", "RegionLSMS", 'female', "age", "religion", 
                      "WealthIdx", "FtFZone", "Quintiles")
d.assets1m <- melt(d.agassets, id=c("HID", "Year", "Region", "age", "female", 
                                   "RegionLSMS", "religion", "Wealth",
                                   "WealthIdx", "FtFZone", "Quintiles"))


p <- ggplot(filter(d.assets1m), aes(x = Wealth, y = value, colour = variable)) +
  facet_grid(Year~RegionLSMS) + 
  stat_smooth(method = "loess", alpha = 0.10, size = 1.15, span = 1.5) + 
  g.spec2+ 
  #geom_jitter(alpha = 0.1, position = position_jitter(height=0.05)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), labels=c("", "very poor","poor", "above average")) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Household Wealth Score ", y = "Households owning asset \n") + 
  ggtitle("Ownership of labor intensive agricultural assets declines with wealth") +
  scale_color_manual(values=brewer.pal(12, "Paired"))
p

# Household infrastructure
d.agassets <- as.data.frame(select(d, HID, mudHome, stoneHome, mudFloor, thatchRoof, 
                                   metalRoof, electricity, wlthSmooth, year, saq01, region, 
                                   femhead, agehead, religHoh, wealthPanel, ftfzone, wealthQuints))

names(d.agassets) <- c("HID", "mud home", "stone home", "mud floor", "thatch roof", "metal roof",
                       "electricity", "Wealth", "Year", "Region", "RegionLSMS", 'female', "age", 
                       "religion", "WealthIdx", "FtFZone", "Quintiles")
d.assets1m <- melt(d.agassets, id=c("HID", "Year", "Region", "age", "female", 
                                    "RegionLSMS", "religion", "Wealth",
                                    "WealthIdx", "FtFZone", "Quintiles"))















p <- ggplot(filter(d.shocksm, Year == 2014, age != "", religion != ""), aes(x = Wealth, y = value, colour = variable)) +
  facet_grid(Year~RegionLSMS) + 
  stat_smooth(method = "loess", alpha = 0.0, size = 1.15, span = 1.5) + 
  g.spec1 +
  #geom_jitter(alpha = 0.1, position = position_jitter(height=0.05)) +
  #scale_x_continuous(breaks = c(seq(0, 10, by = 3))) +
  #scale_y_continuous(limits = c(0,0.75)) + 
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Age of Household Head", y = "Percent of households with shock \n") +
  scale_color_brewer(palette="Set2")
p



# Extract only assets; Melt data into a quasi-panel and filter by years to plot shocks
d.assets <- as.data.frame(select(d, HID, radio, mobile, phone, watch, bike, moto, jewel, mudHome,
                                 thatchRoof, roomsPC, wlthSmooth, year, saq01, region, femhead, 
                                 agehead, religHoh, wealthPanel, agehead, wealthPanel))

names(d.assets) <- c("HID", "radio", "mobile", "phone", "watch", "bicycle", "motorcycle", "jewelry",
                     "mud home", "thatched roof", "Rooms per person", "Wealth", "Year",
                     "Region", "RegionLSMS", "female", "age", "religion", "WealthIdx")

d.assetsm <- melt(d.assets, id=c("HID", "Year", "Region", "age", "female", "RegionLSMS", "Wealth", "religion", "WealthIdx"))

# Sort the data for plotting
d.assetsm$RegionLSMS <- factor(d.assetsm$RegionLSMS, levels = c("SNNP", "Oromia", "Other regions", 
                                                                "Amhara", "Tigray"))

p <- ggplot(d.assetsm, aes(x = Wealth, y = value, colour = variable)) +
  facet_wrap(Region ~ Year, ncol = 5) +
  stat_smooth(method = "loess", alpha = 0.1, size = 1.15, span = 1.5) + 
  g.spec1 + theme_bw() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16, face = "bold"), 
        title = element_text(size = 18, face = "bold"), 
        strip.text = element_text(size=11)) +
  #geom_jitter(alpha = 0.1, position = position_jitter(height=0.05)) +
  scale_x_continuous(breaks = c(seq(0, 10, by = 3))) +
  #scale_y_continuous(limits = c(0,0.75)) + 
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Wealth Decile", y = "Percent of households owning asset \n") +
  scale_color_brewer(palette="Set3")
p

### --- Dietary Diveristy ---
d.fcs <- as.data.frame(select(d, HID, FCS, wlthSmooth, year, saq01, region, femhead, 
                              agehead, religHoh, wealthPanel, ftfzone))

names(d.fcs) <- c("HID", "Food Consumption Score", "Wealth", "Year", "Region", "RegionLSMS", "female",
                  "age", "religion", "WealthIdx", "ftfzone")

d.fcsm <- melt(d.fcs, id = c("HID", "Year", "Region", "age", "female", "RegionLSMS", "Wealth", "religion", "WealthIdx", "ftfzone"))

p <- ggplot(filter(d.fcsm, ftfzone != "Missing"), aes(x = Wealth, y = value, colour = factor(Year))) +
  facet_wrap(RegionLSMS ~ ftfzone, ncol = 5) +
  stat_smooth(method = "loess", alpha = 0.1, size = 1.15, span = 1.5) + 
  g.spec1 + theme_bw() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16, face = "bold"), 
        title = element_text(size = 18, face = "bold"), 
        strip.text = element_text(size=11)) +
  #geom_jitter(alpha = 0.1, position = position_jitter(height=0.05)) +
  scale_x_continuous(breaks = c(seq(0, 10, by = 3))) +
  #scale_y_continuous(limits = c(0,0.75)) + 
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Wealth Decile", y = "Percent of households owning asset \n") +
  scale_color_brewer(palette="Set3")
p
