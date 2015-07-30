# --- Plotting LSMS/RIGA panel data for map products
# Date: 2015.04
# By: Tim Essam, Phd
# For: USAID GeoCenter

# --- Clear workspace, set library list
remove(list = ls())
libs <- c ("reshape", "ggplot2", "dplyr", "RColorBrewer", "grid", "scales", "stringr", "directlabels", "gmodels", "reshape")

# --- Load required libraries
lapply(libs, require, character.only=T)

# --- Set working directory for home or away
# wd <- c("U:/UgandaPanel/Export/")
wdw <- c("C:/Users/Tim/Documents/Ethiopia/Export")
wdh <- c("C:/Users/t/Documents/Ethiopia/Export")
setwd(wdh)

# --- Read in as a dplyr data frame tbl
d <- tbl_df(read.csv("Panel.wealth.analysis.csv"))

# plot settings
# Create settings for fitting a smooth trendline
stat.set1 <- stat_smooth(method = "loess", size = 1, se = "TRUE", span = 1, alpha = 1)

# Set alpha settings (for transparency -- below 1 is not good for Adobe illustrator exports)
transp <- c(1)
dpi.out <- c(300)

# --- Shocks versus wealth index ---

# Extract only shocks; Melt data into a quasi-panel and filter by years to plot shocks
d.shocks <- as.data.frame(select(d, HID, assetShk, hazardShk, healthShk, wlthSmooth, priceShk, 
                                 year, saq01, region, femhead, agehead, religHoh, wealthPanel))

names(d.shocks) <- c("HID", "Asset", "Hazard", "Health", "Wealth", "Price", "Year", "Region", "RegionLSMS",
                     'female', "age", "religion", "WealthIdx")

d.shocksm <- melt(d.shocks, id=c("HID", "Year", "Region", "age", "female", "RegionLSMS", "Wealth", "religion", "WealthIdx"))

# Sort the data for plotting
d.shocksm$RegionLSMS <- factor(d.shocksm$RegionLSMS, levels = c("SNNP", "Oromia", "Other regions", 
                                                          "Amhara", "Tigray"))

p <- ggplot(filter(d.shocksm, Year == 2012), aes(x = Wealth, y = value, colour = variable)) +
  facet_wrap(~RegionLSMS, ncol = 5) + 
  stat_smooth(method = "loess", alpha = 0.1, size = 1.15, span = 1.5) + 
  g.spec1 +
  #geom_jitter(alpha = 0.1, position = position_jitter(height=0.05)) +
  scale_x_continuous(breaks = c(seq(0, 10, by = 3))) +
  #scale_y_continuous(limits = c(0,0.75)) + 
  #geom_hline(yintercept = 0.5, linetype = "dotted", size = 1, alpha = transp) +
  labs(x = "Wealth Decile", y = "Percent of households with shock \n") +
  scale_color_brewer(palette="Set2")
p


p <- ggplot(filter(d.shocksm, Year == 2014, age != "", religion != ""), aes(x = Wealth, y = value, colour = variable)) +
  facet_wrap(~RegionLSMS, ncol = 5) + 
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
