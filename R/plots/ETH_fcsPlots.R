source("~/GitHub/Ethiopia/R/loadETHpanel.r")


# Set colors --------------------------------------------------------------
poorThresh = 21 # FCS "poor" categorisation
borderlineThresh = 35 # FCS "borderline" cutoff
avgColor = '#ce1256'
fillColor = '#fee080'
colText = '#662506'


theme_leftTitle = function() {
  theme_bw() + 
    theme(axis.text = element_text(size = 14, hjust = 0.5),
          axis.title =  element_text(size = 16, face = "bold", hjust = 0.5),
          title =  element_text(size = 18, face = "bold", hjust = 0),
          strip.text = element_text(size=12)) 
}

theme_blankBox = function() {
  theme_bw() + 
    theme(axis.text = element_text(size = 14, hjust = 0.5),
          axis.title =  element_text(size = 16, face = "bold", hjust = 0.5),
          title =  element_text(size = 18, face = "bold", hjust = 0),
          strip.text = element_text(size=12)) 
}

setwd("~/GitHub/Ethiopia/")

# Load data ---------------------------------------------------------------
library(grid)
library(ggplot2)
library(animation)

source("R/food security module/setupFncns.r")
hhRaw = read_dta("Data/ETH_201507_LSMS_All.dta") 

hhPanel = read_dta("~/Documents/USAID/Ethiopia/Dataout/ETH_201508_analysis_panel.dta")
hhPanel = removeAttributes(hhPanel) %>% 
  mutate(regionName = ifelse(
    saq01 == 2 , "Afar",
    ifelse(saq01 == 5, "Somalie", 
           ifelse(saq01 == 6, "Benshagul Gumuz",
                  ifelse(saq01 == 12, "Gambella",
                         ifelse(saq01 == 13, "Harari",
                                ifelse(saq01 == 15, "Diredawa",
                                       ifelse(saq01 == 1, "Tigray",
                                              ifelse(saq01 == 3, "Amhara",
                                                     ifelse(saq01 == 4, "Oromia",
                                                            ifelse(saq01 == 7, "SNNP", 
                                                                   ifelse(saq01 == 14, "Addis Ababa", "unknown")
                                                            )))))))))))

hh =  removeAttributes(hhRaw) %>% 
  mutate(stuntingHH = stunting, underweightHH = underweight, wastingHH = wasting, BMIhh = BMI, numChildHH = childTag,
         regionName = ifelse(
           saq01 == 2 , "Afar",
           ifelse(saq01 == 5, "Somalie", 
                  ifelse(saq01 == 6, "Benshagul Gumuz",
                         ifelse(saq01 == 12, "Gambella",
                                ifelse(saq01 == 13, "Harari",
                                       ifelse(saq01 == 15, "Diredawa",
                                              ifelse(saq01 == 1, "Tigray",
                                                     ifelse(saq01 == 3, "Amhara",
                                                            ifelse(saq01 == 4, "Oromia",
                                                                   ifelse(saq01 == 7, "SNNP", 
                                                                          ifelse(saq01 == 14, "Addis Ababa", "unknown")
                                                                   ))))))))))) %>% 
  select(-stunting, -underweight, -wasting, -BMI, -X_merge, -childTag)

hhPanel = hh %>% 
  filter(ptrack == 2)

# Select years, and only the panel data (hh in both years).
hh12 = hh %>% 
  filter(year == 2012, ptrack == 2)

hh14 = hh %>% 
  filter(year == 2014, ptrack == 2)

# FCS01: FCS by region, 2012 ---------------------------------------------------------------
regionOrder = hh12 %>% group_by(regionName) %>% summarise(avg = mean(fcsMin), n = n()) %>% arrange(avg)

# Reorder from low mean FCS to high (2012)
hh12$regionName = factor(hh12$regionName, regionOrder$regionName[1:11])

annot = cbind(regionOrder, xmin = 0, xBorderlineMax= borderlineThresh,
  ymin = 0, yBorderlineMax = c(27, 15, 10, 20, 7, 12,7,7,25,8))


# National FCS value.
avgFCS12 = mean(hh12$fcsMin, na.rm = TRUE)

yBox = 0.05
yText = 0.045



ggplot(hh12, aes(x = fcsMin)) +
  annotate("rect", xmin = 0, xmax = borderlineThresh, ymin = 0, 
           ymax = yBox, alpha = 0.2)  +
  annotate("rect", xmin = 0, xmax = poorThresh, ymin = 0, 
           ymax = yBox, alpha = 0.2)  +
  annotate("text", x = poorThresh/2, y = yText, label = "poor", 
           size = 4, colour =  "#545454") +
  annotate("text", x = (112 - borderlineThresh)/2 +
             borderlineThresh, y = yText, label = "acceptable", 
           size = 4, colour =  "#545454") +
  #   annotate("text", x = FCSavg2014$avg + 30, y = 32, 
  #            label = "Ethiopia mean", colour = avgColor, size = 3) +
  geom_vline(xintercept = avgFCS14, colour = avgColor, linetype = 2, size = 0.75) +
  geom_density(alpha = 0.4, fill = fillColor) +
  ggtitle("Food Consumption scores (2012)") +
  ylab("percent of households") +
  xlab("food consumption score") +
  theme_leftTitle() +
  theme(          strip.background = element_blank(),
                  strip.text = element_text(colour = colText),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 0.04, by = 0.02), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~regionName, scales = "free_y", ncol = 2)


# FCS02: FCS by region, 2014 ---------------------------------------------------------------
avgFCS14 = mean(hh14$fcsMin, na.rm = TRUE)

# Reorder from low mean FCS to high (2012)
hh14$regionName = factor(hh14$regionName, regionOrder$regionName[1:11])

ggplot(hh14, aes(x = fcsMin)) +
  annotate("rect", xmin = 0, xmax = borderlineThresh, ymin = 0, 
           ymax = yBox, alpha = 0.2)  +
  annotate("rect", xmin = 0, xmax = poorThresh, ymin = 0, 
           ymax = yBox, alpha = 0.2)  +
  annotate("text", x = poorThresh/2, y = yText, label = "poor", 
           size = 4, colour =  "#545454") +
  annotate("text", x = (112 - borderlineThresh)/2 +
             borderlineThresh, y = yText, label = "acceptable", 
           size = 4, colour =  "#545454") +
  #   annotate("text", x = FCSavg2014$avg + 30, y = 32, 
  #            label = "Ethiopia mean", colour = avgColor, size = 3) +
  geom_vline(xintercept = avgFCS14, colour = avgColor, linetype = 2, size = 0.75) +
  geom_density(alpha = 0.4, fill = fillColor) +
  ggtitle("Food Consumption scores (2014)") +
  ylab("percent of households") +
  xlab("food consumption score") +
  theme_leftTitle() +
  theme(          strip.background = element_blank(),
                  strip.text = element_text(colour = colText),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 0.04, by = 0.02), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~regionName, scales = "free_y", ncol = 2)



# FCS03: FCS by year ------------------------------------------------------
colLower = '#b2182b'
colHigher = '#2166ac'

hhPanel = data %>% 
  mutate(fcsCat = ifelse(fcsMin < 35, "not acceptable",
                         ifelse(fcsMin >= 35, "acceptable", NA)))

fcsYear = hhPanel %>% 
  group_by(regionName, year, fcsCat) %>% 
  summarise(num = n()) %>%  
  mutate(pct = num/sum(num)) %>% 
  select(-num)

fcsYear = spread(fcsYear, year, pct) %>% 
  mutate( 
         change = `2014` - `2012`) %>% 
  mutate(chg = ifelse(change < 0, -1, 1)) %>% 
  mutate(chgCorr = chg*c(1, -1)) %>% 
  # mutate(chgCorr = chg*c(1, -1, -1)) %>% 
  mutate(colors = ifelse(chgCorr < 0,
                    colLower, colHigher),
         maxYr = ifelse(`2014` > `2012`, `2014`, `2012`))


fcsYear$regionName = factor(fcsYear$regionName,
  (fcsYear %>% filter(fcsCat == 'acceptable')  %>% arrange(`2014`))$regionName)

plotFCSYr = function(fcsYear,
                     col12 = '#969696', col14 = '#525252',
                     ncol = 2) {
  ggplot(fcsYear) +
    geom_segment(aes(x = fcsCat, xend= fcsCat, 
                     y = `2012`, yend = `2014`, colour = colors), 
                 size = 2) +
    scale_colour_identity()+
    geom_point(aes(x = fcsCat, y = `2012`), 
               colour = col12, size = 6) +
    geom_point(aes(x = fcsCat, y = `2014`, colour = colors), 
                size = 6) +
    geom_text(aes(x = fcsCat, y = maxYr + 0.05, label = percent(`2014`,0))) +
    facet_wrap(~ regionName, ncol = ncol) +
    xlab("food consumption score") +
    theme_box_ygrid() +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks = element_blank(), 
          strip.background = element_blank(),
          panel.border = element_rect(color = 'grey', size = 0.2),
          axis.text.x = element_text(angle = 0)
#           panel.margin = unit(2, "lines"),
#           rect = element_blank()
)
}



plotFCSYr(fcsYear, ncol = 10)

# FCS04: FtF v. non-FtF ---------------------------------------------------
fcsFtF = hhPanel %>% 
  group_by(ftfzone, year, fcsCat) %>% 
  summarise(num = n()) %>%  
  mutate(pct = num/sum(num)) %>% 
  select(-num)

fcsFtF = spread(fcsFtF, year, pct) %>% 
  mutate( 
    change = `2014` - `2012`) %>% 
  mutate(chg = ifelse(change < 0, -1, 1)) %>% 
  mutate(chgCorr = chg*c(1, -1, -1)) %>% 
  mutate(colors = ifelse(chgCorr < 0,
                         colLower, colHigher),
         maxYr = ifelse(`2014` > `2012`, `2014`, `2012`)) %>% 
  filter(!is.na(ftfzone))




ggplot(fcsFtF) +
  geom_segment(aes(x = fcsCat, xend= fcsCat, 
                   y = `2012`, yend = `2014`, colour = colors), 
               size = 2) +
  scale_colour_identity()+
  geom_point(aes(x = fcsCat, y = `2012`), 
             colour = col12, size = 6) +
  geom_point(aes(x = fcsCat, y = `2014`, colour = colors), 
             size = 6) +
  geom_text(aes(x = fcsCat, y = maxYr + 0.05, label = percent(`2014`,0))) +
  facet_wrap(~ ftfzone, ncol = 2) +
  xlab("food consumption score") +
  theme_blankBox() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 0)
        #           panel.margin = unit(2, "lines"),
        #           rect = element_blank()
  )

# FCS05: ethnicity --------------------------------------------------------

# FCS by religion ---------------------------------------------------------



# FCS vs. perceptions -----------------------------------------------------


# FCS by female HoH -------------------------------------------------------




# FCS heatmaps ------------------------------------------------------------
# Load in food groups
# setwd("~/Documents/USAID/Ethiopia/")
# source("R/nutrition analysis/EthiopiaFCS2014.R")


# regions = hhPanel %>% 
#   select(household_id, household_id2, dietDiv, year, regionName, fcsMin, religHoh)
# 
# regions = regions %>% 
#   mutate(religion = ifelse(religHoh == 1, 
#                            "Orthodox",
# #                            ifelse(religHoh == 2,
# #                                   "Catholic",
#                                   ifelse(religHoh == 3,
#                                          "Protestant",
#                                          ifelse(religHoh == 4,
#                                                 "Muslim",
# # #                                                 ifelse(religHoh == 5,
# # #                                                        "Traditional",
# #                                                        ifelse(religHoh == 6,
# #                                                               "Pagan",
#                                                               ifelse(religHoh == 7 | religHoh == 2 | religHoh == 6 | religHoh == 5,
#                                                                      "other", NA)))))
# 
# fcsReg = left_join(regions, hhAggr2014, by = c("household_id2" = "hhID2014"))
# fcsReg = fcsReg %>% 
#   filter(year == 2014)

fcsReg = data

# weights = data.frame(cereals = 2, pulses = 3, veg = 1, fruit = 1, meat = 4,
#                      milk = 4, sugar = 0.5, oil = 0.5)

fcs2014_heat = fcsReg %>% 
  group_by(regionName) %>% 
  summarise(starches = mean(cereal_days) * 2,
            oils = mean(oil_days) * 0.5,
            pulses = mean(legumes_days) * 3,
            sugar = mean(sweet_days) * 0.5, 
            vegetables = mean(veg_days) * 1,
            dairy = mean(milk_days) * 4,
            meat = mean(meat_days) * 4, 
            fruits  = mean(fruit_days) * 1, 
            fcs = mean(fcsMin),
            `dietary diversity` = mean(dietDiv, na.rm = TRUE)) %>% 
  arrange(desc(fcs))


# fcs2014_relig_heat = fcsReg %>% 
#   filter(!is.na(religion)) %>% 
#   group_by(religion) %>% 
#   summarise(starches = mean(cerealsMin) * weights$cereals,
#             oils = mean(oil) * 0.5,
#             pulses = mean(pulses) * 3,
#             sugar = mean(sugar) * 0.5, 
#             vegetables = mean(veg) * 1,
#             dairy = mean(milk) * 4,
#             meat = mean(proteinMin) * 4, 
#             fruits  = mean(fruit) * 1, 
#             fcs = mean(fcsMin),
#             `dietary diversity` = mean(dietDiv, na.rm = TRUE),
#             num = n()) %>% 
#   arrange(desc(`dietary diversity`))


fcs_avg = fcsReg %>% 
  summarise(starches = mean(cereal_days) * 2,
            oils = mean(oil_days) * 0.5,
            pulses = mean(legumes_days) * 3,
            sugar = mean(sweet_days) * 0.5, 
            vegetables = mean(veg_days) * 1,
            dairy = mean(milk_days) * 4,
            meat = mean(meat_days) * 4, 
            fruits  = mean(fruit_days) * 1, 
            fcs = mean(fcsMin),
            `dietary diversity` = mean(dietDiv, na.rm = TRUE)) %>% 
  arrange(desc(fcs))


rel_fcs2014_heat = fcs2014_heat %>% 
  mutate(starches = starches - fcs_avg$starches,
         oils = oils - fcs_avg$oils,
         pulses = pulses - fcs_avg$pulses,
         sugar = sugar - fcs_avg$sugar,
         vegetables = vegetables - fcs_avg$vegetables,
         dairy = dairy - fcs_avg$dairy,
         meat = meat - fcs_avg$meat,
         fruits  = fruits - fcs_avg$fruits)


# rel_fcs2014_relig_heat = fcs2014_relig_heat %>% 
#   mutate(starches = starches - fcs_avg$starches,
#          oils = oils - fcs_avg$oils,
#          pulses = pulses - fcs_avg$pulses,
#          sugar = sugar - fcs_avg$sugar,
#          vegetables = vegetables - fcs_avg$vegetables,
#          dairy = dairy - fcs_avg$dairy,
#          meat = meat - fcs_avg$meat,
#          fruits  = fruits - fcs_avg$fruits)

# 
# setwd("~/GitHub/Ethiopia/Python/")
# write.csv(fcs2014_heat, 'fcs2014_heat.csv')
# write.csv(rel_fcs2014_heat, 'rel_fcs2014_heat.csv')
# write.csv(fcs2014_relig_heat, 'fcs2014_relig_heat.csv')
# write.csv(rel_fcs2014_relig_heat, 'rel_fcs2014_relig_heat.csv')


# -- plot --
widthDDheat = 3.25
heightDDheat = 1.65
widthDDavg = 2.25

fcsRange = c(30, 50)

fcsOrder = rev(rel_fcs2014_heat$regionName)

rel_fcs2014_heat = rel_fcs2014_heat %>% 
  gather(food, rel_mean, -regionName, -fcs, -`dietary diversity`)

rel_fcs2014_heat$regionName = 
  factor(rel_fcs2014_heat$regionName,
         fcsOrder)


# Main heatmap
ggplot(rel_fcs2014_heat) +
  geom_tile(aes(x = food, y = regionName, fill = rel_mean), 
            color = 'white', size = 0.3) +
  scale_fill_gradientn(colours = PlBl, 
                       limits = c(-15.5,15.5)) +
  # geom_text(aes(y = food, x = regionName, label = round(rel_mean,1)), size = 4) +
  ggtitle('FCS, relative to the national average, 2014') +
  theme_blankLH() +
  theme(
    axis.text = element_text(size = 6, color = softBlack),
    title =  element_text(size = 8, face = "bold", hjust = 0, color = softBlack),
    legend.position = 'right',
    legend.text  = element_text(size = 4, color = softBlack),
    legend.key.width = unit(0.05, 'inch'),
    legend.key.height = unit(0.15, 'inch')
  )


ggsave("~/GitHub/Ethiopia/R/plots/ETH_fcsHeat14.pdf", 
       width = widthDDheat, height = heightDDheat,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Side heatmap w/ dietary diversity score.

ggplot(rel_fcs2014_heat) +
  geom_tile(aes(x = 1, y = regionName, fill = fcs), 
            color = 'white', size = 0.3) +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu'), 
                       name = 'food consumption score', limits = fcsRange) +
  geom_text(aes(x = 1, y = regionName, label = round(fcs,0)), size = 2,
            colour = 'white') +
  ggtitle('FCS, relative to the national average, 2014') +
  theme_blankLH() +
  theme(
    axis.text = element_text(size = 6, color = softBlack),
    title =  element_text(size = 8, face = "bold", hjust = 0, color = softBlack),
    legend.position = 'right',
    legend.text  = element_text(size = 4, color = softBlack),
    legend.key.width = unit(0.05, 'inch'),
    legend.key.height = unit(0.15, 'inch')
  )

ggsave("~/GitHub/Ethiopia/R/plots/ETH_fcsavg14.pdf", 
       width = widthDDavg, height = heightDDheat,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)




#How many hh change their FCS category? ---------------------------



# FCS GIF 2014 ------------------------------------------------------------

yBox = 0.052
yText = 0.05

regionOrder = hh14 %>% 
  group_by(regionName) %>% 
  summarise(avg = mean(fcsMin), n = n()) %>% 
  arrange(avg)


avgFCS14 = mean(hh14$fcsMin, na.rm = TRUE)

# Reorder from low mean FCS to high (2012)
hh14$regionName = factor(hh14$regionName, regionOrder$regionName[1:11])

saveGIF({
  for (i in 1:10) {
    data = hh14 %>%
      filter(regionName == regionOrder$regionName[i])
    
    
    x=ggplot(data, aes(x = fcsMin)) +
      annotate("rect", xmin = 0, xmax = borderlineThresh, ymin = 0, 
               ymax = yBox, alpha = 0.2)  +
      annotate("rect", xmin = 0, xmax = poorThresh, ymin = 0, 
               ymax = yBox, alpha = 0.2)  +
      annotate("text", x = poorThresh/2, y = yText, label = "poor", 
               size = 5.5, colour =  "#545454") +
      annotate("text", x = (112 - borderlineThresh)/2 +
                 borderlineThresh, y = yText, label = "acceptable", 
               size = 5.5, colour =  "#545454") +
      annotate("text", x = 85, y = 0.035, 
               label = paste0('average: ', round(mean(data$fcsMin),1)), 
               colour = avgColor, size = 6) +
      geom_vline(xintercept = avgFCS14, colour = avgColor, linetype = 2, size = 0.75) +
      geom_density(alpha = 0.4, fill = fillColor) +
      ggtitle(regionOrder$regionName[i]) +
      ylab("percent of households") +
      xlab("food consumption score") +
      theme_leftTitle() +
      coord_cartesian(xlim = c(0, 112))+
      theme(          strip.background = element_blank(),
                      strip.text = element_text(colour = colText),
                      panel.grid.minor.y = element_blank(),
                      panel.grid.major.y = element_blank()) +
      scale_y_continuous(breaks = seq(0, 0.04, by = 0.02), expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0))
  }
}, movie.name = "fcs2014.gif")




# Foods as a function of wealth -------------------------------------------------------------------

# Combined years, panel data.

colorLeg = brewer.pal(12, 'Paired')[3]
colorCer = brewer.pal(12, 'Paired')[8]
colorVeg = brewer.pal(12, 'Paired')[4]
colorFruit = brewer.pal(12, 'Paired')[6]
colorMeat = brewer.pal(12, 'Paired')[5]
colorMilk = brewer.pal(12, 'Paired')[2]
colorSweet = brewer.pal(12, 'Paired')[9]
colorStarch = brewer.pal(12, 'Paired')[12]
colorOil = brewer.pal(12, 'Paired')[7]
colorEggs = brewer.pal(12, 'Paired')[1]
colorSpices = '#662506'

xAnnot = 10.1

ggplot(hhPanel, aes(x = wlthSmooth)) + 
  geom_smooth(aes(y = legumes_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorLeg, method = 'loess')+
  #   geom_smooth(aes(y = fish_days), data = hhPanel, 
  #               alpha = 0.2, size = 2, colour= 'salmon', method = 'loess')+
  geom_smooth(aes(y = cereal_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorCer, method = 'loess')+
  geom_smooth(aes(y = milk_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorMilk, method = 'loess')+
  geom_smooth(aes(y = eggs_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorEggs, method = 'loess')+
  geom_smooth(aes(y = veg_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorVeg, method = 'loess')+
  geom_smooth(aes(y = starch_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorStarch, method = 'loess')+
  geom_smooth(aes(y =  fruit_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorFruit, method = 'loess')+
  geom_smooth(aes(y =  meat2_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorMeat, method = 'loess')+
  geom_smooth(aes(y =  sweet_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorSweet, method = 'loess')+
  geom_smooth(aes(y =  oil_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorOil, method = 'loess')+
  geom_smooth(aes(y =  cond_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorSpices, method = 'loess')+
  annotate('text', y = 6.45, x = xAnnot, label = 'cereals', color = colorCer, size = 5, hjust = 0) +
  annotate('text', y = 6.6, x = xAnnot, label = 'condiments', color = colorSpices, size = 5, hjust = 0) +
  annotate('text', y = 6.8, x = xAnnot, label = 'oils', color = colorOil, size = 5, hjust = 0) +
  annotate('text', y = 4.9, x = xAnnot, label = 'sweets', color = colorSweet, size = 5, hjust = 0) +
  annotate('text', y = 3.3, x = xAnnot, label = 'legumes', color = colorLeg, size = 5, hjust = 0) +
  annotate('text', y = 2.7, x = xAnnot, label = 'starch', color = colorStarch, size = 5, hjust = 0) +
  annotate('text', y = 2.5, x = xAnnot, label = 'vegetables', color = colorVeg, size = 5, hjust = 0) +
  annotate('text', y = 1.9, x = xAnnot, label = 'milk', color = colorMilk, size = 5, hjust = 0) +
  annotate('text', y = 1.75, x = xAnnot, label = 'meat', color = colorMeat, size = 5, hjust = 0) +
  annotate('text', y = 1., x = xAnnot, label = 'fruit', color = colorFruit, size = 5, hjust = 0) +
  annotate('text', y = 0.8, x = xAnnot, label = 'eggs', color = colorEggs, size = 5, hjust = 0) +
  theme_jointplot()+ 
  coord_cartesian(ylim = c(0,7), xlim = c(1,11)) +
  ggtitle('Food consumption increases as wealth increases')+
  xlab('wealth index') +
  ylab('number of days consumed') +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = 'grey', size = 0.5),
        panel.grid.minor.y = element_line(colour = 'grey', size = 0.25)
  )  +facet_wrap(~ftfzone)

# Foods by wealth by region -----------------------------------------------


hhPanel$regionName = factor(hhPanel$regionName, c(             'SNNP' ,
                                                             'Tigray' ,
                                                             'Amhara' ,
                                                    'Benshagul Gumuz' ,
                                                           'Gambella' ,
                                                               'Afar' ,
                                                             'Harari' ,
                                                             'Oromia' ,
                                                            'Somalie' ,
                                                          'Diredawa'))

ggplot(hhPanel, aes(x = wlthSmooth)) + 
  geom_smooth(aes(y = legumes_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorLeg, method = 'loess')+
#   geom_smooth(aes(y = fish_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= 'salmon', method = 'loess')+
#   geom_smooth(aes(y = cereal_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorCer, method = 'loess')+
  geom_smooth(aes(y = milk_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorMilk, method = 'loess')+
#   geom_smooth(aes(y = eggs_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorEggs, method = 'loess')+
  geom_smooth(aes(y = veg_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorVeg, method = 'loess')+
  geom_smooth(aes(y = starch_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorStarch, method = 'loess')+
#   geom_smooth(aes(y =  fruit_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorFruit, method = 'loess')+
  geom_smooth(aes(y =  meat2_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorMeat, method = 'loess')+
  geom_smooth(aes(y =  sweet_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorSweet, method = 'loess')+
#   geom_smooth(aes(y =  oil_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorOil, method = 'loess')+
#   geom_smooth(aes(y =  cond_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorSpices, method = 'loess')+
#   annotate('rect', xmin = 1, xmax = 1.2, ymin = 0, ymax = 7.05, fill = 'white', colour = 'white', hjust = 0) +
#   annotate('text', y = 6.1, x = xAnnot, label = 'cereals', color = colorCer, size = 5, hjust = 0) +
#   annotate('text', y = 6.5, x = xAnnot, label = 'condiments', color = colorSpices, size = 5, hjust = 0) +
#   annotate('text', y = 6.8, x = xAnnot, label = 'oils', color = colorOil, size = 5, hjust = 0) +
#   annotate('text', y = 4.75, x = xAnnot, label = 'sweets', color = colorSweet, size = 5, hjust = 0) +
#   annotate('text', y = 4.1, x = xAnnot, label = 'legumes', color = colorLeg, size = 5, hjust = 0) +
#   annotate('text', y = 2.7, x = xAnnot, label = 'starch', color = colorStarch, size = 5, hjust = 0) +
#   annotate('text', y = 2.5, x = xAnnot, label = 'vegetables', color = colorVeg, size = 5, hjust = 0) +
#   annotate('text', y = 2.3, x = xAnnot, label = 'milk', color = colorMilk, size = 5, hjust = 0) +
#   annotate('text', y = 1.7, x = xAnnot, label = 'meat', color = colorMeat, size = 5, hjust = 0) +
#   annotate('text', y = 1., x = xAnnot, label = 'fruit', color = colorFruit, size = 5, hjust = 0) +
#   annotate('text', y = 0.8, x = xAnnot, label = 'eggs', color = colorEggs, size = 5, hjust = 0) +
  facet_wrap(~ regionName) +
  theme_jointplot()+ 
  coord_cartesian(ylim = c(0,7), xlim = c(-0.5,11)) +
  ggtitle('Food consumption increases as wealth increases')+
  xlab('wealth index') +
  ylab('number of days consumed') +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = 'grey', size = 0.5),
        panel.grid.minor.y = element_line(colour = 'grey', size = 0.25)
  ) 

# Foods as a function of FCS -------------------------------------------------------------------

# Combined years, all panel data, all country

xAnnot = 113

ggplot(hhPanel, aes(x = fcsMin)) + 
  geom_smooth(aes(y = legumes_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorLeg, method = 'loess')+
  #   geom_smooth(aes(y = fish_days), data = hhPanelPanel, 
  #               alpha = 0.2, size = 2, colour= 'salmon', method = 'loess')+
  geom_smooth(aes(y = cereal_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorCer, method = 'loess')+
  geom_smooth(aes(y = milk_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorMilk, method = 'loess')+
  geom_smooth(aes(y = eggs_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorEggs, method = 'loess')+
  geom_smooth(aes(y = veg_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorVeg, method = 'loess')+
  geom_smooth(aes(y = starch_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorStarch, method = 'loess')+
  geom_smooth(aes(y =  fruit_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorFruit, method = 'loess')+
  geom_smooth(aes(y =  meat2_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorMeat, method = 'loess')+
  geom_smooth(aes(y =  sweet_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorSweet, method = 'loess')+
  geom_smooth(aes(y =  oil_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorOil, method = 'loess')+
  geom_smooth(aes(y =  cond_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorSpices, method = 'loess')+
  annotate('text', y = 6.5, x = xAnnot, label = 'cereals', color = colorCer, size = 5, hjust = 0) +
  annotate('text', y = 7.1, x = xAnnot, label = 'condiments', color = colorSpices, size = 5, hjust = 0) +
  annotate('text', y = 6.9, x = xAnnot, label = 'oils', color = colorOil, size = 5, hjust = 0) +
  annotate('text', y = 4.75, x = xAnnot, label = 'sweets', color = colorSweet, size = 5, hjust = 0) +
  annotate('text', y = 4.3, x = 23, label = 'legumes', color = colorLeg, size = 5, hjust = 0) +
  annotate('text', y = 4, x = xAnnot, label = 'starch', color = colorStarch, size = 5, hjust = 0) +
  annotate('text', y = 5.5, x = xAnnot, label = 'vegetables', color = colorVeg, size = 5, hjust = 0) +
  annotate('text', y = 5.7, x = xAnnot, label = 'milk', color = colorMilk, size = 5, hjust = 0) +
  annotate('text', y = 7.25, x = xAnnot, label = 'meat', color = colorMeat, size = 5, hjust = 0) +
  annotate('text', y = 3.5, x = xAnnot, label = 'fruit', color = colorFruit, size = 5, hjust = 0) +
  annotate('text', y = 5.3, x = xAnnot, label = 'eggs', color = colorEggs, size = 5, hjust = 0) +
  theme_jointplot()+ 
  coord_cartesian(ylim = c(0,7.3), xlim = c(0,135)) +
  ggtitle('oils, condiments, and cereals are staples')+
  xlab('Food Consumption Score') +
  ylab('number of days consumed') +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = 'grey', size = 0.5),
        panel.grid.minor.y = element_line(colour = 'grey', size = 0.25)
  )


# Foods as a function of FCS, region -------------------------------------------------------------------

# Combined years, all panel data

xAnnot = 113

ggplot(hhPanel, aes(x = fcsMin)) + 
  geom_smooth(aes(y = legumes_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorLeg, method = 'loess')+
  geom_smooth(aes(y = cereal_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorCer, method = 'loess')+
  geom_smooth(aes(y = milk_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorMilk, method = 'loess')+
#   geom_smooth(aes(y = eggs_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorEggs, method = 'loess')+
  geom_smooth(aes(y = veg_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorVeg, method = 'loess')+
  geom_smooth(aes(y = starch_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorStarch, method = 'loess')+
#   geom_smooth(aes(y =  fruit_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorFruit, method = 'loess')+
  geom_smooth(aes(y =  meat2_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorMeat, method = 'loess')+
  geom_smooth(aes(y =  sweet_days), data = hhPanel, 
              alpha = 0.2, size = 2, colour= colorSweet, method = 'loess')+
#   geom_smooth(aes(y =  oil_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorOil, method = 'loess')+
#   geom_smooth(aes(y =  cond_days), data = hhPanel, 
#               alpha = 0.2, size = 2, colour= colorSpices, method = 'loess')+
#   annotate('text', y = 6.5, x = xAnnot, label = 'cereals', color = colorCer, size = 5, hjust = 0) +
#   # annotate('text', y = 7.1, x = xAnnot, label = 'condiments', color = colorSpices, size = 5, hjust = 0) +
#   annotate('text', y = 6.9, x = xAnnot, label = 'oils', color = colorOil, size = 5, hjust = 0) +
#   annotate('text', y = 4.75, x = xAnnot, label = 'sweets', color = colorSweet, size = 5, hjust = 0) +
#   annotate('text', y = 4.3, x = 23, label = 'legumes', color = colorLeg, size = 5, hjust = 0) +
#   annotate('text', y = 4, x = xAnnot, label = 'starch', color = colorStarch, size = 5, hjust = 0) +
#   annotate('text', y = 5.5, x = xAnnot, label = 'vegetables', color = colorVeg, size = 5, hjust = 0) +
#   annotate('text', y = 5.7, x = xAnnot, label = 'milk', color = colorMilk, size = 5, hjust = 0) +
#   annotate('text', y = 7.25, x = xAnnot, label = 'meat', color = colorMeat, size = 5, hjust = 0) +
#   annotate('text', y = 3.5, x = xAnnot, label = 'fruit', color = colorFruit, size = 5, hjust = 0) +
#   annotate('text', y = 5.3, x = xAnnot, label = 'eggs', color = colorEggs, size = 5, hjust = 0) +
  theme_jointplot()+ 
  coord_cartesian(ylim = c(0,7.3), xlim = c(0,112)) +
  ggtitle('regions in the east consume more milk')+
  xlab('Food Consumption Score') +
  ylab('number of days consumed') +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = 'grey', size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = 'grey', size = 0.5),
        panel.grid.minor.y = element_line(colour = 'grey', size = 0.25)
  ) +
  facet_wrap(~ regionName)


# Foods by FtF zone, wealth -------------------------------------------------------

xAnnot = 10.1

hhFtf = hhPanel %>% 
  mutate(ftfzone = ifelse(ftfzone == 0, 
                          'non-Feed the Future zone',
                          ifelse(ftfzone == 1,
                                 'Feed the Future zone',
                                 NA))) %>% 
  filter(!is.na(ftfzone))

ggplot(hhFtf, aes(x = wlthSmooth)) + 
  geom_smooth(aes(y = legumes_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorLeg, method = 'loess')+
  #   geom_smooth(aes(y = fish_days), data = hhFtf, 
  #               alpha = 0.2, size = 2, colour= 'salmon', method = 'loess')+
  geom_smooth(aes(y = cereal_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorCer, method = 'loess')+
  geom_smooth(aes(y = milk_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorMilk, method = 'loess')+
  geom_smooth(aes(y = eggs_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorEggs, method = 'loess')+
  geom_smooth(aes(y = veg_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorVeg, method = 'loess')+
  geom_smooth(aes(y = starch_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorStarch, method = 'loess')+
  geom_smooth(aes(y =  fruit_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorFruit, method = 'loess')+
  geom_smooth(aes(y =  meat2_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorMeat, method = 'loess')+
  geom_smooth(aes(y =  sweet_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorSweet, method = 'loess')+
  geom_smooth(aes(y =  oil_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorOil, method = 'loess')+
  geom_smooth(aes(y =  cond_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorSpices, method = 'loess')+
  annotate('text', y = 6.45, x = xAnnot, label = 'cereals', color = colorCer, size = 5, hjust = 0) +
  annotate('text', y = 6.6, x = xAnnot, label = 'condiments', color = colorSpices, size = 5, hjust = 0) +
  annotate('text', y = 6.8, x = xAnnot, label = 'oils', color = colorOil, size = 5, hjust = 0) +
  annotate('text', y = 4.9, x = xAnnot, label = 'sweets', color = colorSweet, size = 5, hjust = 0) +
  annotate('text', y = 3.3, x = xAnnot, label = 'legumes', color = colorLeg, size = 5, hjust = 0) +
  annotate('text', y = 2.7, x = xAnnot, label = 'starch', color = colorStarch, size = 5, hjust = 0) +
  annotate('text', y = 2.5, x = xAnnot, label = 'vegetables', color = colorVeg, size = 5, hjust = 0) +
  annotate('text', y = 1.9, x = xAnnot, label = 'milk', color = colorMilk, size = 5, hjust = 0) +
  annotate('text', y = 1.75, x = xAnnot, label = 'meat', color = colorMeat, size = 5, hjust = 0) +
  annotate('text', y = 1., x = xAnnot, label = 'fruit', color = colorFruit, size = 5, hjust = 0) +
  annotate('text', y = 0.8, x = xAnnot, label = 'eggs', color = colorEggs, size = 5, hjust = 0) +
  theme_jointplot()+ 
  coord_cartesian(ylim = c(0,7), xlim = c(1,11)) +
  ggtitle('')+
  xlab('wealth index') +
  ylab('number of days consumed') +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = 'grey', size = 0.5),
        panel.grid.minor.y = element_line(colour = 'grey', size = 0.25)
  )  +facet_wrap(~ftfzone + year)



# Foods by FtF, FCS, year (not useful) ------------------------------------


ggplot(hhFtf, aes(x = fcsMin)) + 
  geom_smooth(aes(y = legumes_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorLeg, method = 'loess')+
  #   geom_smooth(aes(y = fish_days), data = hhFtf, 
  #               alpha = 0.2, size = 2, colour= 'salmon', method = 'loess')+
  geom_smooth(aes(y = cereal_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorCer, method = 'loess')+
  geom_smooth(aes(y = milk_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorMilk, method = 'loess')+
  geom_smooth(aes(y = eggs_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorEggs, method = 'loess')+
  geom_smooth(aes(y = veg_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorVeg, method = 'loess')+
  geom_smooth(aes(y = starch_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorStarch, method = 'loess')+
  geom_smooth(aes(y =  fruit_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorFruit, method = 'loess')+
  geom_smooth(aes(y =  meat2_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorMeat, method = 'loess')+
  geom_smooth(aes(y =  sweet_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorSweet, method = 'loess')+
  geom_smooth(aes(y =  oil_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorOil, method = 'loess')+
  geom_smooth(aes(y =  cond_days), data = hhFtf, 
              alpha = 0.2, size = 2, colour= colorSpices, method = 'loess')+
  annotate('text', y = 6.45, x = xAnnot, label = 'cereals', color = colorCer, size = 5, hjust = 0) +
  annotate('text', y = 6.6, x = xAnnot, label = 'condiments', color = colorSpices, size = 5, hjust = 0) +
  annotate('text', y = 6.8, x = xAnnot, label = 'oils', color = colorOil, size = 5, hjust = 0) +
  annotate('text', y = 4.9, x = xAnnot, label = 'sweets', color = colorSweet, size = 5, hjust = 0) +
  annotate('text', y = 3.3, x = xAnnot, label = 'legumes', color = colorLeg, size = 5, hjust = 0) +
  annotate('text', y = 2.7, x = xAnnot, label = 'starch', color = colorStarch, size = 5, hjust = 0) +
  annotate('text', y = 2.5, x = xAnnot, label = 'vegetables', color = colorVeg, size = 5, hjust = 0) +
  annotate('text', y = 1.9, x = xAnnot, label = 'milk', color = colorMilk, size = 5, hjust = 0) +
  annotate('text', y = 1.75, x = xAnnot, label = 'meat', color = colorMeat, size = 5, hjust = 0) +
  annotate('text', y = 1., x = xAnnot, label = 'fruit', color = colorFruit, size = 5, hjust = 0) +
  annotate('text', y = 0.8, x = xAnnot, label = 'eggs', color = colorEggs, size = 5, hjust = 0) +
  theme_jointplot()+ 
  coord_cartesian(ylim = c(0,7), xlim = c(1,112)) +
  ggtitle('')+
  xlab('food consumption score') +
  ylab('number of days consumed') +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = 'grey', size = 0.5),
        panel.grid.minor.y = element_line(colour = 'grey', size = 0.25)
  )  +facet_wrap(~ftfzone + year)
