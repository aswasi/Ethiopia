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

source("R/food security module/setupFncns.r")
hhRaw = read_dta("Data/ETH_201507_LSMS_All.dta") 

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

hhPanel = hhPanel %>% 
  mutate(fcsCat = ifelse(fcsMin < 21, "poor",
                         ifelse(fcsMin > 35, "acceptable", "borderline")))

fcsYear = hhPanel %>% 
  group_by(regionName, year, fcsCat) %>% 
  summarise(num = n()) %>%  
  mutate(pct = num/sum(num)) %>% 
  select(-num)

fcsYear = spread(fcsYear, year, pct) %>% 
  mutate( 
         change = `2014` - `2012`) %>% 
  mutate(chg = ifelse(change < 0, -1, 1)) %>% 
  mutate(chgCorr = chg*c(1, -1, -1)) %>% 
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



