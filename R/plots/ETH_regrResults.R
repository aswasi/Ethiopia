setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")

# price = read.csv('Analysis/priceShkWide.txt', skip = 1 , sep = '\t')

source("~/GitHub/Ethiopia/R/loadETHpanel.r")


data14 = data %>% 
  filter(year == 2014, ptrack == 2) %>% 
  mutate(eduFcat = ifelse(educAdultF == 0, 'no education',
                          ifelse((educAdultF == 1 | educAdultF == 2 | educAdultF == 3), 'primary',
                                 ifelse((educAdultF == 4 | educAdultF == 5), 'secondary',
                                        ifelse(educAdultF == 6, 'tertiary', NA)))),
         eduMcat = ifelse(educAdultM == 0, 'no education',
                          ifelse((educAdultM == 1 | educAdultM == 2 | educAdultM == 3), 'primary',
                                 ifelse((educAdultM == 4 | educAdultM == 5), 'secondary',
                                        ifelse(educAdultM == 6, 'tertiary', NA)))))



# colors ------------------------------------------------------------------
colorAvg = '#e31a1c'
sizeAvg = 0.25
colorDot = '#353839' #'#525252'
colorAnnot = BuBr[2]
sizeDot = 2.4

width = 0.8
height = 0.7


# Infographic similar to fontawesome chart icon
fontawesome = data.frame(x = 1:20, y = c(1:8, seq(7,4, by = -1), 5:12))

ggplot(fontawesome, aes(x = x, y = y)) +
  geom_path(size = 0.8, colour = colorDot,
            arrow = arrow(length = unit(0.1,"cm"), type = 'closed')) +
  theme(aspect.ratio = 5/6) +
  theme_blankLH() +
  theme(axis.line = element_line(color = 'black', size = 0.15))

ggsave("~/GitHub/Ethiopia/R/plots/dummy_timeChart.pdf", 
       width = 0.6, height = 0.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       scale = 1.25,
       dpi = 300)


# price -------------------------------------------------------------------


priceAvg14 = mean(data14$priceShk, na.rm = TRUE)

priceMax = 0.32


ggplot(data14, aes(x = wlthSmooth, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = colorAvg, size = 0.5) +
  geom_smooth(fill = NA, size = 3) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, priceMax)) +
  theme_blankLH()
# stat_summary(fun.y = mean, geom  = 'line', size = 3)




# Price shocks v. education
# <<price_eduF_regr.pdf>>

s = ggplot(data14, aes(x = eduFcat, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = colorAvg, size = sizeAvg) +
  coord_cartesian(xlim = c(0.6,4.4), ylim = c(0, priceMax)) +
  theme(aspect.ratio = height/width) +
  # scale_x_discrete(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot) + 
  labs(x=NULL, y=NULL)


ggsave("~/GitHub/Ethiopia/R/plots/price_eduF_regr.pdf", plot = s,
       width = width, height = height,
       bg = 'white',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Religion
# <<price_relig_regr.pdf>>


q = ggplot(data14, aes(x = religHoh, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = colorAvg, size = sizeAvg) +
  coord_cartesian(xlim = c(0.6,7.4), ylim = c(0, priceMax)) +
  theme(aspect.ratio = height/width) +
  # scale_x_discrete(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot) + 
  labs(x=NULL, y=NULL)


ggsave("~/GitHub/Ethiopia/R/plots/price_relig_regr.pdf", plot = q,
       width = width, height = height,
       bg = 'white',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

ggplot(data14, aes(x = landQtile, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = colorAvg, size = 0.5) +
  coord_cartesian(ylim = c(0, priceMax)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'line', size = 3)


# health ------------------------------------------------------------------
healthAvg14 = mean(data14$healthShk, na.rm = TRUE)

healthMax = healthAvg14 + 0.03

ggplot(data14, aes(x = eduFcat, y = healthShk)) +
  geom_hline(yint = healthAvg14, color = colorAvg, size = sizeAvg) +
  coord_cartesian(xlim = c(0.6,4.4), ylim = c(0, healthMax)) +
  theme(aspect.ratio = height/width) +
  # scale_x_discrete(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot) + 
  labs(x=NULL, y=NULL)


ggplot(data14, aes(x = ftfzone, y = healthShk)) +
  geom_hline(yint = healthAvg14, color = colorAvg, size = sizeAvg) +
  coord_cartesian(xlim = c(-0.4,1.4), ylim = c(0, healthMax)) +
  theme(aspect.ratio = height/width) +
  # scale_x_discrete(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot) + 
  labs(x=NULL, y=NULL)

ggplot(data14, aes(x = marriedHoh, y = healthShk)) +
  geom_hline(yint = healthAvg14, color = colorAvg, size = sizeAvg) +
  coord_cartesian(xlim = c(-0.4,1.4), ylim = c(0, healthMax)) +
  theme(aspect.ratio = height/width) +
  # scale_x_discrete(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot) + 
  labs(x=NULL, y=NULL)



ggplot(data14, aes(x = dist_market, y = healthShk)) +
  geom_hline(yint = healthAvg14, color = colorAvg, size = sizeAvg) +
  # coord_cartesian(xlim = c(-0.4,1.4), ylim = c(0, healthMax)) +
  theme(aspect.ratio = height/width) +
  # scale_x_discrete(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_blankLH() + 
  stat_smooth()+
  # stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot) + 
  labs(x=NULL, y=NULL)


# hazard ------------------------------------------------------------------
hazardAvg14 = mean(data14$hazardShk, na.rm = TRUE)

hazardMax = hazardAvg14 + 0.1

ggplot(data14, aes(x = eduMcat, y = hazardShk)) +
  geom_hline(yint = hazardAvg14, color = colorAvg, size = sizeAvg) +
  coord_cartesian(xlim = c(0.6,4.4), ylim = c(0, hazardMax)) +
  theme(aspect.ratio = height/width) +
  # scale_x_discrete(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot) + 
  labs(x=NULL, y=NULL)




# test 2 ------------------------------------------------------------------
priceRegr = data.frame(xVal = 1:3, type = c('primary', 'secondary', 'tertiary'), yVal = c(-0.0158, -0.0460, -0.0931),
             se = c(0.0163354, 0.0251413, 0.042207))


# if (confidLevel == 0.95) {
#   confidFactor = 1.96
# }  else if (confidLevel == 0.9) {
#   confidFactor = 1.645
# } else if (confidLevel == 0.98) {
#   confidFactor = 2.33
# } else if (confidLevel == 0.99) {
#   confidFactor = 2.575
# }

priceRegr = priceRegr %>% 
  mutate(ci = se * 1.96)


healthRegr = data.frame(xVal = 1:3, type = c('primary', 'secondary', 'tertiary'), yVal = c(-0.0124397,
                                                                                           -0.0393059,
                                                                                           -0.0645966),
                       se = c(0.0140293,
                              0.0198599,
                              0.0234935))

healthRegr = healthRegr %>% 
  mutate(ci = se * 1.96)

plumbPlot = function(data,
                     confidLevel = 0.95,
                     colorDot = colorDot,
                     colorAnnot = colorAnnot, 
                     labOffset = 0.1,
                     sizePlumbLine = 0.2){


  
  ggplot(data, aes(y = yVal, x = xVal)) +
    
    # -- Set themes --
    theme_blankLH() + 
    theme(aspect.ratio = 1) +
    coord_cartesian(xlim = c(0.5, 3.5)) +
    
    # -- Plumb line --
    geom_segment(aes(x = xVal, xend = xVal, y = 0,  yend = yVal), colour = colorDot, size = sizePlumbLine) +
    
    # -- CI bars @ CI --
    geom_linerange(aes(x = xVal, ymin = yVal - ci, ymax = yVal + ci), colour = colorAnnot, alpha = 0.2, size = 5) +
    
    
    # -- Point for magnitude of change --
    geom_point(size  = 5, colour = colorDot) +
    
    
    # -- Annotation: baseline --
    geom_hline(yint = 0, colour = colorAvg, size = 0.5) +
    annotate(geom = 'text', label = 'no male education',  y = 0.005, x = 2, 
             color = colorAvg, hjust = 0.5) +
    
    # -- Annotation: % difference --
    geom_text(aes(label = paste0(percent(-yVal), ' points'),  
                  x = xVal + .07, y = yVal / 2), 
              color = colorAnnot, hjust = 0) +
    
    # -- Anotation: category type
    geom_text(aes(label = type,  
                  x = xVal - 0.07, y = yVal), 
              color = colorAnnot, hjust = 1)
}

plumbPlot(x)

