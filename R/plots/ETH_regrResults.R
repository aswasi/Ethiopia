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


