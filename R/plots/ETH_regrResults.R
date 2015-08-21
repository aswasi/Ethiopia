setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")

price = read.csv('Analysis/priceShkWide.txt', skip = 1 , sep = '\t')

source("~/GitHub/Ethiopia/R/loadETHpanel.r")


data14 = data %>% 
  filter(year == 2014) %>% 
  mutate(eduFcat = ifelse(educAdultF == 0, 'no education',
                          ifelse((educAdultF == 1 | educAdultF == 2 | educAdultF == 3), 'primary',
                                 ifelse((educAdultF == 4 | educAdultF == 5), 'secondary',
                                        ifelse(educAdultF == 6, 'tertiary', NA)))))



# colors ------------------------------------------------------------------
colorAvg = '#e31a1c'
sizeAvg = 0.05
colorDot = '#353839' #'#525252'
sizeDot = 0.7

width = 0.8
height = 0.7
# price -------------------------------------------------------------------


priceAvg14 = mean(data14$priceShk)

priceMax = 0.35


ggplot(data14, aes(x = wlthSmooth, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = colorAvg, size = 0.5) +
  geom_smooth(fill = NA, size = 3) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, priceMax)) +
  theme_blankLH()
  # stat_summary(fun.y = mean, geom  = 'line', size = 3)


# Price shocks v. education
ggplot(data14 %>% filter(!is.na(eduFcat)), aes(x = eduFcat, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = colorAvg, size = sizeAvg) +
  coord_cartesian(xlim = c(0.8,4.2), ylim = c(0, priceMax)) +
  theme_blankLH() + 
  theme(aspect.ratio = width/height) +
  scale_x_continuous(expand = c(0,0)) +
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot)

ggsave("~/GitHub/Ethiopia/R/plots/price_eduF_regr.pdf", plot = s,
       width = width, height = height,
       units = 'in', dpi = 300)


ggplot(data14, aes(x = religHoh, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = '#e31a1c', size = 0.5) +
  coord_cartesian(ylim = c(0, priceMax)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'point', size = 5)

ggplot(data14, aes(x = landQtile, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = '#e31a1c', size = 0.5) +
  coord_cartesian(ylim = c(0, priceMax)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'line', size = 3)
