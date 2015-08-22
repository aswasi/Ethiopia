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
sizeAvg = 0.25
colorDot = '#353839' #'#525252'
sizeDot = 2.4

width = 0.8
height = 0.7
# price -------------------------------------------------------------------


priceAvg14 = mean(data14$priceShk)

priceMax = 0.32


ggplot(data14, aes(x = wlthSmooth, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = colorAvg, size = 0.5) +
  geom_smooth(fill = NA, size = 3) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, priceMax)) +
  theme_blankLH()
  # stat_summary(fun.y = mean, geom  = 'line', size = 3)


# Infographic similar to fontawesome chart icon
fontawesome = data.frame(x = 1:20, y = c(1:8, seq(7,4, by = -1), 5:12))
ggplot(fontawesome, aes(x = x, y = y)) +
  geom_path(size = 2, colour = colorDot,
            arrow = arrow(length = unit(0.5,"cm"), type = 'closed')) +
  theme_blankLH() +
  theme(axis.line = element_line(color = 'black', size = 1))

# Price shocks v. education
# <<price_eduF_regr.pdf>>

s = ggplot(data14 %>% filter(!is.na(eduFcat)), aes(x = eduFcat, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = colorAvg, size = sizeAvg) +
  coord_cartesian(xlim = c(0.8,4.2), ylim = c(0, priceMax)) +
  theme_blankLH() + 
  theme(aspect.ratio = height/width) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot, colour = colorDot)

ggsave("~/GitHub/Ethiopia/R/plots/price_eduF_regr.pdf", plot = s,
       width = width, height = height,
       units = 'in',
       useDingbats=FALSE,
       scale = 1.7,
       dpi = 300)

# Religion
# <<price_relig_regr.pdf>>

par(mar = rep(0, 4), oma = rep(0,4), 
    omd = rep(0,4), omi = rep(0,4),
    pty = 'm', pin = c(width, height),
    din = c(width, height),
    omd = rep(0,4),
    plt = rep(1,4))

q = ggplot(data14, aes(x = religHoh, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = '#e31a1c', size = sizeAvg) +
  coord_cartesian(xlim = c(0.8,7.2), ylim = c(0, priceMax)) +
  theme(aspect.ratio = height/width) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # theme_classic() +
  theme_new() + 
#     plot.background = element_rect(colour = 'blue'),
#     panel.background = element_rect('dodgerblue')) +
  stat_summary(fun.y = mean, geom  = 'point', size = sizeDot) + 
  labs(x=NULL, y=NULL)

!! Check deleting par() and increasing the margin w/i params to set buffer w/ x/y-axes.
!! Should be able to change the scale_x instead?
!! Otherwise change x/y limits
!! Delete bg and grid lines from theme
!! and resize the dots and line.

ggsave("~/GitHub/Ethiopia/R/plots/price_relig_regr.pdf", plot = q,
       width = width, height = height,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

ggplot(data14, aes(x = landQtile, y = priceShk)) +
  geom_hline(yint = priceAvg14, color = '#e31a1c', size = 0.5) +
  coord_cartesian(ylim = c(0, priceMax)) +
  theme_blankLH() + 
  stat_summary(fun.y = mean, geom  = 'line', size = 3)
