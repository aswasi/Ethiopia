
# Load data ---------------------------------------------------------------
setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")


# price = read.csv('Analysis/priceShkWide.txt', skip = 1 , sep = '\t')

source("~/GitHub/Ethiopia/R/loadETHpanel.r")

data = data %>% 
  filter(ptrack == 2)


data14 = data %>% 
  filter(year == 2014) %>% 
  mutate(eduFcat = ifelse(educAdultF == 0, 'no education',
                          ifelse((educAdultF == 1 | educAdultF == 2 | educAdultF == 3), 'primary',
                                 ifelse((educAdultF == 4 | educAdultF == 5), 'secondary',
                                        ifelse(educAdultF == 6, 'tertiary', NA)))),
         eduMcat = ifelse(educAdultM == 0, 'no education',
                          ifelse((educAdultM == 1 | educAdultM == 2 | educAdultM == 3), 'primary',
                                 ifelse((educAdultM == 4 | educAdultM == 5), 'secondary',
                                        ifelse(educAdultM == 6, 'tertiary', NA)))))


# colors and sizes --------------------------------------------------------
colorsMobile = c('#BC9B8F',  "#D55970", "#3CA37A", "#3D7FCC", "#895CB6")

widthWlthYr = 7.5
heightWlthYr = 3

#   c("#5D9141",
#                  "#C0642E",
#                  "#6E77B5",
#                  "#B52652",
#                  "#BF55C0")


# overall change ----------------------------------------------------------
x = data %>% 
  group_by(year) %>% 
  summarise(avg = mean(mobile))

mob2014 = (x %>% filter(year == 2014))$avg
mob2012 = (x %>% filter(year == 2012))$avg
mobChg = (mob2014 - mob2012)/mob2012





# panelize ----------------------------------------------------------------
mobileData = data %>% 
  select(household_id, household_id2, year, region, regionName, 
         wlthSmooth, wealthIndex2012, wealthIndex2014, wealthIndex,
         watch, phone, mobile, tv, dvd, sat, radio, 
         ftfzone, religion, femhead, literateHoh, agehead) %>% 
  gather(item, owns, -household_id, -household_id2, -year, -region, -regionName,
         -ftfzone, -religion, -femhead, -literateHoh, -agehead, 
         -wlthSmooth, -wealthIndex2012, -wealthIndex2014, -wealthIndex)

mobileData$item = factor(mobileData$item, c('watch', 'mobile', 'tv', 'radio', 'phone'))


# wealth v. year --------------------------------------------------------
ggplot(mobileData %>% filter(item != 'dvd', item != 'sat'), aes(x = wlthSmooth, y = owns, colour = item)) +
  # geom_smooth(method = "loess", alpha = 0.00, size = 1.15, span = 1.5)  +
  geom_smooth(size = 1.15, fill = NA) +
  # stat_summary(fun.y= mean, geom = 'point')+
  facet_wrap(~year) +   
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   expand = c(0,0),
                   labels=c("", "very poor","poor", "above average")) +
  scale_y_continuous(labels = percent, limits = c(0, 1), 
                     breaks = seq(0,  1, by = 0.25), expand = c(0,0)) +
  scale_color_manual(values = colorsMobile) +
  theme_box_ygrid() +
  ggtitle('Percent ownership per household') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/mobiles_wealthYr.pdf", 
       width = widthWlthYr, height = heightWlthYr,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Phones by age -----------------------------------------------------------
ggplot(mobileData %>% filter(item == 'mobile'), aes(x = agehead, y = owns, colour = factor(year))) +
  geom_smooth(size = 1.15, fill = NA) +
  scale_y_continuous(labels = percent, limits = c(0, 1), 
                     breaks = c(0, 0.5, 1), expand = c(0,0)) +
  scale_color_manual(values = c('#ebb7ba', colorsMobile[2])) +
  coord_cartesian(xlim = c(10, 100)) +
  # geom_jitter(alpha = 0.4, size = 4) +
  theme_box_ygrid() +
  annotate('text', x = 15, y = 0.95, label = '2012', colour = '#ebb7ba', size = 5.5) +
  annotate('text', x = 15, y = 0.88, label = '2014', colour = colorsMobile[2], size = 5.5) +
  xlab('age of household head')

ggsave("~/GitHub/Ethiopia/R/plots/mobiles_ageYr.pdf", 
       width = 2.5, height = 2.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Phones by sex -----------------------------------------------------------
ggplot(mobileData %>% filter(item == 'mobile'), aes(x = wlthSmooth, y = owns, colour = factor(femhead))) +
  geom_smooth(size = 1.15, fill = NA) +
  scale_y_continuous(labels = percent, limits = c(0, 1), 
                     breaks = c(0, 0.5, 1), expand = c(0,0)) +
  scale_color_manual(values = c(colorsMobile[4], '#aec0e3')) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   expand = c(0,0),
                   labels=c("", "very poor","poor", "above average")) +
  theme_box_ygrid() +
  annotate('text', x = 1, y = 0.95, label = 'male-headed', colour = colorsMobile[4], size = 5.5) +
  annotate('text', x = 1, y = 0.88, label = 'female-headed', colour = '#aec0e3', size = 5.5) +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/mobiles_wlthFem.pdf", 
       width = 2.5, height = 2.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)




# Not much difference for ftfzones.  

# Phones by religion ------------------------------------------------------
ggplot(mobileData %>% filter(year ==2014,
                             !is.na(religion), item == 'mobile' | item == 'radio'), 
       aes(x = wlthSmooth, y = owns, colour = factor(religion))) +
  geom_smooth(method = "loess", alpha = 0.00, size = 1.15, span = 1.5)  +
  facet_wrap(~item) +   
  scale_x_discrete(expand = c(0,0),
                   breaks = c(seq(0, 10, by = 3)), 
                   labels=c("", "very poor","poor", "above average")) +
  scale_y_continuous(labels = percent, limits = c(0, 1),
                     expand = c(0,0)) +
  theme_box_ygrid() + 
  theme(legend.position = 'left') +ggtitle('religion')



# who got phones? ---------------------------------------------------------
panel12 = data %>% 
  filter(year == 2012) %>%
  select(household_id,  mobile12 = mobile, regionName, 
         wealthIndex2012, wlthChg, 
         educHoh, educSpouse, educAdultM, educAdultF, literateSpouse,
         femhead, ftfzone,         eduMcat, eduFcat,
         religion, literateHoh, agehead) 

panel14 = data %>% 
  filter(year == 2014) %>% 
  select(household_id,   mobile14 = mobile, regionName, 
         wealthIndex2014,
         femhead, ftfzone, 
         eduMcat, eduFcat,
         educHoh, educSpouse, educAdultM, educAdultF, literateSpouse,
         religion, literateHoh, agehead)

panel = full_join(panel12, panel14, by = c("household_id", "regionName"))

panel = panel %>% 
  mutate(diff = mobile14 - mobile12,
         literateChg = literateHoh.y - literateHoh.x,
         literateSpChg = literateSpouse.y - literateSpouse.x,
         eduFchg = educAdultF.y - educAdultF.x,
         eduMchg = educAdultM.y - educAdultM.x,
         eduMchgCat = ifelse(eduMcat.x == eduMcat.y, 'no change',
                             ifelse(eduMcat.x == 'no education' & eduMcat.y == '')
         ))

         
maleEdu = panel  %>% 
       filter(!is.na(eduMcat.x), !is.na(eduMcat.y)) %>% 
       group_by(eduMcat.x, eduMcat.y)  %>% summarise(mN = n(), male = mean(diff))

femaleEdu = panel  %>% 
       filter(!is.na(eduFcat.x), !is.na(eduFcat.y)) %>% 
       group_by(eduFcat.x, eduFcat.y)  %>% summarise(fN = n(), female = mean(diff))

View(full_join(maleEdu, femaleEdu, by = c('eduMcat.x' = 'eduFcat.x', 'eduMcat.y' = 'eduFcat.y')))


# Age at middle
# religion not interesting?
# As expected, when get wealthier, buy a phone.
# slight diff b/w Ftf, literate
# literate change big.
ggplot(panel, aes(x = eduFchg, y = diff)) +
  stat_summary(fun.y = mean, geom = 'point', size =5, colour = 'red') +
  stat_summary(aes(x = eduMchg, y = diff), fun.y = mean, geom = 'point', colour = 'blue', size =5)

# change in wealth index --------------------------------------------------

panel12 = data %>% 
  select(household_id,  year, wealthIndex2012) %>% 
  spread(year, wealthIndex2012) %>% 
  select(-contains('2014'))

panel14 = data %>% 
  select(household_id,  year, wealthIndex2014) %>% 
  spread(year, wealthIndex2014) %>% 
  select(-contains('2012'))

panel = full_join(panel12, panel14)

panel = panel %>% 
  mutate(diff = `2014` - `2012`)

ggplot(panel, aes(x = `2012`, y = `2014`)) +
  geom_point(alpha = 0.3, size = 4) +
  theme_jointplot() +
  # theme(aspect.ratio = 1) +
  geom_abline(slope = 1, colour = 'red')
# coord_cartesian(xlim = c(-1, 6.2), ylim =c(-1, 6.2))

ggplot(panel, aes(x = `2012`, y = diff)) +
  geom_point(alpha = 0.3, size = 4) +
  theme_jointplot()