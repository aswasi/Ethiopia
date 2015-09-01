
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



widthWlthYr = 5. * 1.05
heightWlthYr = 2.25 * 1.05

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

colorsMobile = c('#BC9B8F', '#f768a1',  '#41ab5d', "#2171b5", "#895CB6")

ggplot(mobileData %>% filter(item != 'dvd', item != 'sat'), aes(x = wlthSmooth, y = owns, colour = item)) +
  # geom_smooth(method = "loess", alpha = 0.00, size = 1.15, span = 1.5)  +
  geom_smooth(size = 0.7, fill = NA) +
  # stat_summary(fun.y= mean, geom = 'point')+
  facet_wrap(~year) +   
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   expand = c(0,0),
                   labels=c("", "very poor","poor", "above average")) +
  scale_y_continuous(labels = percent, limits = c(0, 1), 
                     breaks = seq(0,  1, by = 0.25), expand = c(0,0)) +
  scale_color_manual(values = colorsMobile) +
  theme_box_ygrid() +
  # ggtitle('Percent ownership per household') +
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
         wealthIndex2012, wlthChg, wlthSmooth,
         educHoh, educSpouse, educAdultM, educAdultF, literateSpouse,
         femhead, ftfzone, eduMcat, eduFcat,
         religion, literateHoh, agehead, ageLinear) 

panel14 = data %>% 
  filter(year == 2014) %>% 
  select(household_id,   mobile14 = mobile, regionName, 
         wealthIndex2014,wlthSmooth,
         femhead, ftfzone, 
         eduMcat, eduFcat,
         educHoh, educSpouse, educAdultM, educAdultF, literateSpouse,
         religion, literateHoh, agehead, ageLinear)

panel = full_join(panel12, panel14, by = c("household_id", "regionName"))

panel = panel %>% 
  mutate(diff = mobile14 - mobile12,
         ageChg = agehead.y - agehead.x,
         literateChg = literateHoh.y - literateHoh.x,
         literateSpChg = literateSpouse.y - literateSpouse.x,
         eduFchg = educAdultF.y - educAdultF.x,
         eduMchg = educAdultM.y - educAdultM.x, 
         ageQuint.x = cut(agehead.x, c(8, 30, 40, 50,  100)),
         ageQuint.y = cut(agehead.y, c(8, 30, 40, 50,  100))
#          eduMchgCat = ifelse(eduMcat.x == eduMcat.y, 'no change',
#                              ifelse(eduMcat.x == 'no education' & eduMcat.y == '')
         )

# -- Regions -- 
# regionName          x        y1        y2
# 1           Tigray 0.20103093 0.3350515 0.5360825
# 2           Amhara 0.14833127 0.1866502 0.3349815
# 3          Somalie 0.14285714 0.2775510 0.4204082
# 4  Benshagul Gumuz 0.12800000 0.3840000 0.5120000
# 5             SNNP 0.09938525 0.2684426 0.3678279
# 6           Oromia 0.09511229 0.3513871 0.4464993
# 7         Diredawa 0.09322034 0.2372881 0.3305085
# 8             Afar 0.07317073 0.3577236 0.4308943
# 9           Harari 0.05833333 0.4583333 0.5166667
# 10        Gambella 0.04347826 0.4347826 0.4782609
panel %>% 
  group_by(regionName) %>% 
  summarise(diff = mean(diff), y1 = mean(mobile12), y2 = mean(mobile14)) %>% 
  arrange(desc(diff))


# -- wealth --
w1 = panel %>% 
  group_by(wlthSmooth.x) %>% 
  summarise(y1 = mean(mobile12))

w2 = panel %>% 
  group_by(wlthSmooth.y) %>% 
  summarise(y2 = mean(mobile14))

w = full_join(w1, w2, by = c('wlthSmooth.x' = 'wlthSmooth.y')) %>% 
  mutate(diff = y2 - y1, pct = (y2 - y1)/y2)


# -- female-headed --
f1 = panel %>% 
  group_by(femhead.x) %>% 
  summarise(y1 = mean(mobile12))

f2 = panel %>% 
  group_by(femhead.y) %>% 
  summarise(y2 = mean(mobile14))

f = full_join(f1, f2, by = c('femhead.x' = 'femhead.y')) %>% 
  mutate(diff = y2 - y1, pct = (y2 - y1)/y2)


# -- ftf --
ftf1 = panel %>% 
  group_by(ftfzone.x) %>% 
  summarise(y1 = mean(mobile12))

ftf2 = panel %>% 
  group_by(ftfzone.y) %>% 
  summarise(y2 = mean(mobile14))

ftf = full_join(ftf1, ftf2, by = c('ftfzone.x' = 'ftfzone.y')) %>% 
  mutate(diff = y2 - y1, pct = (y2 - y1)/y2)


# -- age --
a1 = panel %>% 
  group_by(femhead.x, ageQuint.x) %>% 
  summarise(y1 = mean(mobile12), n())

a2 = panel %>% 
  group_by(femhead.y, ageQuint.y) %>% 
  summarise(y2 = mean(mobile14), n())

a = full_join(a1, a2, by = c('ageQuint.x' = 'ageQuint.y', 'femhead.x' = 'femhead.y')) %>% 
  mutate(diff = y2 - y1, pct = (y2 - y1)/y2) %>% 
  ungroup() 


# -- literate --
ftf1 = panel %>% 
  group_by(ftfzone.x) %>% 
  summarise(y1 = mean(mobile12))

ftf2 = panel %>% 
  group_by(ftfzone.y) %>% 
  summarise(y2 = mean(mobile14))

ftf = full_join(ftf1, ftf2, by = c('ftfzone.x' = 'ftfzone.y')) %>% 
  mutate(diff = y2 - y1, pct = (y2 - y1)/y2)

panel %>% 
  group_by(regionName) %>% 
  summarise(diff = mean(diff), y1 = mean(mobile12), y2 = mean(mobile14)) %>% 
  arrange(desc(diff))

panel %>% 
  group_by(eduMcat.y) %>% 
  summarise(y1 = mean(mobile14))

maleEdu = panel  %>% 
       filter(!is.na(eduMcat.x), !is.na(eduMcat.y)) %>% 
       group_by(eduMcat.x, eduMcat.y)  %>% 
  summarise(mN = n(), male = mean(diff, na.rm=))

femaleEdu = panel  %>% 
       filter(!is.na(eduFcat.x), !is.na(eduFcat.y)) %>% 
       group_by(eduFcat.x, eduFcat.y)  %>% summarise(fN = n(), female = mean(diff))

View(full_join(maleEdu, femaleEdu, by = c('eduMcat.x' = 'eduFcat.x', 'eduMcat.y' = 'eduFcat.y')))


# Age at middle
# religion not interesting?
# As expected, when get wealthier, buy a phone.
# slight diff b/w Ftf, literate
# literate change big.
ggplot(panel, aes(x = ageChg, y = diff)) +
  stat_summary(fun.y = mean, geom = 'point', size =5, colour = 'red') 

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



# Ladder plot summary: female, age, religion ------------------------------
coloursLadder = c(brewer.pal(9, 'RdPu')[4], 
                             brewer.pal(9, 'RdPu')[5],  
                             brewer.pal(9, 'RdPu')[6]) 



# -- wealth -- 
wealthMob = data  %>% 
  filter(year == 2014, !is.na(wealthQuints)) %>% 
  group_by(wealthQuints) %>% 
  summarise(avg = mean(mobile))


ggplot(wealthMob) +
#   geom_segment(aes(x = wealthQuints, xend = 2, y = `male-headed`, yend = `female-headed`, colour = factor(wealthQuints)),
#                linetype = 1, size = 0.5) +
#   geom_text(aes(x = 1.1, y = `male-headed`,
#                 colour = factor(wealthQuints), label = percent(`male-headed`)),
            # size = sizeText, hjust = 0.5) +
  geom_text(aes(x = wealthQuints, y = avg,
                colour = factor(wealthQuints), label = percent(avg)),
            size = sizeText, hjust = 0.5) +
  scale_colour_manual(values = brewer.pal(9,'RdPu')[3:7])+
  scale_x_reverse(breaks = c(seq(1, 5)), 
                   # expand = c(0,0),
                   labels=c("very poor","poor", "average", "above average", "wealthiest")) +
  coord_cartesian(ylim = c(-0.15, 1)) +
  theme_xOnly() +
  theme(axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title = element_blank())




# -- age --
# << ETH_mobile14_age.pdf >>

heightAge = 2
widthAge = 2.8
ymax = 0.75
sizeText = 3.5

ageMob = data  %>% 
  mutate(ageQuant =  cut(agehead, quantile(agehead, na.rm=T)),
         ageLinear = cut(agehead, c(8, 25, 35, 45, 55, 100)),
         wealthTriads = ifelse(wealthQuints == 3, 'poor',
                               ifelse(wealthQuints < 3, 'very poor',
                                      ifelse(wealthQuints > 3, 'above average', NA)))) %>% 
  group_by(ageLinear, wealthTriads, year) %>% 
  summarise(avg = mean(mobile), n()) %>% 
  filter(!is.na(wealthTriads), year == 2014, !is.na(ageLinear))





ggplot(ageMob, aes(x = ageLinear, y = avg, label = percent(avg),
                   group = factor(wealthTriads),
                   colour = factor(wealthTriads))) +
  geom_line(size = 0.3) + 
  geom_point(colour = 'white', shape= 16, size = sizeText*2.8) +
  geom_text(size = sizeText) +
  scale_colour_manual(values = rev(coloursLadder)) +
  coord_cartesian(ylim = c(-0, ymax)) +
  scale_x_discrete(expand = c(0.1,0.1),
    labels=c("< 25","25 - 35", "35 - 45", "45 - 55", "> 55")) +
  theme_xOnly() +
  theme(axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(),
        title = element_blank(),
        axis.ticks.margin = unit(0, units =  'points'),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        aspect.ratio = heightAge / widthAge,
        axis.text.x = element_text(size = 7.5))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_mobile14_age.pdf", 
       width = widthAge, height = heightAge,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- female-headed --
heightFem = 2
widthFem = 1.52*1

femMob = data  %>% 
  mutate(wealthTriads = ifelse(wealthQuints == 3, 'poor',
                               ifelse(wealthQuints < 3, 'very poor',
                                      ifelse(wealthQuints > 3, 'above average', NA)))) %>% 
  group_by(femhead, wealthTriads, year) %>% 
  summarise(avg = mean(mobile), n(), sd(mobile)) %>% 
  filter(!is.na(wealthTriads), year == 2014, !is.na(femhead))





ggplot(femMob, aes(x = femhead, y = avg, label = percent(avg),
                   group = factor(wealthTriads),
                   colour = factor(wealthTriads))) +
  geom_line(size = 0.3) + 
  geom_point(colour = 'white', shape= 16, size = sizeText*2.8) +
  geom_text(size = sizeText) +
  scale_colour_manual(values = rev(coloursLadder)) +
  coord_cartesian(ylim = c(-0, ymax)) +
  scale_x_discrete(expand = c(0.1,0.1),
                   labels=c("male \n headed", "female \n headed")) +
  theme_xOnly() +
  theme(axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(),
        title = element_blank(),
        axis.ticks.margin = unit(0, units =  'points'),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        aspect.ratio = heightFem / widthFem,
        axis.text.x = element_text(size = 7.5, hjust = 0.5))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_mobile14_fem.pdf", 
       width = widthFem, height = heightFem,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# femaleMob = data  %>% 
#   group_by(femhead, wealthQuints, year) %>% 
#   summarise(avg = mean(mobile)) %>% 
#   filter(wealthQuints == 1 | wealthQuints == 3 | wealthQuints == 5, year == 2014)
# 
# femaleMob = femaleMob %>% 
#   spread(femhead, avg) %>% 
#   mutate(`male-headed` = `0`, `female-headed` = `1`)
# 
# sizeText = 10
# 
# ggplot(femaleMob) +
#   #   geom_segment(aes(x = 1, xend = 1.2, y = `male-headed`, yend = `male-headed`, colour = factor(wealthQuints)),
#   #                size = 1.5) +
#   #   geom_segment(aes(x = 2, xend = 2.2, y = `female-headed`, yend = `female-headed`, colour = factor(wealthQuints)),
#   #                size = 1.5) +
#   geom_segment(aes(x = 1.2, xend = 2, y = `male-headed`, yend = `female-headed`, colour = factor(wealthQuints)),
#                linetype = 1, size = 0.25) +
#   geom_text(aes(x = 1.1, y = `male-headed`,
#                 colour = factor(wealthQuints), label = percent(`male-headed`)),
#             size = sizeText, hjust = 0.5) +
#   geom_text(aes(x = 2.1, y = `female-headed`,
#                 colour = factor(wealthQuints), label = percent(`female-headed`)),
#             size = sizeText, hjust = 0.5) +
#   scale_colour_manual(values = coloursLadder) +
#   coord_cartesian(xlim = c(0.90, 2.3), ylim = c(-0.15, 1)) +
#   annotate('text', x = 1.1, y = 0, label = 'male-headed \n households', size  = 6, hjust = 0.5, vjust = 1) +
#   annotate('text', x = 2.1, y = 0, label = 'female-headed \n households', size  = 6, hjust = 0.5, vjust = 1) +
#   theme_blankLH()



# -- religion -- 

heightRelig = 2
widthRelig = 1.12*2

religMob = data  %>% 
  mutate(wealthTriads = ifelse(wealthQuints == 3, 'poor',
                               ifelse(wealthQuints < 3, 'very poor',
                                      ifelse(wealthQuints > 3, 'above average', NA)))) %>% 
  group_by(religion, wealthTriads, year) %>% 
  summarise(avg = mean(mobile), n(), sd(mobile)) %>% 
  filter(!is.na(wealthTriads), year == 2014, !is.na(religion))


religMob$religion = factor(religMob$religion , c('Muslim', 'Protestant', "Orthodox", 'other'))


ggplot(religMob, aes(x = religion, y = avg, label = percent(avg),
                   group = factor(wealthTriads),
                   colour = factor(wealthTriads))) +
  geom_line(size = 0.3) + 
  geom_point(colour = 'white', shape= 16, size = sizeText*2.8) +
  geom_text(size = sizeText) +
  scale_colour_manual(values = rev(coloursLadder)) +
  coord_cartesian(ylim = c(-0, ymax)) +
  scale_x_discrete(expand = c(0.1,0.1)) +
  theme_xOnly() +
  theme(axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(),
        title = element_blank(),
        axis.ticks.margin = unit(0, units =  'points'),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        aspect.ratio = heightRelig / widthRelig,
        axis.text.x = element_text(size = 7.5))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_mobile14_relig.pdf", 
       width = widthRelig, height = heightRelig,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# religMob = data  %>% 
#   group_by(religion, wealthQuints, year) %>% 
#   summarise(avg = mean(mobile)) %>% 
#   filter(wealthQuints == 1 | wealthQuints == 3 | wealthQuints == 5, year == 2014, !is.na(religion))
# 
# religMob = religMob %>% 
#   spread(religion, avg)
# 
# sizeText = 10
# 
# ggplot(religMob) +
#   #   geom_segment(aes(x = 1, xend = 1.2, y = `male-headed`, yend = `male-headed`, colour = factor(wealthQuints)),
#   #                size = 1.5) +
#   #   geom_segment(aes(x = 2, xend = 2.2, y = `female-headed`, yend = `female-headed`, colour = factor(wealthQuints)),
#   #                size = 1.5) +
#   
#   # -- Cross-tabs --
#   geom_segment(aes(x = 1.3, xend = 1.9, y = Muslim, yend = Protestant, colour = factor(wealthQuints)),
#                linetype = 1, size = 0.25) +
#   geom_segment(aes(x = 2.3, xend = 2.9, y = Protestant, yend = Orthodox, colour = factor(wealthQuints)),
#                linetype = 1, size = 0.25) +
#   geom_segment(aes(x = 3.3, xend = 3.9, y = Orthodox, yend = other, colour = factor(wealthQuints)),
#                linetype = 1, size = 0.25) +
#   
#   # -- percent labels --
#   geom_text(aes(x = 1.1, y = Muslim,
#                 colour = factor(wealthQuints), label = percent(Muslim)),
#             size = sizeText, hjust = 0.5) +
#   geom_text(aes(x = 2.1, y = Protestant,
#                 colour = factor(wealthQuints), label = percent(Protestant)),
#             size = sizeText, hjust = 0.5) +
#   geom_text(aes(x = 3.1, y = Orthodox,
#                 colour = factor(wealthQuints), label = percent(`Orthodox`)),
#             size = sizeText, hjust = 0.5) +
#   geom_text(aes(x = 4.1, y = other,
#                 colour = factor(wealthQuints), label = percent(`other`)),
#             size = sizeText, hjust = 0.5) +
#   scale_colour_manual(values = coloursLadder) +
#   coord_cartesian(xlim = c(0.90, 4.3), ylim = c(-0.15, 1)) +
#   annotate('text', x = 1.1, y = 0, label = 'Muslim', size  = 6, hjust = 0.5, vjust = 1) +
#   annotate('text', x = 2.1, y = 0, label = 'Protestant', size  = 6, hjust = 0.5, vjust = 1) +
#   annotate('text', x = 3.1, y = 0, label = 'Orthodox', size  = 6, hjust = 0.5, vjust = 1) +
#   annotate('text', x = 4.1, y = 0, label = 'other', size  = 6, hjust = 0.5, vjust = 1) +
#   theme_blankLH()
