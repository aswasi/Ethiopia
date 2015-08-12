source("~/GitHub/Ethiopia/R/loadETHpanel.r")



# hfias -------------------------------------------------------------------
child = child %>% 
  select(-q1_HFIAS, -q2_HFIAS, -q2a_HFIAS, -q3_HFIAS, -q3a_HFIAS, -q4_HFIAS,
         -q4a_HFIAS, -q5_HFIAS, -q5a_HFIAS, -q6_HFIAS, -q6a_HFIAS, -q7_HFIAS,
         -q7a_HFIAS, -q8_HFIAS, -q8a_HFIAS, -q9_HFIAS, -q9a_HFIAS) %>% 
  mutate(q1_HFIAS = ifelse(worryLackFood == 1, 2, 
                           ifelse(worryLackFood == 0, 0, NA)),
         q2_HFIAS = ifelse(daysEatBadFood == 0, 0, 
                           ifelse(daysEatBadFood == 1 | daysEatBadFood == 2, 2,
                                  ifelse(daysEatBadFood > 2, 3, NA))),
         q3_HFIAS = ifelse(daysLimitVariety == 0, 0, 
                           ifelse(daysLimitVariety == 1 | daysLimitVariety == 2, 2,
                                  ifelse(daysLimitVariety > 2, 3, NA))),
         q5_HFIAS = ifelse(daysRedAmt == 0, 0, 
                           ifelse(daysRedAmt == 1 | daysRedAmt == 2, 2,
                                  ifelse(daysRedAmt > 2, 3, NA))),
         q6_HFIAS = ifelse(daysRedNumMeals == 0, 0, 
                           ifelse(daysRedNumMeals == 1 | daysRedNumMeals == 2, 2,
                                  ifelse(daysRedNumMeals > 2, 3, NA))),
         q7_HFIAS = ifelse(daysNoFoodSuppl == 0, 0, 
                           ifelse(daysNoFoodSuppl == 1 | daysNoFoodSuppl == 2, 2,
                                  ifelse(daysNoFoodSuppl > 2, 3, NA))),
         q9_HFIAS = ifelse(daysFast == 0, 0, 
                           ifelse(daysFast == 1 | daysFast == 2, 2,
                                  ifelse(daysFast > 2, 3, NA))),
         q5b_HFIAS = ifelse(daysRedAdultIntake == 0, 0, 
                            ifelse(daysRedAdultIntake == 1 | daysRedAdultIntake == 2, 2,
                                   ifelse(daysRedAdultIntake > 2, 3, NA))),
         q20_HFIAS = ifelse(daysBorrowFood == 0, 0, 
                            ifelse(daysBorrowFood  == 1 | daysBorrowFood  == 2, 2,
                                   ifelse(daysBorrowFood  > 2, 3, NA))),
         wtModHFIASscore = (q1_HFIAS + q2_HFIAS + q3_HFIAS + q5b_HFIAS + q5_HFIAS +
                              q7_HFIAS + q9_HFIAS + q20_HFIAS),
         modHFIAS_cat = ifelse(q1_HFIAS < 2 & wtModHFIASscore < 2, 1, # food secure
                               ifelse(q2_HFIAS < 3 | 
                                        q3_HFIAS == 1 |
                                        q5_HFIAS == 0 & 
                                        q6_HFIAS == 0 &
                                        q7_HFIAS == 0 & 
                                        q9_HFIAS == 0, 2, # mildly food insecure
                                      ifelse(q5_HFIAS < 3 |
                                               q6_HFIAS < 3 &
                                               q7_HFIAS == 0 &
                                               q9_HFIAS == 0, 3, # moderately food insecure
                                             4) # severely food insecure
                               )),
         fcsCatMin = ifelse(is.na(fcsMin), NA, 
                            ifelse(fcsMin < 21, "poor",
                                   ifelse(fcsMin > 35, "acceptable", "borderline"))),
         hfias = ifelse(
           modHFIAS_cat == 1, 'none',
           ifelse(modHFIAS_cat == 2, 'mild',
                  ifelse(modHFIAS_cat == 3, 'moderate',
                         ifelse(modHFIAS_cat == 4, 'severe', NA)))),
         toiletBin = ifelse(noToilet == 1,
                            'none',
                            ifelse(noToilet == 0,
                                   'designated area', NA))
  ) %>%  
  ungroup()

child$hfias = factor(child$hfias, rev(c('none', 'mild', 'moderate', 'severe')))
child$fcsCatMin = factor(child$fcsCatMin, c('poor', 'borderline', 'acceptable'))
child$toiletBin = factor(child$toiletBin, c('none', 'designated area'))

data= data%>% 
  select(-q1_HFIAS, -q2_HFIAS, -q2a_HFIAS, -q3_HFIAS, -q3a_HFIAS, -q4_HFIAS,
         -q4a_HFIAS, -q5_HFIAS, -q5a_HFIAS, -q6_HFIAS, -q6a_HFIAS, -q7_HFIAS,
         -q7a_HFIAS, -q8_HFIAS, -q8a_HFIAS, -q9_HFIAS, -q9a_HFIAS) %>% 
  mutate(q1_HFIAS = ifelse(worryLackFood == 1, 2, 
                           ifelse(worryLackFood == 0, 0, NA)),
         q2_HFIAS = ifelse(daysEatBadFood == 0, 0, 
                           ifelse(daysEatBadFood == 1 | daysEatBadFood == 2, 2,
                                  ifelse(daysEatBadFood > 2, 3, NA))),
         q3_HFIAS = ifelse(daysLimitVariety == 0, 0, 
                           ifelse(daysLimitVariety == 1 | daysLimitVariety == 2, 2,
                                  ifelse(daysLimitVariety > 2, 3, NA))),
         q5_HFIAS = ifelse(daysRedAmt == 0, 0, 
                           ifelse(daysRedAmt == 1 | daysRedAmt == 2, 2,
                                  ifelse(daysRedAmt > 2, 3, NA))),
         q6_HFIAS = ifelse(daysRedNumMeals == 0, 0, 
                           ifelse(daysRedNumMeals == 1 | daysRedNumMeals == 2, 2,
                                  ifelse(daysRedNumMeals > 2, 3, NA))),
         q7_HFIAS = ifelse(daysNoFoodSuppl == 0, 0, 
                           ifelse(daysNoFoodSuppl == 1 | daysNoFoodSuppl == 2, 2,
                                  ifelse(daysNoFoodSuppl > 2, 3, NA))),
         q9_HFIAS = ifelse(daysFast == 0, 0, 
                           ifelse(daysFast == 1 | daysFast == 2, 2,
                                  ifelse(daysFast > 2, 3, NA))),
         q5b_HFIAS = ifelse(daysRedAdultIntake == 0, 0, 
                            ifelse(daysRedAdultIntake == 1 | daysRedAdultIntake == 2, 2,
                                   ifelse(daysRedAdultIntake > 2, 3, NA))),
         q20_HFIAS = ifelse(daysBorrowFood == 0, 0, 
                            ifelse(daysBorrowFood  == 1 | daysBorrowFood  == 2, 2,
                                   ifelse(daysBorrowFood  > 2, 3, NA))),
         wtModHFIASscore = (q1_HFIAS + q2_HFIAS + q3_HFIAS + q5b_HFIAS + q5_HFIAS +
                              q7_HFIAS + q9_HFIAS + q20_HFIAS),
         modHFIAS_cat = ifelse(q1_HFIAS < 2 & wtModHFIASscore < 2, 1, # food secure
                               ifelse(q2_HFIAS < 3 | 
                                        q3_HFIAS == 1 |
                                        q5_HFIAS == 0 & 
                                        q6_HFIAS == 0 &
                                        q7_HFIAS == 0 & 
                                        q9_HFIAS == 0, 2, # mildly food insecure
                                      ifelse(q5_HFIAS < 3 |
                                               q6_HFIAS < 3 &
                                               q7_HFIAS == 0 &
                                               q9_HFIAS == 0, 3, # moderately food insecure
                                             4) # severely food insecure
                               )),
         fcsCatMin = ifelse(is.na(fcsMin), NA, 
                            ifelse(fcsMin < 21, "poor",
                                   ifelse(fcsMin > 35, "acceptable", "borderline"))),
         hfias = ifelse(
           modHFIAS_cat == 1, 'none',
           ifelse(modHFIAS_cat == 2, 'mild',
                  ifelse(modHFIAS_cat == 3, 'moderate',
                         ifelse(modHFIAS_cat == 4, 'severe', NA))))
  ) %>%  
  ungroup()



# plots -------------------------------------------------------------------


ggplot(child, aes(x = fcsMin, y = modHFIAS_score, 
                  colour = stunted)) +
  geom_point(size = 4, alpha = 0.2) +
  theme_jointplot()

colorWlth = brewer.pal(9, 'Greens')


ggplot(data, aes(x = fcsMin, y = modHFIAS_score, 
                 colour = wlthSmooth)) +
  scale_colour_gradientn(colours = colorWlth) +
  geom_point(size = 4) +
  theme_jointplot()

ggplot(data, aes(x = fcsCatMin, y = wlthSmooth, 
                 colour = modHFIAS_cat)) +
  scale_colour_gradientn(colours = colorWlth) +
  geom_point(size = 15, alpha = 0.5) +
  theme_jointplot()

colorStunt = brewer.pal(9, 'PiYG')
stunting = child %>% 
  filter(!is.na(stunting))

ggplot(stunting, aes(x = fcsMin, y = modHFIAS_score, 
                     colour = stunting)) +
  scale_colour_gradientn(colours = colorStunt) +
  geom_point(size = 4) +
  theme_jointplot()

colorStunt = rev(brewer.pal(9, 'PiYG'))

stunted = child %>% 
  filter(!is.na(stunted))

ggplot(stunted, aes(x = fcsMin, y = modHFIAS_score, 
                    colour = stunted)) +
  scale_colour_gradientn(colours = colorStunt) +
  geom_point(size = 4, alpha = 0.3) +
  theme_jointplot()

ggplot(stunted, aes(x = fcsMin, y = wtModHFIASscore, 
                    colour = stunted)) +
  scale_colour_gradientn(colours = colorStunt) +
  geom_point(size = 4, alpha = 0.3) +
  theme_jointplot()

ggplot(stunted, aes(x = fcsMin, y = wtModHFIASscore, 
                    colour = stunted)) +
  scale_colour_gradientn(colours = colorStunt) +
  geom_point(size = 4, alpha = 0.3) +
  theme_jointplot()

ggplot(stunted, aes(x = fcsMin, y = modHFIAS_cat, 
                    colour = stunted)) +
  scale_colour_gradientn(colours = colorStunt) +
  geom_jitter(size = 4, alpha = 0.3) +
  theme_jointplot()

ggplot(stunted %>% filter(!is.na(hfias)), aes(x = fcsCatMin, y = hfias, 
                                              colour = stunted)) +
  scale_colour_gradientn(colours = colorStunt) +
  geom_point(size = 25, alpha = 0.3) +
  theme_jointplot() +
  theme(legend.position = 'left')


stunted$hfias = factor(stunted$hfias, rev(c('none', 'mild', 'moderate', 'severe')))
stunted$fcsCatMin = factor(stunted$fcsCatMin, c('poor', 'borderline', 'acceptable'))


ggplot(child, aes(x = fcsMin, y = wtModHFIASscore, 
                  colour = wasted)) +
  scale_colour_gradientn(colours = colorStunt) +
  geom_point(size = 4, alpha = 0.3) +
  theme_jointplot()



# toilets -----------------------------------------------------------------
toiletHFIAS = child %>% 
  filter(!is.na(hfias), !is.na(stunted)) %>% 
  group_by(hfias, toiletBin) %>% 
  summarise(num = n(), avg = mean(stunted)) 

toiletFCS = child %>% 
  filter(!is.na(fcsCatMin), !is.na(stunted)) %>% 
  group_by(toiletBin, fcsCatMin) %>% 
  summarise(num = n(), avg = mean(stunted)) 



ggplot(toiletFCS, aes(x = fcsCatMin, y = toiletBin, 
                      colour = avg)) +
  scale_colour_gradientn(name = 'percent stunted',
                         colours = rev(brewer.pal(11, 'RdYlBu'))) +
  geom_point(alpha = 1, size = 25) +
  theme_classicLH() +
  theme(legend.position = 'left',
        legend.title = element_text(size = 16)) +
  scale_size_continuous(range = c(10, 20), name = 'number of hh',
                        limits = c(15, 2032)) +
  ylab('toilet') +
  xlab('food consumption score') 




ggplot(toiletHFIAS, aes(x = hfias, y = toiletBin, 
                        colour = avg)) +
  scale_colour_gradientn(name = 'percent stunted',
                         colours = rev(brewer.pal(11, 'RdYlBu'))) +
  geom_point(alpha = 1, size = 25) +
  theme_classicLH() +
  theme(legend.position = 'left',
        legend.title = element_text(size = 16)) +
  scale_size_continuous(range = c(10, 20), name = 'number of hh',
                        limits = c(15, 2032)) +
  ylab('toilet') +
  xlab('food insecurity') 

# twister plot ------------------------------------------------------------


fcsThresh = 10

stuntedAvg = stunted %>% 
  filter(fcsMin > fcsThresh) %>% 
  group_by(fcsCatMin, modHFIAS_cat) %>% 
  summarise(num = n(), avg = mean(stunted)) %>% 
  filter(!is.na(modHFIAS_cat)) %>% 
  mutate(hfias = ifelse(
    modHFIAS_cat == 1, 'none',
    ifelse(modHFIAS_cat == 2, 'mild',
           ifelse(modHFIAS_cat == 3, 'moderate',
                  ifelse(modHFIAS_cat == 4, 'severe', NA)))
  ))

stuntedAvg$hfias = factor(stuntedAvg$hfias, rev(c('none', 'mild', 'moderate', 'severe')))
stuntedAvg$fcsCatMin = factor(stuntedAvg$fcsCatMin, c('poor', 'borderline', 'acceptable'))

ggplot(stuntedAvg, aes(x = fcsCatMin, y = hfias, 
                       colour = avg)) +
  scale_colour_gradientn(name = 'percent stunted',
                         colours = rev(brewer.pal(11, 'RdYlBu'))) +
  geom_point(alpha = 1, size = 25) +
  theme_jointplot() +
  theme(legend.position = 'left') +
  scale_size_continuous(range = c(10, 20), name = 'number of hh',
                        limits = c(15, 2032)) +
  ylab('food insecurity') +
  xlab('food consumption score') +
  ggtitle('fcs thresholded at > 10')


# Perceptions: land area. -------------------------------------------------
data %>% 
  mutate(landConstr = ifelse(
    causeShort1 == 3 | causeShort2 == 3 | causeShort3 == 3, 1, 0
  )) %>% 
  filter(foodShortSit == 1, !is.na(landConstr)) %>% 
  select(landConstr, fcsMin, dietDiv, areaField, landQtile) %>% 
  group_by(landConstr) %>% 
    summarise(n(), mean(areaField), mean(landQtile))

# Perceptions: over time, by FtF ------------------------------------------

# Aggregate causes at the month level.
foodShort1 = data %>% 
  select(cause = cause, contains("FoodShort"), 
         -foodShortSit, -numMonthFoodShort, -foodShortage,
         ftfzone) %>% 
  gather(month, shortage, -cause, -ftfzone)

foodShort2 = data %>% 
  select(cause = causeShort2, contains("FoodShort"), 
         -foodShortSit, -numMonthFoodShort, -foodShortage,
         ftfzone) %>% 
  gather(month, shortage, -cause, -ftfzone)

foodShort3 = data %>% 
  select(cause = causeShort3, contains("FoodShort"), 
         -foodShortSit, -numMonthFoodShort, -foodShortage,
         ftfzone) %>% 
  gather(month, shortage, -cause, -ftfzone)

foodShort = rbind(foodShort1, foodShort2, foodShort3)

foodShort= foodShort %>% 
  filter(!is.na(cause), ftfzone != 99, !is.na(ftfzone)) %>%
  # filter(shortage > 0, !is.na(cause), ftfzone != 99, !is.na(ftfzone)) %>%
  group_by(cause, month, ftfzone) %>% 
  summarise(num = sum(shortage))


foodShort = foodShort %>% 
  ungroup() %>% 
  mutate(cause2= ifelse(cause == 1, "drought", 
                            ifelse(cause == 2, "crop pests",
                                   ifelse(cause == 3, "small land",
                                          ifelse(cause == 4, "lack farm inputs",
                                                 ifelse(cause == 5, "lack farm tools",
                                                        ifelse(cause == 6, "food prices",
                                                               ifelse(cause == 7, "high transportation costs",
                                                                      ifelse(cause == 8, "market far away",
                                                                             ifelse(cause == 9, "no food in market",
                                                                                    ifelse(cause == 10, "floods",
                                                                                           ifelse(cause == 11, "other", NA))))))))))),
    
    month = ifelse(month == "janFoodShort", "Jan",
                   ifelse(month == "febFoodShort", "Feb",  
                          ifelse(month == "marFoodShort", "Mar",
                                 ifelse(month == "aprFoodShort", "Apr",
                                        ifelse(month == "mayFoodShort", "May",
                                               ifelse(month == "juneFoodShort", "Jun", 
                                                      ifelse(month == "julyFoodShort", "Jul",
                                                             ifelse(month == "augFoodShort", "Aug", 
                                                                    ifelse(month == "septFoodShort", "Sep",
                                                                           ifelse(month == "octFoodShort", "Oct",
                                                                                  ifelse(month == "novFoodShort", "Nov",  
                                                                                         ifelse(month == "decFoodShort", "Dec", "unknown")
                                                                                  ))))))))))))  %>% 
  select(cause, month, num, ftfzone)




foodShort$month = factor(foodShort$month, month.abb)

foodShortCat$cause = factor(foodShortCat$cause, 
                            levels = c("small land", "food prices","other", "drought",
                                       "lack farm inputs", "lack farm tools","crop pests", 
                                       "market far away",  "floods", "no food in market", 
                                       "high transportation costs"))



ggplot(foodShort %>% filter(!is.na(cause2)), aes(x = month, y = num,
                      group = ftfzone, colour = ftfzone)) +
  geom_line(size = 1) + 
  facet_wrap(~cause2, scales = "free_y") +
  theme_laura() +
  theme(axis.text.x = element_text(size = 12, angle = 90)) +
  ylab("percent of households") +
  xlab("month") +
  ggtitle("Causes for food shortage \n (first, second, third most important causes)") +
  annotate("rect", xmin = 6.5, xmax = 9.25, ymin = 0, 
           ymax = 0.1, alpha = 0.25, fill ='#c7e9b4')

