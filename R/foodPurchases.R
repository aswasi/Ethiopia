setwd("~/Documents/USAID/Ethiopia/Datain/")
fivea=read_dta('sect5a_hh_w2.dta'); attrA <- pullAttributes(fivea)
fivea=read_dta('wave2014/sect5a_hh_w2.dta'); attrA <- pullAttributes(fivea)
fiveaTable = removeAttributes(fivea)


food2014  = fiveaTable %>% 
  select(household_id, household_id2,
         foodCode = hh_s5aq00, food = hh_s5aq0a,
         isConsumed = hh_s5aq01,
         totAmt = hh_s5aq02_a, totUnit = hh_s5aq02_b,
         purchAmt = hh_s5aq03_a, purchUnit = hh_s5aq03_b,
         prodAmt = hh_s5aq05_a, prodUnit = hh_s5aq05_b) %>% 
  mutate(year = 2014)

ptrack = data %>% 
  select(household_id, household_id2, year, ptrack, priceShk)

# select only panel
food2014 = right_join(food2014, ptrack) 

food2014 = food2014 %>% 
  mutate(pctPurch = ifelse(purchAmt == 0, 0, 
                           ifelse(
                             totUnit == purchUnit, purchAmt / totAmt, 
                             NA)),
         pctProd = ifelse(prodAmt == 0, 0, 
                          ifelse(totUnit == prodUnit, prodAmt / totAmt, NA)
         ),
         totPct = pctPurch + pctProd
         ) 

ggplot(food2014, aes(x = food, y = pctPurch, order = pctPurch)) + 
  # stat_summary(fun.y = mean,  geom = 'point', size = 5) +
  geom_jitter(size = 5, alpha = 0.2) + 
  theme_jointplot() + coord_cartesian(xlim = c(-0.1,1.1)) + coord_flip()


ggplot(data %>% filter(ftfzone != 99), aes(y = worryLackFood, x = maxDaysLimit, colour = factor(year))) + 
  stat_summary(fun.y = mean, geom = 'point', size =6) +
  facet_wrap(~ftfzone) +
  theme_jointplot() + theme(legend.position = 'left')

ggplot(data, aes(y = worryLackFood, x = maxDaysLimit, colour = factor(year))) + 
  stat_smooth()+
  theme_jointplot()


ggplot(food2014, aes(x = pctProd, y = priceShk)) + 
  # stat_summary(fun.y = mean,  geom = 'point', size = 5) +
  facet_wrap(~food) +
  geom_smooth()+
  # geom_jitter(size = 5, alpha = 0.2) + 
  theme_jointplot() + coord_cartesian(xlim = c(-0.1,1.1), ylim = c(-0.1, 1.1)) 

ggplot(food2014, aes(x = pctPurch, y = priceShk)) + 
  # stat_summary(fun.y = mean,  geom = 'point', size = 5) +
  facet_wrap(~food) +
  geom_smooth()+
  # geom_jitter(size = 5, alpha = 0.2) + 
  theme_jointplot() + coord_cartesian(xlim = c(-0.1,1.1), ylim = c(-0.1, 1.1)) 


x = fiveaTable %>%
filter(!is.na(hh_s5aq02_b)) %>%
group_by(tot = hh_s5aq02_b,
purch = hh_s5aq03_b,
own = hh_s5aq05_b) %>%
summarise(r=n()) %>%
arrange(desc(r))




data = data %>% 
  mutate()


