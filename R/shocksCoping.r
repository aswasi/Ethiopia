# Module for pulling out the shock-level data, and merging to hh-level data.
# Ethiopia LS/MS analysis
# August 2015, Laura Hughes, lhughes@usaid.gov


# Load Data ---------------------------------------------------------------
source("~/GitHub/Ethiopia/R/setupFncns.r")

# Read in the merged household level data and load packages / helper functions.
source("~/GitHub/Ethiopia/R/loadETHpanel.r")
# remove child data
rm(child, childRaw)


# Load in raw shock data from 2012 and 2014.
setwd("~/Documents/USAID/Ethiopia/")

shocks2012Raw = read_dta("Datain/wave2012/sect8_hh_w1.dta")
shocks2014Raw = read_dta("Datain/wave2014/sect8_hh_w2.dta")

attr2012 = pullAttributes(shocks2012Raw)
attr2014 = pullAttributes(shocks2014Raw)

shocks2012 = removeAttributes(shocks2012Raw)
shocks2014 = removeAttributes(shocks2014Raw)


# Rename and consolidate shock data ---------------------------------------


# Remove variables don't need; add in ones do.
shocks2012 = shocks2012 %>% 
  mutate(year = 2012,
         isShocked = ifelse(hh_s8q01 == 2, 0,
                            ifelse(hh_s8q01 == 1, 1, NA)),
         incomeChg = ifelse(hh_s8q03_a == 1, 1, 
                            ifelse(hh_s8q03_a == 2, -1, 
                                   ifelse(hh_s8q03_a == 3, 0, NA))),
         assetsChg = ifelse(hh_s8q03_b == 1, 1, 
                            ifelse(hh_s8q03_b == 2, -1, 
                                   ifelse(hh_s8q03_b == 3, 0, NA))),
         foodProdChg = ifelse(hh_s8q03_c == 1, 1, 
                              ifelse(hh_s8q03_c == 2, -1, 
                                     ifelse(hh_s8q03_c == 3, 0, NA))),
         foodStocksChg = ifelse(hh_s8q03_d == 1, 1, 
                                ifelse(hh_s8q03_d == 2, -1, 
                                       ifelse(hh_s8q03_d == 3, 0, NA))),
         foodPurchChg = ifelse(hh_s8q03_e == 1, 1, 
                               ifelse(hh_s8q03_e == 2, -1, 
                                      ifelse(hh_s8q03_e == 3, 0, NA)))
  ) %>% 
  select(household_id, year, regionCode = saq01,
         shockCode = hh_s8q00, shockDescrip = hh_s8q0a, 
         isShocked, shockSev = hh_s8q02,
         incomeChg, assetsChg, foodProdChg, foodStocksChg, foodPurchChg,
         cope1 = hh_s8q04_a, cope2 = hh_s8q04_b, cope3 = hh_s8q04_c,
         freqYr = hh_s8q05, freq5y = hh_s8q06)%>% 
  mutate(shockDescrip = ifelse(shockCode == 101, 'Death of household head',
                               ifelse(shockCode == 102, 'Illness of household member',
                                      ifelse(shockCode == 103, 'Loss of non-farm jobs',
                                             ifelse(shockCode == 104, 'Drought',
                                                    ifelse(shockCode == 105, 'Flood',
                                                           ifelse(shockCode == 106, 'Landslides / avalanches',
                                                                  ifelse(shockCode == 107, 'Heavy rains preventing work',
                                                                         ifelse(shockCode == 108, 'Crop damage',
                                                                                ifelse(shockCode == 109, 'Price fall of food items',
                                                                                       ifelse(shockCode == 110, 'Price rise of food items',
                                                                                              ifelse(shockCode == 111, 'Price increase of farm inputs',
                                                                                                     ifelse(shockCode == 112, 'Loss of livestock',
                                                                                                            ifelse(shockCode == 113, 'Fire',
                                                                                                                   ifelse(shockCode == 114, 'Theft, robbery, and violence',
                                                                                                                          ifelse(shockCode == 115, 'Involuntary loss of house / land',
                                                                                                                                 ifelse(shockCode == 116, 'Displacement from govt. dvpt.',
                                                                                                                                        ifelse(shockCode ==117, 'Local unrest / violence',
                                                                                                                                               ifelse(shockCode == 118, 'other', NA)
                                                                                                                                        ))))))))))))))))),
         
         shockScore = incomeChg + assetsChg + foodProdChg + foodStocksChg + foodPurchChg,
         
         assetShockBin = ifelse(isShocked == 1 & (shockCode == 108 | shockCode == 112 | 
                                                    shockCode == 115 | shockCode == 116), 1, 0),
         priceShockBin = ifelse(isShocked == 1 & (shockCode == 109 | shockCode == 110 | 
                                                    shockCode == 111), 1, 0),
         hazardShockBin = ifelse(isShocked == 1 & (shockCode == 104 | shockCode == 105 | 
                                                     shockCode == 106 | shockCode == 107 | shockCode == 113), 1, 0),
         healthShockBin = ifelse(isShocked == 1 & (shockCode == 101 | shockCode == 102), 1, 0),
         
         
         shockClass = ifelse(isShocked == 1 & (shockCode == 108 | shockCode == 112 | 
                                                 shockCode == 115 | shockCode == 116), 'asset',
                             ifelse(isShocked == 1 & (shockCode == 109 | shockCode == 110 | 
                                                        shockCode == 111), 'price',
                                    ifelse(isShocked == 1 & (shockCode == 104 | shockCode == 105 | 
                                                               shockCode == 106 | shockCode == 107 | shockCode == 113), 'hazard',
                                           ifelse(isShocked == 1 & (shockCode == 101 | shockCode == 102), 'health', NA)))),
         
         
         cope1Cat = ifelse(cope1 == 1, 'used savings',
                           ifelse(cope1 == 2, 'help from family/friends',
                                  ifelse(cope1 == 3, 'help from govt',
                                         ifelse(cope1 == 4, 'help from NGO/relig org',
                                                ifelse(cope1 == 5, 'changed food consump',
                                                       ifelse(cope1 == 6, 'second job', 
                                                              ifelse(cope1 == 7, 'new job',
                                                                     ifelse(cope1 == 8, 'migrated',
                                                                            ifelse(cope1 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope1 == 10, 'got credit',
                                                                                          ifelse(cope1 == 11, 'sold ag assets',
                                                                                                 ifelse(cope1 == 12, 'sold durable assets',
                                                                                                        ifelse(cope1 == 13, 'sold land',
                                                                                                               ifelse(cope1 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope1 == 15, 'sold livestock',
                                                                                                                             ifelse(cope1 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope1 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope1 == 18, 'prayed',
                                                                                                                                                  ifelse(cope1 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope1 == 20, 'other', NA)))))))))))))))))))),
         cope2Cat = ifelse(cope2 == 1, 'used savings',
                           ifelse(cope2 == 2, 'help from family/friends',
                                  ifelse(cope2 == 3, 'help from govt',
                                         ifelse(cope2 == 4, 'help from NGO/relig org',
                                                ifelse(cope2 == 5, 'changed food consump',
                                                       ifelse(cope2 == 6, 'second job', 
                                                              ifelse(cope2 == 7, 'new job',
                                                                     ifelse(cope2 == 8, 'migrated',
                                                                            ifelse(cope2 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope2 == 10, 'got credit',
                                                                                          ifelse(cope2 == 11, 'sold ag assets',
                                                                                                 ifelse(cope2 == 12, 'sold durable assets',
                                                                                                        ifelse(cope2 == 13, 'sold land',
                                                                                                               ifelse(cope2 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope2 == 15, 'sold livestock',
                                                                                                                             ifelse(cope2 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope2 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope2 == 18, 'prayed',
                                                                                                                                                  ifelse(cope2 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope2 == 20, 'other', NA)))))))))))))))))))),
         cope3Cat = ifelse(cope3 == 1, 'used savings',
                           ifelse(cope3 == 2, 'help from family/friends',
                                  ifelse(cope3 == 3, 'help from govt',
                                         ifelse(cope3 == 4, 'help from NGO/relig org',
                                                ifelse(cope3 == 5, 'changed food consump',
                                                       ifelse(cope3 == 6, 'second job', 
                                                              ifelse(cope3 == 7, 'new job',
                                                                     ifelse(cope3 == 8, 'migrated',
                                                                            ifelse(cope3 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope3 == 10, 'got credit',
                                                                                          ifelse(cope3 == 11, 'sold ag assets',
                                                                                                 ifelse(cope3 == 12, 'sold durable assets',
                                                                                                        ifelse(cope3 == 13, 'sold land',
                                                                                                               ifelse(cope3 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope3 == 15, 'sold livestock',
                                                                                                                             ifelse(cope3 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope3 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope3 == 18, 'prayed',
                                                                                                                                                  ifelse(cope3 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope3 == 20, 'other', NA))))))))))))))))))))
         
  )


# Remove variables don't need; add in ones do.
shocks2014 = shocks2014 %>% 
  mutate(year = 2014,
         isShocked = ifelse(hh_s8q01 == 2, 0,
                            ifelse(hh_s8q01 == 1, 1, NA)),
         incomeChg = ifelse(hh_s8q03_a == 1, 1, 
                            ifelse(hh_s8q03_a == 2, -1, 
                                   ifelse(hh_s8q03_a == 3, 0, NA))),
         assetsChg = ifelse(hh_s8q03_b == 1, 1, 
                            ifelse(hh_s8q03_b == 2, -1, 
                                   ifelse(hh_s8q03_b == 3, 0, NA))),
         foodProdChg = ifelse(hh_s8q03_c == 1, 1, 
                              ifelse(hh_s8q03_c == 2, -1, 
                                     ifelse(hh_s8q03_c == 3, 0, NA))),
         foodStocksChg = ifelse(hh_s8q03_d == 1, 1, 
                                ifelse(hh_s8q03_d == 2, -1, 
                                       ifelse(hh_s8q03_d == 3, 0, NA))),
         foodPurchChg = ifelse(hh_s8q03_e == 1, 1, 
                               ifelse(hh_s8q03_e == 2, -1, 
                                      ifelse(hh_s8q03_e == 3, 0, NA)))
  ) %>% 
  select(household_id, household_id2, year, regionCode = saq01,
         shockCode = hh_s8q00, shockDescrip = hh_s8q0a, 
         isShocked, shockSev = hh_s8q02,
         incomeChg, assetsChg, foodProdChg, foodStocksChg, foodPurchChg,
         cope1 = hh_s8q04_a, cope2 = hh_s8q04_b, cope3 = hh_s8q04_c,
         freqYr = hh_s8q05, freq5y = hh_s8q06) %>% 
  
  # remove death of other hh member, to be consistent w/ 2012 data.
  filter(shockCode != 201) %>% 
  mutate(shockDescrip = ifelse(shockCode == 101, 'Death of household head',
                               ifelse(shockCode == 102, 'Illness of household member',
                                      ifelse(shockCode == 103, 'Loss of non-farm jobs',
                                             ifelse(shockCode == 104, 'Drought',
                                                    ifelse(shockCode == 105, 'Flood',
                                                           ifelse(shockCode == 106, 'Landslides / avalanches',
                                                                  ifelse(shockCode == 107, 'Heavy rains preventing work',
                                                                         ifelse(shockCode == 108, 'Crop damage',
                                                                                ifelse(shockCode == 109, 'Price fall of food items',
                                                                                       ifelse(shockCode == 110, 'Price rise of food items',
                                                                                              ifelse(shockCode == 111, 'Price increase of farm inputs',
                                                                                                     ifelse(shockCode == 112, 'Loss of livestock',
                                                                                                            ifelse(shockCode == 113, 'Fire',
                                                                                                                   ifelse(shockCode == 114, 'Theft, robbery, and violence',
                                                                                                                          ifelse(shockCode == 115, 'Involuntary loss of house / land',
                                                                                                                                 ifelse(shockCode == 116, 'Displacement from govt. dvpt.',
                                                                                                                                        ifelse(shockCode ==117, 'Local unrest / violence',
                                                                                                                                               ifelse(shockCode == 118, 'other', NA)
                                                                                                                                        ))))))))))))))))),
         
         shockScore = incomeChg + assetsChg + foodProdChg + foodStocksChg + foodPurchChg,
         
         assetShockBin = ifelse(isShocked == 1 & (shockCode == 108 | shockCode == 112 | 
                                                    shockCode == 115 | shockCode == 116), 1, 0),
         priceShockBin = ifelse(isShocked == 1 & (shockCode == 109 | shockCode == 110 | 
                                                    shockCode == 111), 1, 0),
         hazardShockBin = ifelse(isShocked == 1 & (shockCode == 104 | shockCode == 105 | 
                                                     shockCode == 106 | shockCode == 107 | shockCode == 113), 1, 0),
         healthShockBin = ifelse(isShocked == 1 & (shockCode == 101 | shockCode == 102), 1, 0),
         
         
         shockClass = ifelse(isShocked == 1 & (shockCode == 108 | shockCode == 112 | 
                                                 shockCode == 115 | shockCode == 116), 'asset',
                             ifelse(isShocked == 1 & (shockCode == 109 | shockCode == 110 | 
                                                        shockCode == 111), 'price',
                                    ifelse(isShocked == 1 & (shockCode == 104 | shockCode == 105 | 
                                                               shockCode == 106 | shockCode == 107 | shockCode == 113), 'hazard',
                                           ifelse(isShocked == 1 & (shockCode == 101 | shockCode == 102), 'health', NA)))),
         
         
         cope1Cat = ifelse(cope1 == 1, 'used savings',
                           ifelse(cope1 == 2, 'help from family/friends',
                                  ifelse(cope1 == 3, 'help from govt',
                                         ifelse(cope1 == 4, 'help from NGO/relig org',
                                                ifelse(cope1 == 5, 'changed food consump',
                                                       ifelse(cope1 == 6, 'second job', 
                                                              ifelse(cope1 == 7, 'new job',
                                                                     ifelse(cope1 == 8, 'migrated',
                                                                            ifelse(cope1 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope1 == 10, 'got credit',
                                                                                          ifelse(cope1 == 11, 'sold ag assets',
                                                                                                 ifelse(cope1 == 12, 'sold durable assets',
                                                                                                        ifelse(cope1 == 13, 'sold land',
                                                                                                               ifelse(cope1 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope1 == 15, 'sold livestock',
                                                                                                                             ifelse(cope1 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope1 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope1 == 18, 'prayed',
                                                                                                                                                  ifelse(cope1 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope1 == 20, 'other', NA)))))))))))))))))))),
         cope2Cat = ifelse(cope2 == 1, 'used savings',
                           ifelse(cope2 == 2, 'help from family/friends',
                                  ifelse(cope2 == 3, 'help from govt',
                                         ifelse(cope2 == 4, 'help from NGO/relig org',
                                                ifelse(cope2 == 5, 'changed food consump',
                                                       ifelse(cope2 == 6, 'second job', 
                                                              ifelse(cope2 == 7, 'new job',
                                                                     ifelse(cope2 == 8, 'migrated',
                                                                            ifelse(cope2 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope2 == 10, 'got credit',
                                                                                          ifelse(cope2 == 11, 'sold ag assets',
                                                                                                 ifelse(cope2 == 12, 'sold durable assets',
                                                                                                        ifelse(cope2 == 13, 'sold land',
                                                                                                               ifelse(cope2 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope2 == 15, 'sold livestock',
                                                                                                                             ifelse(cope2 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope2 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope2 == 18, 'prayed',
                                                                                                                                                  ifelse(cope2 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope2 == 20, 'other', NA)))))))))))))))))))),
         cope3Cat = ifelse(cope3 == 1, 'used savings',
                           ifelse(cope3 == 2, 'help from family/friends',
                                  ifelse(cope3 == 3, 'help from govt',
                                         ifelse(cope3 == 4, 'help from NGO/relig org',
                                                ifelse(cope3 == 5, 'changed food consump',
                                                       ifelse(cope3 == 6, 'second job', 
                                                              ifelse(cope3 == 7, 'new job',
                                                                     ifelse(cope3 == 8, 'migrated',
                                                                            ifelse(cope3 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope3 == 10, 'got credit',
                                                                                          ifelse(cope3 == 11, 'sold ag assets',
                                                                                                 ifelse(cope3 == 12, 'sold durable assets',
                                                                                                        ifelse(cope3 == 13, 'sold land',
                                                                                                               ifelse(cope3 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope3 == 15, 'sold livestock',
                                                                                                                             ifelse(cope3 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope3 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope3 == 18, 'prayed',
                                                                                                                                                  ifelse(cope3 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope3 == 20, 'other', NA))))))))))))))))))))
         
  )



# Merge w/ hh data --------------------------------------------------------
hh = data %>% 
  select(household_id, household_id2, year, ptrack,
         religHoh, agehead, saq01, region, ftfzone,
         literateHoh, literateSpouse, educAdultM, 
         educAdultF, educHoh, educSpouse,
         landQtile, landHectares, TLUtotal,
         wealthIndex, wlthSmooth, wealthPanel,
         contains('Shk'), contains('shock'), 
         -merge_shocks) %>% 
  mutate(religion = ifelse(religHoh == 1, 
                           "Orthodox",
                           ifelse(religHoh == 3,
                                  "Protestant",
                                  ifelse(religHoh == 4,
                                         "Muslim",
                                         ifelse(religHoh == 7 | religHoh == 2 | religHoh == 6 | religHoh == 5,
                                                "other", NA)))))


shocksPanel12 = inner_join(shocks2012, hh, by = c("household_id", "year"))

shocksPanel14 = inner_join(shocks2014, hh, by = c("household_id", "household_id2", "year"))

shocksPanel = rbind(shocksPanel12, shocksPanel14)

sh = shocksPanel


pr = sh %>% 
  filter(isShocked == 1, priceShockBin == 1)

# Exploring differences ------------------------------------------------------------------
View(sh  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(religion), !is.na(cope1Cat)) %>%  
       group_by(cope1Cat, religion)  %>% 
       summarise(num = n()) %>% ungroup()  %>% 
       group_by(religion) %>% 
       mutate(pct=percent(num/sum(num)))  %>% 
       ungroup() %>% 
       arrange(cope1Cat))

View(sh  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(cope1Cat)) %>%  
       group_by(cope1Cat)  %>% 
       summarise(num = n()) %>% 
       mutate(pct=percent(num/sum(num))))



# shock effect ------------------------------------------------------------
effect = sh %>% 
  gather(change, changeEffect, 8:12) %>% 
  filter(!is.na(changeEffect))
# 
# effect = effect %>% 
#   gather(shockType, )


effect$change = factor(effect$change, 
                       c('incomeChg','foodProdChg', 'foodStocksChg' ,     'assetsChg',        'foodPurchChg' ))

ggplot(effect, aes(x = change, y = (..count..)/sum(..count..),
                   fill = factor(changeEffect))) +
  geom_bar() +
  facet_wrap(~wlthSmooth) +
  theme_laura()


plotStackedBar = function(data,
                          facetVar = 'wlthSmooth',
                          valueVar = 'changeEffect',
                          colorPos = '#516FB5',
                          colorNeg = '#9E2F49',
                          asPercent = TRUE) {
  
  
  # Filter data to break it into positives and negatives.
  pos = data %>% 
    filter(changeEffect > 0)
  
  neg = data %>% 
    filter(changeEffect < 0)
  
  ggplot(pos, aes(x = change, y = (..count..))) +
    geom_bar(fill = colorPos) +
    geom_bar(aes(x = change, y = -1*(..count..)),
             data = neg, fill = colorNeg) +
    facet_wrap(as.formula(paste0('~', facetVar)))+
    theme_jointplot() 
}

plotStackedBar(effect %>% filter(shockClass != 'asset', !is.na(shockClass)), facetVar = 'shockClass')

x = effect %>% group_by(wlthSmooth, shockClass, changeEffect) %>% 
  summarise(num = n()) %>% 
  mutate(pct = num/sum(num)) %>% 
  filter(!is.na(shockClass))

ggplot(x, 
       aes(x = wlthSmooth, y = pct, color = factor(changeEffect),
           group = factor(changeEffect))) +
  geom_path(size = 2) + 
  theme_laura() +
  facet_wrap(~shockClass) +
  coord_cartesian(ylim = c(0, 1))



# coping by wealth --------------------------------------------------------
# price

w = sh  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>%  
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))




w$cope1Cat = factor(w$cope1Cat, c("sold livestock",           "used savings",             "did nothing"  ,           
                                  "prayed",                   "got credit",               "help from family/friends",
                                  "other",                    "help from govt",           "changed food consump"  ,  
                                  "second job",               "sold crop stock" ,         "decr health/edu spending",
                                  "new job",                  "help from NGO/relig org" , "migrated" ,               
                                  "sent kids away" ,          "sold land",                "sold ag assets",          
                                  "sold durable assets",      "incr fishing" ))

p = ggplot(w %>% filter(), aes(x = wlthSmooth, y = pct, size = num))+
  geom_point()+ 
  theme_jointplot() +
  scale_size_continuous(range = c(2, 11)) +
  theme(legend.position = 'right') +
  facet_wrap(~cope1Cat)

# hazards
haz = sh  %>% filter(isShocked == 1, hazardShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>%  
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



haz$cope1Cat = factor(haz$cope1Cat, c("sold livestock",           "used savings",             "did nothing"  ,           
                                      "prayed",                   "got credit",               "help from family/friends",
                                      "other",                    "help from govt",           "changed food consump"  ,  
                                      "second job",               "sold crop stock" ,         "decr health/edu spending",
                                      "new job",                  "help from NGO/relig org" , "migrated" ,               
                                      "sent kids away" ,          "sold land",                "sold ag assets",          
                                      "sold durable assets",      "incr fishing" ))

ggplot(haz %>% filter(), aes(x = wlthSmooth, y = pct, size = num))+
  geom_point()+ 
  theme_jointplot() +
  scale_size_continuous(range = c(2, 11)) +
  theme(legend.position = 'right') +
  facet_wrap(~cope1Cat)


# health
health = sh  %>% filter(isShocked == 1, healthShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>%  
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



# health$cope1Cat = factor(haz$cope1Cat, c("sold livestock",           "used savings",             "did nothing"  ,           
#                                       "prayed",                   "got credit",               "help from family/friends",
#                                       "other",                    "help from govt",           "changed food consump"  ,  
#                                       "second job",               "sold crop stock" ,         "decr health/edu spending",
#                                       "new job",                  "help from NGO/relig org" , "migrated" ,               
#                                       "sent kids away" ,          "sold land",                "sold ag assets",          
#                                       "sold durable assets",      "incr fishing" ))

h = ggplot(health %>% filter(), aes(x = wlthSmooth, y = pct, size = num))+
  geom_point()+ 
  theme_jointplot() +
  scale_size_continuous(range = c(2, 11)) +
  theme(legend.position = 'right') +
  facet_wrap(~cope1Cat)


# t-tests: are there significant differences between groups? --------------





# Income incr., decr., no change
table8 %>% group_by(hh_s8q03_a) %>% summarise(n())
# hh_s8q03_a   n()
# 1          1    77
# 2          2  2989
# 3          3   398
# 4         NA 67978

# Assets
table8 %>% group_by(hh_s8q03_b) %>% summarise(n())
# hh_s8q03_b   n()
# 1          1    47
# 2          2  2301
# 3          3  1115
# 4         NA 67979

### TESTER data
foodIncr = table8 %>% 
  filter(hh_s8q00 == 110)

#110, 104, 102, 111

examineShocks = function (code){
  x = table8 %>% filter(hh_s8q00 == code, hh_s8q01 == 1) %>% group_by(hh_s8q04_a) %>% 
    summarise(num = n()) %>% arrange(desc(num))
  
  y = table8 %>% filter(hh_s8q00 == code, hh_s8q01 == 1) %>% group_by(hh_s8q04_b) %>% 
    summarise(num = n()) %>% arrange(desc(num))
  
  z = table8 %>% filter(hh_s8q00 == code, hh_s8q01 == 1) %>% group_by(hh_s8q04_c) %>% 
    summarise(num = n()) %>% arrange(desc(num))
  
  coping = full_join(x, y, by = c("hh_s8q04_a" = "hh_s8q04_b")) 
  
  coping = full_join(coping, z, by = c("hh_s8q04_a" = "hh_s8q04_c"))
  
  coping = coping %>% 
    mutate(sumCope = num.x + num.y + num, copeCode = hh_s8q04_a) %>% 
    filter(!is.na(copeCode)) %>% 
    select(copeCode, sumCope) %>% 
    arrange(desc(sumCope))
}



table8 %>% filter(hh_s8q01 == 1) %>% group_by(hh_s8q00) %>% summarise(n())

foodIncrYN = foodIncr %>% group_by(hh_s8q01) %>% summarise(n())

foodShockSev = foodIncr %>% group_by(hh_s8q02) %>% summarise(n())

foodShockCope = foodIncr %>% filter(hh_s8q00 == 110, hh_s8q01 == 1) %>% group_by( hh_s8q04_a) %>% 
  summarise(num = n()) %>% arrange(desc(num))

shockScore = foodIncr %>% 
  mutate(income = ifelse(hh_s8q03_a == 1, 1, 
                         ifelse(hh_s8q03_a == 2, -1, 0)),
         assets = ifelse(hh_s8q03_b == 1, 1, 
                         ifelse(hh_s8q03_b == 2, -1, 0)),
         foodProd = ifelse(hh_s8q03_c == 1, 1, 
                           ifelse(hh_s8q03_c == 2, -1, 0)),
         foodStocks = ifelse(hh_s8q03_d == 1, 1, 
                             ifelse(hh_s8q03_d == 2, -1, 0)),
         foodPurch = ifelse(hh_s8q03_e == 1, 1, 
                            ifelse(hh_s8q03_e == 2, -1, 0))
  ) %>% 
  mutate(shockScore = assets + foodProd + foodStocks + foodPurch)

shockScore %>% group_by(shockScore) %>% summarise(n())
View(shockScore %>% group_by(shockScore, hh_s8q04_b) %>% summarise(num=n()) %>% arrange(desc(num)))
