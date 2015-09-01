# Module for pulling out the shock-level data, and merging to hh-level data.
# Ethiopia LS/MS analysis
# August 2015, Laura Hughes, lhughes@usaid.gov


# Colors ------------------------------------------------------------------
colorShock = '#bd0026'


# Load Data ---------------------------------------------------------------

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
         
         
         goodCope = ifelse(cope1 == 1 | cope1 == 2 | cope1 == 3 | cope1 == 4 | cope1 == 5 | cope1 == 6 | cope1 == 7 | cope1 == 8 | cope1 == 10 | cope1 == 12 | cope1 == 16, 1, 0),
         badCope = ifelse(cope1 == 5 | cope1 == 9 | cope1 == 11| cope1 == 13| cope1 == 14| cope1 == 15| cope1 == 17, 1,0),
         othCope = ifelse(cope1 == 18 | cope1 == 19 | cope1 == 20 | cope1 == 25 | cope1 == 60, 1, 0),
         
         
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
         
         goodCope = ifelse(cope1 == 1 | cope1 == 2 | cope1 == 3 | cope1 == 4 | cope1 == 5 | cope1 == 6 | cope1 == 7 | cope1 == 8 | cope1 == 10 | cope1 == 12 | cope1 == 16, 1, 0),
         badCope = ifelse(cope1 == 5 | cope1 == 9 | cope1 == 11| cope1 == 13| cope1 == 14| cope1 == 15| cope1 == 17, 1,0),
         othCope = ifelse(cope1 == 18 | cope1 == 19 | cope1 == 20 | cope1 == 25 | cope1 == 60, 1, 0),
         
         
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
         religHoh, agehead, ageLinear, saq01, region, ftfzone,
         literateHoh, literateSpouse, educAdultM, 
         educAdultF, educHoh, educSpouse, eduMcat, eduFcat,
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


sh14 = sh %>% 
  filter(year == 2014)


# # Exploring differences ------------------------------------------------------------------
# View(sh  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(religion), !is.na(cope1Cat)) %>%  
#        group_by(cope1Cat, religion)  %>% 
#        summarise(num = n()) %>% ungroup()  %>% 
#        group_by(religion) %>% 
#        mutate(pct=percent(num/sum(num)))  %>% 
#        ungroup() %>% 
#        arrange(cope1Cat))
# 
# View(sh  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(cope1Cat)) %>%  
#        group_by(cope1Cat)  %>% 
#        summarise(num = n()) %>% 
#        mutate(pct=percent(num/sum(num))))


# severity ----------------------------------------------------------------
shSev = sh14 %>% 
  filter(!is.na(shockClass), shockClass != 'asset')

# Eliminating multiple shocks of the same type; allowed to have one type of each shock, over multiple years.
shSev = shSev %>% group_by(household_id, year, shockClass) %>% 
  filter(isShocked == 1) %>% 
  mutate(mostSev = min(shockSev)) %>% 
  summarise(shockSev = mean(mostSev), 
            wlthSmooth = mean(wlthSmooth),
            agehead = mean(agehead),
            ageLinear = mean(ageLinear),
            ftfzone = mean(ftfzone), 
            religion = min(religion),
            TLU = mean(TLUtotal),
            landQtile = mean(landQtile),
            landHectares = mean(landHectares),
            educAdultM = mean(educAdultM),
            educAdultF = mean(educAdultF),
            literateHoh = mean(literateHoh),
            literateSpouse = mean(literateSpouse),
            priceShk = mean(priceShk)
            )


shSev$shockClass = factor(shSev$shockClass, 
                          c('price', 'hazard', 'health'))

# All shocks
# <<ETH_anyShock_shockSeverity_draft.pdf>>
ggplot(shSev, aes(x = shockSev)) +
  stat_bin(binwidth = 0.5, fill = colorShock) + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), 
           vjust = 1.75, hjust = 1.5, color = 'white', size = 4) +
  facet_wrap(~shockClass) +
  theme_xOnly() +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0.75, 3.75),
                     breaks = 1:3,
                     labels = c("most", "", "least")) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 950, by = 100)) +
  theme(panel.grid = element_line(),
        strip.text = element_text(size=20, face = 'bold'),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_line(colour = 'grey', size = 0.1),
        panel.margin = unit(4, "lines"),
        axis.text.x = element_text(hjust = 0)) +
  xlab('severity of shock')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShock_shockSeverity_draft.pdf",
       width = 9, height = 4,
       bg = 'white',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# Shock severity over different factors.

ggplot(shSev, aes(x = wlthSmooth, y = shockSev)) +
  geom_smooth(size  = 1.5) +
  facet_wrap(~shockClass) +
  theme_jointplot() +
  scale_y_reverse() +
  theme(strip.text = element_text(size=20, face = 'bold'),
        panel.margin = unit(2, "lines")) +
  ylab('average shock severity') +
  xlab('wealth decile')

ggplot(shSev, aes(x = wlthSmooth, y = shockSev)) +
  geom_smooth(size  = 1.5) +
  facet_wrap(~shockClass) +
  theme_jointplot() +
  scale_y_reverse() +
  theme(strip.text = element_text(size=20, face = 'bold'),
        panel.margin = unit(2, "lines")) +
  ylab('average shock severity') +
  xlab('wealth decile')

ggplot(shSev, aes(x = agehead, y = shockSev)) +
  geom_smooth(size  = 1.5, method = 'loess', span = 1) +
  facet_wrap(~shockClass) +
  theme_jointplot() +
  scale_y_reverse() +
  theme(strip.text = element_text(size=20, face = 'bold'),
        panel.margin = unit(2, "lines")) +
  ylab('average shock severity') +
  xlab('age of household head')


# Not as interesting-- other dimensions without tons of variation.

# ggplot(shSev, aes(x = TLU, y = shockSev)) +
#   geom_smooth(size  = 1.5, method = 'loess', span = 1) +
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   ylab('average shock severity') +
#   xlab('total tropical livestock units')


# ggplot(shSev, aes(x = landHectares, y = shockSev)) +
#   geom_smooth(size  = 1.5, method = 'loess', span = 1) +
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   ylab('average shock severity') +
#   xlab('land size (hectares)') +
#   coord_cartesian(xlim = c(0,50))
# 
# 
# 
# # worst to be other?
# ggplot(shSev %>% filter(!is.na(religion)), aes(x = religion, y = shockSev)) +
#   stat_summary(fun.y=mean, colour="red", geom = 'point', size = 6)+
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   ylab('average shock severity') +
#   xlab('religion')
# 
# 
# ggplot(shSev, aes(x = landQtile, y = shockSev)) +
#   stat_summary(fun.y=mean, colour="red", geom = 'point', size = 3)+
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   ylab('average shock severity') +
#   xlab('land quartile')
# 
# ggplot(shSev, aes(x = educAdultF, y = shockSev)) +
#   stat_summary(fun.y=mean, colour="red", geom = 'point', size = 3)+
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   xlab('education of adult female') +
#   ylab('average shock severity')
# 
# 
# ggplot(shSev, aes(x = educAdultM, y = shockSev)) +
#   stat_summary(fun.y=mean, colour="red", geom = 'point', size = 3)+
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   xlab('education of adult male') +
#   ylab('average shock severity')


# shk frequency by severity, wealth/age ----------------------------------

sevLimits = c(1.1,1.85)
sizeHH = c(0, 250)
shkMax = 0.22

widthSev = 6.
heightSev = 2.875

# -- age, price
filterVar = 'price'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(ageLinear) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(ageLinear) %>% 
  summarise(avg = mean(priceShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(ageLinear))


ggplot(df, aes(x = ageLinear, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(labels=c("< 25","25 - 35", "35 - 45", "45 - 55", "> 55")) +
  ggtitle('price shocks \npercent of households reporting shock') +
  xlab('age of household head')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_priceShk_sev_age.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


ggsave("~/GitHub/Ethiopia/R/plots/ETH_scale_sev_age.pdf",
       width = widthSev, height = heightSev*2,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- age, hazard
filterVar = 'hazard'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(ageLinear) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(ageLinear) %>% 
  summarise(avg = mean(hazardShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(ageLinear))


ggplot(df, aes(x = ageLinear, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(labels=c("< 25","25 - 35", "35 - 45", "45 - 55", "> 55")) +
  ggtitle('hazard shocks \npercent of households reporting shock') +
  xlab('age of household head')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_hazardShk_sev_age.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- age, health
filterVar = 'health'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(ageLinear) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(ageLinear) %>% 
  summarise(avg = mean(healthShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(ageLinear))


ggplot(df, aes(x = ageLinear, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(labels=c("< 25","25 - 35", "35 - 45", "45 - 55", "> 55")) +
  ggtitle('health shocks \npercent of households reporting shock') +
  xlab('age of household head')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_sev_age.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# -- wealth, health
filterVar = 'health'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(wlthSmooth) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(wlthSmooth) %>% 
  summarise(avg = mean(healthShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(wlthSmooth))


ggplot(df, aes(x = wlthSmooth, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('health shocks \npercent of households reporting shock') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_sev_wlth.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# -- wealth, price
filterVar = 'price'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(wlthSmooth) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(wlthSmooth) %>% 
  summarise(avg = mean(priceShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(wlthSmooth))


ggplot(df, aes(x = wlthSmooth, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('price shocks \npercent of households reporting shock') +
  xlab('wealth')


ggsave("~/GitHub/Ethiopia/R/plots/ETH_priceShk_sev_wlth.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- wealth, hazard
filterVar = 'hazard'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(wlthSmooth) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(wlthSmooth) %>% 
  summarise(avg = mean(hazardShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(wlthSmooth))


ggplot(df, aes(x = wlthSmooth, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('hazard shocks \npercent of households reporting shock') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_hazardShk_sev_wlth.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)





# shock effect ------------------------------------------------------------
effect = sh %>% 
  gather(change, changeEffect, 8:12) %>% 
  filter(!is.na(changeEffect))
# 
# effect = effect %>% 
#   gather(shockType, )


effect$change = factor(effect$change, 
                       c('incomeChg','foodProdChg', 'foodStocksChg',  'assetsChg',  'foodPurchChg' ))

effect$shockClass = factor(effect$shockClass,
                           c('price', 'health', 'hazard'))

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
    theme_box_ygrid() +
    theme(axis.ticks.x = element_blank()) +
    scale_y_continuous(breaks = seq(-800, 200, by = 100)) +
    scale_x_discrete(labels = c('income', 'food product', 'food stocks','assets','food purchases'))
}

plotStackedBar(effect %>% filter(shockClass != 'asset', !is.na(shockClass)), facetVar = 'shockClass')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_chgWlfare.pdf",
       width = widthCoping, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

x = effect %>% 
  filter(shockClass != "asset") %>% 
  group_by(educAdultF, shockClass, changeEffect) %>% 
  summarise(num = n()) %>% 
  mutate(pct = num/sum(num)) %>% 
  filter(!is.na(shockClass))

ggplot(x, 
       aes(x = educAdultF, y = pct, color = factor(changeEffect),
           group = factor(changeEffect))) +
  geom_path(size = 2) + 
  theme_laura() +
  facet_wrap(~shockClass) +
  coord_cartesian(ylim = c(0, 1))



# coping by wealth --------------------------------------------------------

colorCope = brewer.pal(9, 'PuBuGn')
orderCope = c ('sold livestock' , 'used savings' ,  
               'help from govt', 'help from family/friends')
ymax = 0.35

widthCoping = 19.5
heightCoping = 2.5

# price

priceCope = sh14  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))


priceCope$cope1Cat = factor(priceCope$cope1Cat, orderCope)

ggplot(priceCope %>% filter(!is.na(cope1Cat)), aes(x = wlthSmooth, 
                                   y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(2, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('coping with price shocks') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_priceShk_copingWlth.pdf",
       width = widthCoping, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# hazards
haz = sh14  %>% filter(isShocked == 1, hazardShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



haz$cope1Cat = factor(haz$cope1Cat, orderCope)

ggplot(haz %>% filter(!is.na(cope1Cat)), aes(x = wlthSmooth, y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(2, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('coping with hazard shocks') +
  xlab('wealth')



ggsave("~/GitHub/Ethiopia/R/plots/ETH_hazShk_copingWlth.pdf",
       width = widthCoping, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# health
health = sh14  %>% filter(isShocked == 1, healthShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



health$cope1Cat = factor(health$cope1Cat, orderCope)

ggplot(health %>% filter(!is.na(cope1Cat)), aes(x = wlthSmooth, y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(2, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)),
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('coping with health shocks') +
  xlab('wealth')


ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_copingWlth.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# any shock

# coping - sold cows - wealth ---------------------------------------------



allCope = sh14  %>% filter(isShocked == 1, !is.na(wlthSmooth)) %>% 
  mutate(soldCows = ifelse(is.na(cope1Cat), NA, 
                           ifelse(cope1Cat == 'sold livestock', 1, 0)),
         usedSavings = ifelse(is.na(cope1Cat), NA, 
                              ifelse(cope1Cat == 'used savings', 1, 0)),
         didNothing = ifelse(is.na(cope1Cat), NA, 
                           ifelse(cope1Cat == 'did nothing', 1, 0))
         )
 
ggplot(allCope, aes(x = wlthSmooth, y = soldCows))+
  geom_smooth(colour = '#01503f', fill = NA,
              size = 1.25, method = 'loess', span = 1)+ 
  theme_box_ygrid() +
  coord_cartesian(ylim = c(0, ymax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('Wealthy households tend to hold onto valuable assets \n (cows)') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShk_cowsWlth.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)




# coping- saved - by male edu ---------------------------------------------


allCope = sh14  %>% filter(isShocked == 1, !is.na(eduMcat), !is.na(cope1Cat)) %>%  
  group_by(cope1Cat, eduMcat)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(eduMcat) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))


allCope$cope1Cat = factor(allCope$cope1Cat, 'used savings')

ggplot(allCope %>% filter(!is.na(cope1Cat)), aes(x = eduMcat, 
                                                 y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(5, 15)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  theme(panel.margin = unit(2, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('Educated households tend to rely on savings') 




ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShk_savedEduM.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# coping - did nothing - wealth  ------------------------------------------


ggplot(allCope, aes(x = wlthSmooth, y = didNothing))+
  geom_smooth(colour = '#057773', fill = NA,
              size = 1.25, method = 'loess', span = 1)+ 
  theme_box_ygrid() +
  coord_cartesian(ylim = c(0, ymax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('Wealthy households tend to do nothing \n (nada)') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShk_nadaWlth.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Any shock, by religion, coping ------------------------------------------

allCope = sh14  %>% filter(isShocked == 1, !is.na(religion), !is.na(cope1Cat)) %>%  
  group_by(cope1Cat, religion)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(religion) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))


allCope$cope1Cat = factor(allCope$cope1Cat, 'prayed')

ggplot(allCope %>% filter(!is.na(cope1Cat)), aes(x = religion, 
                                                 y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(5, 15)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  theme(panel.margin = unit(2, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('Protestants cope through prayer more than \n other religions percent of households') 

ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShk_copingRelig.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# infographic: all coping -------------------------------------------------

sh  %>% filter(isShocked == 1, year == 2014,
               !is.na(cope1Cat))  %>% 
  group_by(cope1Cat) %>% 
  summarise(num = n()) %>% 
  mutate(pct = percent(num/sum(num))) %>% 
  arrange(desc(num))

# decision tree -----------------------------------------------------------
# decision  %>% group_by(shockClass) %>% summarise(sum(nObs))
# 
# decision = sh %>% 
#   filter(shockClass != 'asset', !is.na(shockClass)) %>% 
#   group_by(shockClass, cope1Cat) %>% 
#   summarise(nObs = n()) %>% 
#   ungroup() %>% 
#   arrange(desc(nObs))
# 
# table = decision
# 
# nestedJSON = function(table){
#   parent = unique(table[,1])
#  
#   json =   '[
#   {'
#   
#   for (i in 1:length(parent)) {
#     temp = table  %>% 
#       filter(shockClass == parent[i,]) %>% 
#       summarise(sum(nObs))
#     
#     json = paste0(json, " 'name': '", parent[i,], "'")
#   }
#   
# 
#       "name": "experienced price shock",
#       "parent": null,
#       "w": 956,
#       "children": [
#         {
#           "name": "primary shock",
#           "parent": "yes",
#           "w": 418,
#           "children": 
# }
# 
