#______________________________________________________________________________
# Calculates the food consumption score for each household in 2014.  See
# Rmarkdown document for full details; methodology is based on the U.N.
# World Food Programme.
# 
# By Laura Hughes, USAID, lhughes@usaid.gov
# June 2014.
#______________________________________________________________________________

library(haven)
library(llamar)
library(dplyr)
library(tidyr)
library(ggplot2)

# READ DATA for food module 5B (food consumed over the past 7 days)

fiveb=read_dta('~/Documents/USAID/Ethiopia/Datain/wave2014/sect5b_hh_w2.dta'); 
attrB <- pullAttributes(fiveb)
fivebTable = removeAttributes(fiveb)

# Rename col headers
bCols = c("hhID2012", "hhID2014", "eaID2012", "eaID2014", "rural", "hhWeight", 
          "region", "zone", "woreda", 
          "town", "subcity", "kebele", "ea", "hhIDtrunc", "foodIDaggr", "foodItemAggr",
          "consumedAggr", "daysConsumed") 
colnames(fivebTable) = bCols
rm("bCols")


# Change to binary: rural, consumedAggr
fivebTable = fivebTable %>% mutate(consumedAggr = ifelse(
  consumedAggr == 1, 1, ifelse(
    consumedAggr == 2, 0, NA)))

# Check that the binary conversion was done correctly.
# fivebTable  %>% group_by(consumedAggr)  %>% summarise(n())
# consumedAggr   n()
# 1            1 37658
# 2            2 46493
# 3           NA    41


weights = data.frame(cereals = 2, pulses = 3, veg = 1, fruit = 1, meat = 4,
                     milk = 4, sugar = 0.5, oil = 0.5)

fivebTable [17696,16] = 'Kocho/Bula' # Fix missing data (Item Num Correct but name not)

hhAggr2014 = fivebTable %>% select(hhID2014, foodItemAggr, daysConsumed) %>% 
  mutate(daysConsumed = ifelse(is.na(daysConsumed), 0, daysConsumed)) %>% # Remove NAs.
  group_by(hhID2014) %>% 
  spread(foodItemAggr, daysConsumed) %>% rowwise() %>% 
  mutate(cerealsMin = max(`Enjera (teff)`, `Other cereal`, Potatoes, `Pasta, Macaroni and Biscuits`,
                          `Kocho/Bula`),
         cerealsMax = ifelse((`Enjera (teff)` + `Other cereal` + Potatoes + 
                                `Pasta, Macaroni and Biscuits` + `Kocho/Bula`) > 7 , 7, 
                             `Enjera (teff)` + `Other cereal` + Potatoes + `Pasta, Macaroni and Biscuits` + `Kocho/Bula`),
         pulses = `Beans, lentils, nuts`,
         veg = Vegetables,
         fruit = Fruits,
         proteinMin = max(`Beef, sheep, goat, or other re`, Poulty, Eggs, Fish),
         proteinMax = ifelse((`Beef, sheep, goat, or other re` + Poulty + Eggs + Fish) > 7, 7,
                             `Beef, sheep, goat, or other re` + Poulty + Eggs + Fish),
         milk = `Milk/yogurt/cheese/other dairy`,
         sugar = `Sugar or sugar products`,
         oil = `Oils/fats/butter`) %>% 
  mutate(fcsMin2014 = cerealsMin * weights$cereals  + pulses * weights$pulses + veg * weights$veg +
           fruit * weights$fruit + proteinMin * weights$meat + milk * weights$milk +
           sugar * weights$sugar + oil * weights$oil,
         fcsMax2014 = cerealsMax * weights$cereals  + pulses * weights$pulses + veg * weights$veg +
           fruit * weights$fruit + proteinMax * weights$meat + milk * weights$milk +
           sugar * weights$sugar + oil * weights$oil) %>% 
  ungroup() %>%
  mutate(fcsCatMin2014 = ifelse(fcsMin2014 < 21, "poor",
                            ifelse(fcsMin2014 > 35, "acceptable", "borderline")),
         fcsCatMax2014 = ifelse(fcsMax2014 < 21, "poor",
                            ifelse(fcsMax2014 > 35, "acceptable", "borderline")))

# View(hhAggr2014)

tidyAggr2014 = hhAggr2014 %>% select(hhID2014, fcsMin2014, cerealsMin, pulses, veg, fruit, proteinMin, milk, sugar, oil) %>% 
  gather(foodGrp, consumedAggr, -fcsMin2014, -hhID2014)


#______________________________________________________________________________
# CHECKS
#______________________________________________________________________________
# sum(hhAggr2014$daysConsumed) #: 126775 NA removal works properly.

fivebTable %>% filter(consumedAggr == 1) %>% group_by(foodItemAggr) %>% summarise(n())

# Check FCS ranges b/w 0 - 112.
hhAggr2014 %>% summarise(avg = mean(fcsMin2014), median = median(fcsMin2014), std = sd(fcsMin2014),
                     min = min(fcsMin2014), max = max(fcsMin2014))

# avg median      std min max
# 1 42.16809   41.5 17.39722   0 112

hhAggr2014 %>% summarise(avg = mean(fcsMax2014), median = median(fcsMax2014), std = sd(fcsMax2014),
                     min = min(fcsMax2014), max = max(fcsMax2014))
# 
# avg median      std min max
# 1 43.4676     42 17.94777   0 112

# Check everything is b/w 0 - 7.
hhAggr2014 %>% group_by(cerealsMin) %>% summarise(num = n())
hhAggr2014 %>% group_by(cerealsMax) %>% summarise(num = n())
hhAggr2014 %>% group_by(proteinMin) %>% summarise(num = n())
hhAggr2014 %>% group_by(proteinMax) %>% summarise(num = n())

rm(attrB, fiveb, fivebTable, weights)
