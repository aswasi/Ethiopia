#______________________________________________________________________________
# Calculates the food consumption score for each household in 2012.  See
# Rmarkdown document for full details; methodology is based on the U.N.
# World Food Programme.
# 
# By Laura Hughes, USAID, lhughes@usaid.gov
# June 2014.
#______________________________________________________________________________


#______________________________________________________________________________
# READ DATA for food module 5B (food consumed over the past 7 days)
#______________________________________________________________________________
fiveb=read_dta('sect5b_hh_w1.dta'); 
attrB <- pullAttributes(fiveb)
fivebTable = removeAttributes(fiveb)

# Rename col headers
bCols = c("hhID", "eaID", "rural", "hhWeight", "region", "zone", "woreda", 
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
# 1            0 38314
# 2            1 24236
# 3           NA   954


weights = data.frame(cereals = 2, pulses = 3, veg = 1, fruit = 1, meat = 4,
                     milk = 4, sugar = 0.5, oil = 0.5)

hhAggr2012 = fivebTable %>% select(hhID2012 = hhID, foodItemAggr, daysConsumed) %>% 
  mutate(daysConsumed = ifelse(is.na(daysConsumed), 0, daysConsumed)) %>% # Remove NAs.
  group_by(hhID2012) %>% 
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
  mutate(fcsMin2012 = cerealsMin * weights$cereals  + pulses * weights$pulses + veg * weights$veg +
           fruit * weights$fruit + proteinMin * weights$meat + milk * weights$milk +
           sugar * weights$sugar + oil * weights$oil,
         fcsMax2012 = cerealsMax * weights$cereals  + pulses * weights$pulses + veg * weights$veg +
           fruit * weights$fruit + proteinMax * weights$meat + milk * weights$milk +
           sugar * weights$sugar + oil * weights$oil) %>% 
  ungroup() %>%
  mutate(fcsCatMin2012 = ifelse(fcsMin2012 < 21, "poor",
                            ifelse(fcsMin2012 > 35, "acceptable", "borderline")),
         fcsCatMax2012 = ifelse(fcsMax2012 < 21, "poor",
                            ifelse(fcsMax2012 > 35, "acceptable", "borderline")))



tidyAggr2012 = hhAggr2012 %>% 
  select(hhID2012, fcsMin2012, cerealsMin, pulses, veg, fruit, proteinMin, milk, sugar, oil) %>% 
  gather(foodGrp, consumedAggr, -fcsMin2012, -hhID2012)


#______________________________________________________________________________
# CHECKS
#______________________________________________________________________________
# sum(hhAggr2012$daysConsumed) #: 126775 NA removal works properly.

# Check FCS ranges b/w 0 - 112.
hhAggr2012 %>% summarise(avg = mean(fcsMin2012), median = median(fcsMin2012), std = sd(fcsMin2012),
                     min = min(fcsMin2012), max = max(fcsMin2012))
# avg median      std min   max
# 39.5994     39 17.43861   0 101.5

hhAggr2012 %>% summarise(avg = mean(fcsMax2012), median = median(fcsMax2012), std = sd(fcsMax2012),
                     min = min(fcsMax2012), max = max(fcsMax2012))
#   avg     median      std   min max
# 40.66515     40   17.68079   0 107


# Check everything is b/w 0 - 7.
hhAggr2012 %>% group_by(cerealsMin) %>% summarise(num = n())
hhAggr2012 %>% group_by(cerealsMax) %>% summarise(num = n())
hhAggr2012 %>% group_by(proteinMin) %>% summarise(num = n())
hhAggr2012 %>% group_by(proteinMax) %>% summarise(num = n())

rm(attrB, fiveb, fivebTable, weights)
