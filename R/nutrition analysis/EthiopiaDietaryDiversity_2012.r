#______________________________________________________________________________
# Code to import data from the LS/MS household survey data from the World Bank
# Ethiopia, 2011/2012 surveys and 2013/2014 surveys.
#______________________________________________________________________________
# 
# After importing, the data is converted into a more usable form and 
# household-level nutrition variables (dietary diversity, food consumption score --
# FCS) are calculated.
# 
# Laura Hughes, USAID, lhughes@usaid.gov
# June 2015
#______________________________________________________________________________




#______________________________________________________________________________
# IMPORT DATA
#______________________________________________________________________________

# Read in the data downloaded from the World Bank in Stata form.
# Survey Module 5A == food consumed in the last 24 h.
# Survey Module 5B == food consumed in the last week.
fivea=read_dta('sect5a_hh_w1.dta'); attrA <- pullAttributes(fivea)

fiveaTable = removeAttributes(fivea)

# Renaming the cols so they're more sensical.
# EA = enumeration area: geographic area canvassed by one census representative
# 1) Region
# 2) Zone
# 3) Woreda == District
# 4) Town
# 5) Subcity
# 6) Kebele == ward/neighborhood.
aCols = c("hhID", "eaID", "rural", "hhWeight", "region", "zone", "woreda", 
          "town", "subcity", "kebele", "ea", "hhIDtrunc", "foodID", "foodItem",
          "consumedFood", "totEaten", "totEatenUnit", "amtPurchased", "amtPuchasedUnit",
          "amtSpent", "amtProduced", "amtProducedUnit", "amtGifted", "amtGiftedUnit")
colnames(fiveaTable) = aCols
hh  = fiveaTable

# Change consumed to a binary value.
hh = hh %>% mutate(consumedFood = ifelse(
  consumedFood == 1, 1, ifelse(
    consumedFood == 2, 0, NA)))

# Check: total number of y/n
# hh  %>% group_by(consumedFood)  %>% summarise(n())
# consumedFood   n()
# 1            1 30555 % YES
# 2            2 68665 % NO
# 3           NA     5



#______________________________________________________________________________
# CALCULATE DIETARY DIVERSITY
#______________________________________________________________________________
# Remove unrelated variables, aggregate down to the household level, and
# convert the binary 'consumedFood' variable into binary for each food type,
# and then group into food groups.
# Before grouping: replace NAs within the variables.
# Vars w/ NA: Chat, Chickpea, Sugar, Sorghum, Wheat

foods = hh %>% select(hhID, foodItem, consumedFood) %>% 
  group_by(hhID) %>% 
  spread(foodItem, consumedFood) %>% 
  mutate(`Chat/Kat` = ifelse(is.na(`Chat/Kat`), 0, `Chat/Kat`),
         `Chick pea` = ifelse(is.na(`Chick pea`), 0, `Chick pea`),
         Sugar = ifelse(is.na(Sugar), 0, Sugar),
         Sorghum = ifelse(is.na(Sorghum), 0, Sorghum),
         Wheat = ifelse(is.na(Wheat), 0, Wheat)) %>% 
  mutate(cereals = Teff + Wheat + Barley + Maize + Sorghum + Millet,
         legumes = Horsebeans + `Chick pea`+ `Field pea`+ 
           Lentils + `Haricot beans`,
         oils = `Niger seed` + Linseed,
         veg = Onion, 
         fruit = Banana, 
         tubers = Potato + `Kocho/Bula`,
         meat = Meat, 
         milk = Milk + Cheese,
         eggs = Eggs, 
         sweets = Sugar, 
         spices = Salt + Coffee) %>% 
  mutate(cerealsBin = ifelse(cereals > 0, 1, 0), legumesBin = ifelse(legumes > 0, 1, 0),
         oilsBin = ifelse(oils > 0, 1, 0), tubersBin = ifelse(tubers > 0, 1, 0), 
         milkBin = ifelse(milk > 0, 1, 0), spicesBin = ifelse(spices > 0, 1, 0)) %>% 
  mutate(dietaryDiv = cerealsBin + legumesBin + oilsBin + veg + fruit + tubersBin + meat +
           milkBin + eggs + sweets + spicesBin)





#______________________________________________________________________________
# Checks: 
#______________________________________________________________________________
## Number of households (hh):
# length(unique(fivea$household_id)) # 3969 hh

## Checking the totals for each food. #29903 non-tobacco foods.
# summedFoodsA = hh %>%  group_by(foodItem) %>% summarise(sum = sum(consumedFood, na.rm=TRUE),
#                                                         mean = mean(consumedFood, na.rm = TRUE),
#                                                         sd = sd(consumedFood, na.rm = TRUE))
# 
# 
# summedCalcd = groupedFoods %>% group_by(foodItem) %>% 
#   summarise(sum = sum(sumEaten, na.rm=TRUE),
#             mean = mean(sumEaten, na.rm = TRUE),
#             sd = sd(sumEaten, na.rm = TRUE))
# rm("summedCalcd", summedFoodsA)

# # Spot check 10 random hh
# hh2Sample = sample(1:length(unique(fivea$household_id)), 10)
# 
# for (i in 1:10){
#   View(fiveaTable[fivea$household_id==unique(fivea$household_id)[hh2Sample[i]],])
#   
#   foods %>% filter(hhID == unique(fivea$household_id)[hh2Sample[i]]) %>% select(dietaryDiv)
# }

#______________________________________________________________________________
# Combine dietary diversity with regional codes.
#______________________________________________________________________________

# Aggregating table down to a single value per hhID.
hhLoc = hh %>% group_by(hhID) %>% 
  select(hhID, eaID, rural, hhWeight, region, zone,
         woreda, town, subcity, kebele, ea) %>% 
  summarise_each(funs(mean))

dietaryDiversity2012 = full_join(hhLoc, foods, by = "hhID") %>% 
  mutate(regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", "unknown")))))) %>% 
  rename(hhID2012 = hhID)


#______________________________________________________________________________
# Convert back into a tidy table, for easier graphing.
#______________________________________________________________________________
groupedDD2012 = dietaryDiversity2012 %>% 
  select(hhID2012, dietaryDiv, regionComb, legumes, cereals, milk, veg, fruit, spices,
                                oils, tubers, meat, eggs, sweets) %>% 
  gather(foodItem, sumEaten, -dietaryDiv, -hhID2012, -regionComb) %>% arrange(hhID2012)


groupedBinnedDD2012 = dietaryDiversity2012 %>% select(hhID2012, dietaryDiv, regionComb,
                                                      legumesBin, cerealsBin, milkBin, 
                                      veg, fruit, spicesBin, oilsBin, 
                                      tubersBin, meat, eggs, sweets) %>% 
  gather(foodItem, sumEaten, -dietaryDiv, -hhID2012, -regionComb) %>% arrange(hhID2012)


# Summary variable with just the dietary diversity for compiling with other vars.
dd2012A = dietaryDiversity2012 %>% 
  select(hhID2012, dd2012A = dietaryDiv, regionComb)

rm(aCols, fiveaTable, fivea, hh, attrA, hhLoc, foods)
