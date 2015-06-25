#______________________________________________________________________________
# Code to import data from the LS/MS household survey data from the World Bank
# Ethiopia, 2013/2014 surveys.
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
fivea=read_dta('sect5a_hh_w2.dta'); attrA <- pullAttributes(fivea)

fiveaTable = removeAttributes(fivea)

aCols = c("hhID2012", "hhID2014", "eaID2012", "eaID2014","rural", "hhWeight", "region", "zone", "woreda", 
          "town", "subcity", "kebele", "ea", "hhIDtrunc", "foodID", "foodItem",
          "consumedFood", "totEaten", "totEatenUnit", "totOtherUnit",
          "amtPurchased", "amtPuchasedUnit", "amtOtherPurchUnit",
          "amtSpent", "amtProduced", "amtProducedUnit", "amtOtherProdUnit",
          "amtGifted", "amtGiftedUnit", "amtOtherGiftedUnit")
colnames(fiveaTable) = aCols
hh  = fiveaTable

# Change consumed, rural to a binary value.
hh = hh %>% mutate(consumedFood = ifelse(
  consumedFood == 1, 1, ifelse(
    consumedFood == 2, 0, NA)))


# Basic Stats.
# summedFoodsA = hh %>%  group_by(foodItem) %>% summarise(sum = sum(consumedFood, na.rm=TRUE),
#                                                         mean = mean(consumedFood, na.rm = TRUE),
#                                                         sd = sd(consumedFood, na.rm = TRUE))


foods = hh %>% select(hhID2014, foodItem, consumedFood) %>% 
  group_by(hhID2014) %>% 
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
         tubers = Potato + Kocho + Bula,
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
# Combine dietary diversity with regional codes.
#______________________________________________________________________________

# Aggregating table down to a single value per hhID.
hhLoc = hh %>% group_by(hhID2014, hhID2012) %>% 
  select(hhID2014,  eaID2012, eaID2014, rural, hhWeight, region, zone,
         woreda, town, subcity, kebele, ea) %>% 
  summarise_each(funs(mean))

# Basic stats
# hhLoc  %>% group_by(region) %>% summarise(n())
# 
# region  n()
# 1       1  613
# 2       2  136
# 3       3 1034
# 4       4 1056
# 5       5  290
# 6       6  125
# 7       7 1194
# 8      12  130
# 9      13  165
# 10     14  297
# 11     15  222

# attr(fivea$saq01, 'labels') % Checking region codes haven't changed since 2012...

# Merge into a single table.
dietaryDiversity2014 = full_join(hhLoc, foods, by = "hhID2014") %>% 
  mutate(regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", 
                                         ifelse(region == 14, "Addis Ababa", "unknown")))))))

#______________________________________________________________________
# Convert back into a tidy table, for easier graphing.
#______________________________________________________________________
groupedDD2014 = dietaryDiversity2014 %>% 
  select(hhID2014, hhID2012, dietaryDiv, region,
         legumes, cereals, milk, veg, fruit, spices,
         oils, tubers, meat, eggs, sweets) %>% 
  gather(foodItem, sumEaten, -dietaryDiv, -hhID2014, -hhID2012, -region) %>% 
  arrange(hhID2014)


groupedBinnedDD2014 = dietaryDiversity2014 %>% 
  select(hhID2014, hhID2012, dietaryDiv, region,
         legumesBin, cerealsBin, milkBin, 
         veg, fruit, spicesBin, oilsBin, 
         tubersBin, meat, eggs, sweets) %>% 
  gather(foodItem, sumEaten, -dietaryDiv, -hhID2014, -hhID2012, -region) %>% 
  arrange(hhID2014)

# Summary variable with just the dietary diversity for compiling with other vars.
dd2014A = dietaryDiversity2014 %>% 
  select(hhID2014, hhID2012, dd2014A = dietaryDiv, regionComb)

rm(aCols, hh, fivea, fiveaTable, foods, hhLoc, attrA)

