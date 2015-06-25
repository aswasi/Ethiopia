#______________________________________________________________________________
# Alternate calculation of dietary diversity, based on 7 day recall of food 
# consumption.
#
# Laura Hughes, USAID, lhughes@usaid.gov
#______________________________________________________________________________



#______________________________________________________________________________
# READ DATA for food module 5B (food consumed over the past 7 days)
#______________________________________________________________________________
fiveb=read_dta('sect5b_hh_w2.dta'); 
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

fivebTable [17696,16] = 'Kocho/Bula' # Fix missing data (Item Num Correct but name not)


dd2014B = fivebTable %>% select(hhID2014, foodItemAggr, daysConsumed) %>% 
  mutate(daysConsumed = ifelse(is.na(daysConsumed), 0, daysConsumed)) %>% # Remove NAs.
  group_by(hhID2014) %>% 
  spread(foodItemAggr, daysConsumed) %>% rowwise() %>% 
  mutate(cereals = `Enjera (teff)` + `Other cereal` + `Pasta, Macaroni and Biscuits`,
         tubers = Potatoes + `Kocho/Bula`,
         pulses = `Beans, lentils, nuts`,
         veg = Vegetables,
         fruit = Fruits,
         meat = `Beef, sheep, goat, or other re` + Poulty,
         eggs = Eggs,
         fish = Fish,
         milk = `Milk/yogurt/cheese/other dairy`,
         sugar = `Sugar or sugar products`,
         oils = `Oils/fats/butter`,
         spices = `Other condiments`) %>% 
  ungroup() %>% 
  mutate(cerealsBin = ifelse(cereals > 0, 1, 0), pulsesBin = ifelse(pulses > 0, 1, 0),
         oilsBin = ifelse(oils > 0, 1, 0), tubersBin = ifelse(tubers > 0, 1, 0), 
         milkBin = ifelse(milk > 0, 1, 0), spicesBin = ifelse(spices > 0, 1, 0),
         fishBin = ifelse(fish > 0, 1, 0), eggsBin = ifelse(eggs > 0, 1, 0),
         fruitBin = ifelse(fruit > 0, 1, 0), vegBin = ifelse(veg > 0, 1, 0),
         sugarBin = ifelse(sugar > 0, 1, 0), meatBin = ifelse(meat > 0, 1, 0)) %>% 
  mutate(dd2014B = cerealsBin + pulsesBin + oilsBin + vegBin + fruitBin + tubersBin + meatBin +
           milkBin + eggsBin + sugarBin + spicesBin + fishBin)



dd2014B %>% group_by(dd2014B) %>% summarise(n())
# dd2014B n()
# 1        0  18
# 2        1  27
# 3        2 103
# 4        3 294
# 5        4 572
# 6        5 885
# 7        6 998
# 8        7 946
# 9        8 620
# 10       9 381
# 11      10 262
# 12      11 147
# 13      12   9

ggplot(dd2014B, aes(x=dd2014B)) + geom_histogram(stat="bin", binwidth = 0.2) + theme_bw()


# Basic stats.

dd2014B %>% summarise(avg2014 = round(mean(dd2014B),2), med2014 = median(dd2014B), std2014 = round(sd(dd2014B),2))
# avg2014 med2014 std2014
# 1     6.3       6    2.11

