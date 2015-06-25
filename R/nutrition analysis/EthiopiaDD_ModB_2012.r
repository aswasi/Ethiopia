#______________________________________________________________________________
# Alternate calculation of dietary diversity, based on 7 day recall of food 
# consumption.
#
# Laura Hughes, USAID, lhughes@usaid.gov
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


dd2012B = fivebTable %>% select(hhID2012 = hhID, foodItemAggr, daysConsumed) %>% 
  mutate(daysConsumed = ifelse(is.na(daysConsumed), 0, daysConsumed)) %>% # Remove NAs.
  group_by(hhID2012) %>% 
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
  mutate(dd2012B = cerealsBin + pulsesBin + oilsBin + vegBin + fruitBin + tubersBin + meatBin +
           milkBin + eggsBin + sugarBin + spicesBin + fishBin)



dd2012B %>% group_by(dd2012B) %>% summarise(n())
# dd2012B n()
# 1        0  73
# 2        1  20
# 3        2 131
# 4        3 367
# 5        4 602
# 6        5 736
# 7        6 824
# 8        7 587
# 9        8 360
# 10       9 174
# 11      10  70
# 12      11  24
# 13      12   1

ggplot(dd2012B, aes(x=dd2012B)) + geom_histogram(stat="bin", binwidth = 0.2) + theme_bw()


# Basic stats.

dd2012B %>% summarise(avg2012 = round(mean(dd2012B),2), med2012 = median(dd2012B), std2012 = round(sd(dd2012B),2))
# avg2012 med2012 std2012
# 1    5.53       6    2.02

