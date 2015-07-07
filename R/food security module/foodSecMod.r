#_________________________________________________________________________________
# Read in and cleanup Ethiopia LS/MS data on the food security module (Section 7).
#
# When you run source('R/foodSecMod.r'), 6 variables are created:
# - foodSec2012: table containing food security module 2 from 2012.
# - foodSec2014: table containing food security module 2 from 2014.
# - foodSecQuest2012: survey questions from 2012 ('label' in Stata file)
# - foodSecQuest2014: survey questions from 2014 ('label' in Stata file)
# - rawfoodSec2012: raw table from 2012 from Stata import (includes label and labels)
# - rawfoodSec2014: raw table from 2014 from Stata import (includes label and labels)
#
# This function will also save two files in the "Dataout" directory:
# - foodSec2012 --> foodSec2012.csv
# - foodSec2014 --> foodSec2014.csv
#
# Note: column names for 2012/2014 are identical.
#
# Laura Hughes, lhughes@usaid.gov, July 2015
#_________________________________________________________________________________

masterDir = getwd()

# Load libraries and helper functions.
source("R/setupFncns.r") 

#_________________________________________________________________________________
# 2012
#_________________________________________________________________________________
setwd("Datain/wave2012/")

#_________________________________________________________________________________
# READ in data.
#_________________________________________________________________________________

rawfoodSec2012 = read_dta('sect7_hh_w1.dta')

foodSecQuest2012 <- pullAttributes(rawfoodSec2012)

foodSec2012 = removeAttributes(rawfoodSec2012)

foodSecCols = c("hhID2012", "eaID2012", "rural", "hhWeight", "region", 
                "zone", "woreda", "town","subcity", "kebele", "ea", "hhIDTrunc", 
                "worryLackFood", "daysEatBadFood","daysLimitVariety", "daysRedAmt",
                "daysRedNumMeals", "daysRedAdultIntake", "daysBorrowFood", "daysNoFoodSuppl",
                "daysFast", "avgNumMealsAdults", "avgNumMealsKids", "hhSameDiet", "menDietDiv",
                "womenDietDiv", "kidsDietDiv", "foodShortSit", "janFoodShort", "febFoodShort", "marFoodShort", "aprFoodShort",
                "mayFoodShort", "juneFoodShort", "julyFoodShort", "augFoodShort", "septFoodShort",
                "octFoodShort", "novFoodShort", "decFoodShort", "causeShort1",
                "causeShort2",  "causeShort3"
)

colnames(foodSec2012) = foodSecCols

# Clean up dataset; combine regions into one.
foodSec2012 = foodSec2012 %>% 
  
  # Add in region category
  mutate(
    regionComb = ifelse(
      region == 2 | region == 5 | region == 6 |
        region == 12 | region == 13 | region == 15,
      "other", ifelse(region == 1, "Tigray",
                      ifelse(region == 3, "Amhara",
                             ifelse(region == 4, "Oromia",
                                    ifelse(region == 7, "SNNP", 
                                           ifelse(region == 14, "Addis Ababa", "unknown")))))),
    # Convert things to binary
    worryLackFood = ifelse(worryLackFood == 1, 1, ifelse(worryLackFood == 2, 0, NA)),
    hhSameDiet = ifelse(hhSameDiet == 1, 1, ifelse(hhSameDiet == 2, 0, NA)),
    foodShortSit = ifelse(foodShortSit == 1, 1, ifelse(foodShortSit == 2, 0, NA)),
    
    menDietDiv = ifelse(is.na(menDietDiv), 0, 
                        ifelse(menDietDiv == 1, 1,  # +1 if more diverse; -1 if less diverse; 0 if NA
                               ifelse(menDietDiv == 2, -1, 
                                      NA))),
    womenDietDiv = ifelse(is.na(womenDietDiv), 0,
                          ifelse(womenDietDiv == 1, 1,
                                 ifelse(womenDietDiv == 2, -1, 
                                        NA))),
    kidsDietDiv = ifelse(is.na(kidsDietDiv), 0,
                         ifelse(kidsDietDiv == 1, 1,
                                ifelse(kidsDietDiv == 2, -1, 
                                       NA))),
    
    janFoodShort = ifelse(janFoodShort == "X", 1, 0),
    febFoodShort = ifelse(febFoodShort  == "X", 1, 0),
    marFoodShort = ifelse(marFoodShort  == "X", 1, 0),
    aprFoodShort = ifelse(aprFoodShort == "X", 1, 0),
    mayFoodShort = ifelse(mayFoodShort == "X", 1, 0),
    juneFoodShort = ifelse(juneFoodShort == "X", 1, 0),
    julyFoodShort = ifelse(julyFoodShort == "X", 1, 0),
    augFoodShort = ifelse(augFoodShort == "X", 1, 0),
    septFoodShort = ifelse(septFoodShort == "X", 1, 0),
    octFoodShort = ifelse(octFoodShort == "X", 1, 0),
    novFoodShort = ifelse(novFoodShort == "X", 1, 0),
    decFoodShort = ifelse(decFoodShort == "X", 1, 0),
    
    daysEatBadFoodBin = ifelse(daysEatBadFood > 0 , 1, 0),  # NOTE: converts NAs to 0's.
    daysLimitVarietyBin = ifelse(daysLimitVariety > 0, 1, 0), 
    daysRedAmtBin = ifelse(daysRedAmt > 0, 1, 0), 
    daysRedNumMealsBin = ifelse(daysRedNumMeals > 0, 1, 0), 
    daysRedAdultIntakeBin = ifelse(daysRedAdultIntake > 0 , 1, 0), 
    daysBorrowFoodBin = ifelse(daysBorrowFood > 0, 1, 0),    
    daysNoFoodSupplBin = ifelse(daysNoFoodSuppl > 0, 1, 0), 
    daysFastBin = ifelse(daysFast > 0, 1, 0),
    
    # Create categorical data
    menDietDivCat = ifelse(menDietDiv == 1, "more", ifelse(menDietDiv == -1, "less", NA)),
    womenDietDivCat = ifelse(womenDietDiv == 1, "more", ifelse(womenDietDiv == -1, "less", NA)),
    kidsDietDivCat = ifelse(kidsDietDiv == 1, "more", ifelse(kidsDietDiv == -1, "less", NA)),
    causeShort1cat = ifelse(causeShort1 == 1, "drought", 
                            ifelse(causeShort1 == 2, "crop pests",
                                   ifelse(causeShort1 == 3, "small land",
                                          ifelse(causeShort1 == 4, "lack farm inputs",
                                                 ifelse(causeShort1 == 5, "lack farm tools",
                                                        ifelse(causeShort1 == 6, "food prices",
                                                               ifelse(causeShort1 == 7, "high transportation costs",
                                                                      ifelse(causeShort1 == 8, "market far away",
                                                                             ifelse(causeShort1 == 9, "no food in market",
                                                                                    ifelse(causeShort1 == 10, "floods",
                                                                                           ifelse(causeShort1 == 11, "other", NA)
                                                                                    )))))))))),
    causeShort2cat = ifelse(causeShort2 == 1, "drought", 
                            ifelse(causeShort2 == 2, "crop pests",
                                   ifelse(causeShort2 == 3, "small land",
                                          ifelse(causeShort2 == 4, "lack farm inputs",
                                                 ifelse(causeShort2 == 5, "lack farm tools",
                                                        ifelse(causeShort2 == 6, "food prices",
                                                               ifelse(causeShort2 == 7, "high transportation costs",
                                                                      ifelse(causeShort2 == 8, "market far away",
                                                                             ifelse(causeShort2 == 9, "no food in market",
                                                                                    ifelse(causeShort2 == 10, "floods",
                                                                                           ifelse(causeShort2 == 11, "other", NA)
                                                                                    )))))))))),
    causeShort3cat = ifelse(causeShort3 == 1, "drought", 
                            ifelse(causeShort3 == 2, "crop pests",
                                   ifelse(causeShort3 == 3, "small land",
                                          ifelse(causeShort3 == 4, "lack farm inputs",
                                                 ifelse(causeShort3 == 5, "lack farm tools",
                                                        ifelse(causeShort3 == 6, "food prices",
                                                               ifelse(causeShort3 == 7, "high transportation costs",
                                                                      ifelse(causeShort3 == 8, "market far away",
                                                                             ifelse(causeShort3 == 9, "no food in market",
                                                                                    ifelse(causeShort3 == 10, "floods",
                                                                                           ifelse(causeShort3 == 11, "other", NA)
                                                                                    )))))))))),
    
    
    # Create derived vars
    numMonthFoodShort = janFoodShort + febFoodShort + marFoodShort + aprFoodShort +
      mayFoodShort + juneFoodShort + julyFoodShort + augFoodShort + septFoodShort +
      octFoodShort + novFoodShort + decFoodShort) %>% 
  rowwise() %>% 
  mutate(maxDaysLimit = max(daysEatBadFood, daysLimitVariety, daysRedAmt, daysRedNumMeals,
                            daysRedAdultIntake, daysBorrowFood, daysNoFoodSuppl, daysFast)) %>%  # Maximum number of days in a given week ANY change in behaviour.
  ungroup()


setwd(masterDir)
write.csv(foodSec2012, "Dataout/foodSec2012.csv")






#_________________________________________________________________________________
# 2014
#_________________________________________________________________________________
setwd("Datain/wave2014/")

#_________________________________________________________________________________
# READ in data.
#_________________________________________________________________________________

rawfoodSec2014 = read_dta('sect7_hh_w2.dta')

foodSecQuest2014 <- pullAttributes(rawfoodSec2014)

foodSec2014 = removeAttributes(rawfoodSec2014)

foodSecCols = c("hhID2012", "hhID2014", "eaID2012", "eaID2014", "rural", "hhWeight", "region", 
                "zone", "woreda", "town","subcity", "kebele", "ea", "hhIDTrunc", 
                "worryLackFood", "daysEatBadFood","daysLimitVariety", "daysRedAmt",
                "daysRedNumMeals", "daysRedAdultIntake", "daysBorrowFood", "daysNoFoodSuppl",
                "daysFast", "avgNumMealsAdults", "avgNumMealsKids", "hhSameDiet", "menDietDiv",
                "womenDietDiv", "kidsDietDiv", "foodShortSit", "febFoodShort", "marFoodShort", "aprFoodShort",
                "mayFoodShort", "juneFoodShort", "julyFoodShort", "augFoodShort", "septFoodShort",
                "octFoodShort", "novFoodShort", "decFoodShort", "janFoodShort", "causeShort1",
                "causeShort1other", "causeShort2", "causeShort2other", "causeShort3", "causeShort3other"
)

colnames(foodSec2014) = foodSecCols

# Clean up dataset; combine regions into one.
foodSec2014 = foodSec2014 %>% 
  
  # Add in region category
  mutate(
    regionComb = ifelse(
      region == 2 | region == 5 | region == 6 |
        region == 12 | region == 13 | region == 15,
      "other", ifelse(region == 1, "Tigray",
                      ifelse(region == 3, "Amhara",
                             ifelse(region == 4, "Oromia",
                                    ifelse(region == 7, "SNNP", 
                                           ifelse(region == 14, "Addis Ababa", "unknown")))))),
    # Convert things to binary
    worryLackFood = ifelse(worryLackFood == 1, 1, ifelse(worryLackFood == 2, 0, NA)),
    hhSameDiet = ifelse(hhSameDiet == 1, 1, ifelse(hhSameDiet == 2, 0, NA)),
    foodShortSit = ifelse(foodShortSit == 1, 1, ifelse(foodShortSit == 2, 0, NA)),
    
    menDietDiv = ifelse(is.na(menDietDiv), 0, 
                        ifelse(menDietDiv == 1, 1,  # +1 if more diverse; -1 if less diverse; 0 if NA
                               ifelse(menDietDiv == 2, -1, 
                                      NA))),
    womenDietDiv = ifelse(is.na(womenDietDiv), 0,
                          ifelse(womenDietDiv == 1, 1,
                                 ifelse(womenDietDiv == 2, -1, 
                                        NA))),
    kidsDietDiv = ifelse(is.na(kidsDietDiv), 0,
                         ifelse(kidsDietDiv == 1, 1,
                                ifelse(kidsDietDiv == 2, -1, 
                                       NA))),
    
    janFoodShort = ifelse(janFoodShort == "X", 1, 0),
    febFoodShort = ifelse(febFoodShort  == "X", 1, 0),
    marFoodShort = ifelse(marFoodShort  == "X", 1, 0),
    aprFoodShort = ifelse(aprFoodShort == "X", 1, 0),
    mayFoodShort = ifelse(mayFoodShort == "X", 1, 0),
    juneFoodShort = ifelse(juneFoodShort == "X", 1, 0),
    julyFoodShort = ifelse(julyFoodShort == "X", 1, 0),
    augFoodShort = ifelse(augFoodShort == "X", 1, 0),
    septFoodShort = ifelse(septFoodShort == "X", 1, 0),
    octFoodShort = ifelse(octFoodShort == "X", 1, 0),
    novFoodShort = ifelse(novFoodShort == "X", 1, 0),
    decFoodShort = ifelse(decFoodShort == "X", 1, 0),
    
    daysEatBadFoodBin = ifelse(daysEatBadFood > 0 , 1, 0),  # NOTE: converts NAs to 0's.
    daysLimitVarietyBin = ifelse(daysLimitVariety > 0, 1, 0), 
    daysRedAmtBin = ifelse(daysRedAmt > 0, 1, 0), 
    daysRedNumMealsBin = ifelse(daysRedNumMeals > 0, 1, 0), 
    daysRedAdultIntakeBin = ifelse(daysRedAdultIntake > 0 , 1, 0), 
    daysBorrowFoodBin = ifelse(daysBorrowFood > 0, 1, 0),    
    daysNoFoodSupplBin = ifelse(daysNoFoodSuppl > 0, 1, 0), 
    daysFastBin = ifelse(daysFast > 0, 1, 0),
    
    # Create categorical data
    menDietDivCat = ifelse(menDietDiv == 1, "more", ifelse(menDietDiv == -1, "less", NA)),
    womenDietDivCat = ifelse(womenDietDiv == 1, "more", ifelse(womenDietDiv == -1, "less", NA)),
    kidsDietDivCat = ifelse(kidsDietDiv == 1, "more", ifelse(kidsDietDiv == -1, "less", NA)),
    causeShort1cat = ifelse(causeShort1 == 1, "drought", 
                            ifelse(causeShort1 == 2, "crop pests",
                                   ifelse(causeShort1 == 3, "small land",
                                          ifelse(causeShort1 == 4, "lack farm inputs",
                                                 ifelse(causeShort1 == 5, "lack farm tools",
                                                        ifelse(causeShort1 == 6, "food prices",
                                                               ifelse(causeShort1 == 7, "high transportation costs",
                                                                      ifelse(causeShort1 == 9, "market far away",
                                                                             ifelse(causeShort1 == 8, "no food in market",
                                                                                    ifelse(causeShort1 == 10, "floods",
                                                                                           ifelse(causeShort1 == 11, "other", NA)
                                                                                    )))))))))),
    causeShort2cat = ifelse(causeShort2 == 1, "drought", 
                            ifelse(causeShort2 == 2, "crop pests",
                                   ifelse(causeShort2 == 3, "small land",
                                          ifelse(causeShort2 == 4, "lack farm inputs",
                                                 ifelse(causeShort2 == 5, "lack farm tools",
                                                        ifelse(causeShort2 == 6, "food prices",
                                                               ifelse(causeShort2 == 7, "high transportation costs",
                                                                      ifelse(causeShort2 == 9, "market far away",
                                                                             ifelse(causeShort2 == 8, "no food in market",
                                                                                    ifelse(causeShort2 == 10, "floods",
                                                                                           ifelse(causeShort2 == 11, "other", NA)
                                                                                    )))))))))),
    causeShort3cat = ifelse(causeShort3 == 1, "drought", 
                            ifelse(causeShort3 == 2, "crop pests",
                                   ifelse(causeShort3 == 3, "small land",
                                          ifelse(causeShort3 == 4, "lack farm inputs",
                                                 ifelse(causeShort3 == 5, "lack farm tools",
                                                        ifelse(causeShort3 == 6, "food prices",
                                                               ifelse(causeShort3 == 7, "high transportation costs",
                                                                      ifelse(causeShort3 == 9, "market far away",
                                                                             ifelse(causeShort3 == 8, "no food in market",
                                                                                    ifelse(causeShort3 == 10, "floods",
                                                                                           ifelse(causeShort3 == 11, "other", NA)
                                                                                    )))))))))),
    
    
    # Create derived vars
    numMonthFoodShort = janFoodShort + febFoodShort + marFoodShort + aprFoodShort +
      mayFoodShort + juneFoodShort + julyFoodShort + augFoodShort + septFoodShort +
      octFoodShort + novFoodShort + decFoodShort) %>% 
  rowwise() %>% 
  mutate(maxDaysLimit = max(daysEatBadFood, daysLimitVariety, daysRedAmt, daysRedNumMeals,
                            daysRedAdultIntake, daysBorrowFood, daysNoFoodSuppl, daysFast)) %>%  # Maximum number of days in a given week ANY change in behaviour.
  ungroup()



setwd(masterDir)
write.csv(foodSec2014, "Dataout/foodSec2014.csv")