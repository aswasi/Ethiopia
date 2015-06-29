#_________________________________________________________________________________
# Read in and cleanup Ethiopia LS/MS data on the housing module (Section 9).
#
# When you run source('R/housingMod.r'), 6 variables are created:
# - housing2012: table containing housing module 9 from 2012.
# - housing2014: table containing housing module 9 from 2014.
# - housingQuest2012: survey questions from 2012 ('label' in Stata file)
# - housingQuest2014: survey questions from 2014 ('label' in Stata file)
# - rawHousing2012: raw table from 2012 from Stata import (includes label and labels)
# - rawHousing2014: raw table from 2014 from Stata import (includes label and labels)
#
# This function will also save two files in the "Dataout" directory:
# - housing2012 --> housing2012.csv
# - housing2014 --> housing2014.csv
#
# Note: column names for 2012/2014 are identical.
#
# Laura Hughes, lhughes@usaid.gov, June 2015
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

rawHousing2012 = read_dta('sect9_hh_w1.dta')

housingQuest2012 <- pullAttributes(rawHousing2012)

housing2012 = removeAttributes(rawHousing2012)

houseCols = c("hhID2012", "eaID2012", 
              "rural", "hhWeight", "region", "zone", "woreda", "town",
              "subcity", "kebele", "ea", "hhTrunc", "hhExist", "yrsInDwelling",
              "moInDwelling", "rentOrOwn", "numRooms", "wallMaterial", "roofMaterial",
              "floorMaterial", "typeKitchen", "ovenType", "toilet",
              "bathType", "solidWaste", "sourceWaterRainy", 
              "sourceWaterDry", "boilWater",
              "chlorinateWater", "ownOtherHome", "numHomes", "sourceLight",
              "timeElecFail", "cookingFuel")

colnames(housing2012) = houseCols

# Clean up dataset; combine regions into one.
housing2012 = housing2012 %>% 
  mutate(regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", 
                                         ifelse(region == 14, "Addis Ababa", "unknown")))))),
    toiletCat = ifelse(
      toilet == 1, "flush toilet (private)", 
      ifelse(toilet == 2, "flush toilet (shared)",
             ifelse( toilet == 3, "pit latreen (private ventilated)",
                     ifelse(toilet == 4, "pit latreen (shared ventilated)",
                            ifelse(toilet == 5, "pit latreen (private not ventilated)",
                                   ifelse(toilet == 6, "pit latreen (shared not ventilated)",
                                          ifelse(toilet == 7, "bucket",
                                                 ifelse(toilet == 8, "field/forest",
                                                        "other"
                                                 )))))))),
      waterCatDry = ifelse(
          sourceWaterDry == 1, "tap in house",
          ifelse(sourceWaterDry == 2, "private tap in compound",
                ifelse(sourceWaterDry == 3, "shared tap in compound",
                       ifelse(sourceWaterDry == 4, "communal tap outside compound",
                              ifelse(sourceWaterDry == 5, "water from retailer",
                                     ifelse(sourceWaterDry == 6, "private protected well/spring",
                                            ifelse (sourceWaterDry == 7, "shared protected well/spring",
                                                    ifelse(sourceWaterDry ==8, "unprotected well/spring",
                                                           ifelse(sourceWaterDry == 9, "river/lake/pond",
                                                                  "other"))))))))),
          waterCatRainy = ifelse(
              sourceWaterRainy == 1, "tap in house",
              ifelse(sourceWaterRainy == 2, "private tap in compound",
                    ifelse(sourceWaterRainy == 3, "shared tap in compound",
                           ifelse(sourceWaterRainy == 4, "communal tap outside compound",
                                  ifelse(sourceWaterRainy == 5, "water from retailer",
                                         ifelse(sourceWaterRainy == 6, "private protected well/spring",
                                                ifelse (sourceWaterRainy == 7, "shared protected well/spring",
                                                        ifelse(sourceWaterRainy ==8, "unprotected well/spring",
                                                               ifelse(sourceWaterRainy == 9, "river/lake/pond",
                                                                      ifelse(sourceWaterRainy == 10, "rainwater",
                                                                      "other")))))))))),
    chlorinateWater = ifelse( # Convert to binary
      chlorinateWater == 1, 1, 0),
    boilWater = ifelse( # Convert to binary
      boilWater == 1, 1, 0),
    ownOtherHome = ifelse(
      ownOtherHome == 1, 1, 0)
  ) %>% 
  mutate(anyWaterTreat = ifelse(
    chlorinateWater == 1 | boilWater == 1, 1, 0
  ))

setwd(masterDir)
write.csv(housing2012, "Dataout/housing2012.csv")


#_________________________________________________________________________________
#_________________________________________________________________________________
# 2014
#_________________________________________________________________________________
setwd(masterDir)
setwd("Datain/wave2014/")

#_________________________________________________________________________________
# READ in data.
#_________________________________________________________________________________

rawHousing2014 = read_dta('sect9_hh_w2.dta')

housingQuest2014 <- pullAttributes(rawHousing2014)

housing2014 = removeAttributes(rawHousing2014)

houseCols = c("hhID2012", "hhID2014", "eaID2012", "eaID2014",
              "rural", "hhWeight", "region", "zone", "woreda", "town",
              "subcity", "kebele", "ea", "hhTrunc", "yrsInDwelling",
              "moInDwelling", "rentOrOwn", "numRooms", "wallMaterial", "roofMaterial",
              "floorMaterial", "typeKitchen", "ovenType", "toilet",
              "bathType", "solidWaste", "sourceWaterRainy", "time2WaterRainy",
              "sourceWaterDry", "time2WaterDry", "costWater", "boilWater",
              "chlorinateWater", "ownOtherHome", "numHomes", "sourceLight",
              "sourceElec", "costElec", "timeElecFail", "ElecFailLastWeek", "cookingFuel",
              "ownCellPhone", "costPhone")

colnames(housing2014) = houseCols

# Clean up dataset; combine regions into one.
housing2014 = housing2014 %>% 
  mutate(regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", 
                                         ifelse(region == 14, "Addis Ababa", "unknown")))))),
    toiletCat = ifelse(
      toilet == 1, "flush toilet (private)", 
      ifelse(toilet == 2, "flush toilet (shared)",
             ifelse( toilet == 3, "pit latreen (private ventilated)",
                     ifelse(toilet == 4, "pit latreen (shared ventilated)",
                            ifelse(toilet == 5, "pit latreen (private not ventilated)",
                                   ifelse(toilet == 6, "pit latreen (shared not ventilated)",
                                          ifelse(toilet == 7, "bucket",
                                                 ifelse(toilet == 8, "field/forest",
                                                        "other"
                                                 )))))))),
    waterCatDry = ifelse(
        sourceWaterDry == 1, "tap in house",
        ifelse(sourceWaterDry == 2, "private tap in compound",
              ifelse(sourceWaterDry == 3, "shared tap in compound",
                     ifelse(sourceWaterDry == 4, "communal tap outside compound",
                            ifelse(sourceWaterDry == 5, "water from retailer",
                                   ifelse(sourceWaterDry == 6, "private protected well/spring",
                                          ifelse (sourceWaterDry == 7, "shared protected well/spring",
                                                  ifelse(sourceWaterDry ==8, "unprotected well/spring",
                                                         ifelse(sourceWaterDry == 9, "river/lake/pond",
                                                                "other"))))))))),
    waterCatRainy = ifelse(
        sourceWaterRainy == 1, "tap in house",
        ifelse(sourceWaterRainy == 2, "private tap in compound",
              ifelse(sourceWaterRainy == 3, "shared tap in compound",
                     ifelse(sourceWaterRainy == 4, "communal tap outside compound",
                            ifelse(sourceWaterRainy == 5, "water from retailer",
                                   ifelse(sourceWaterRainy == 6, "private protected well/spring",
                                          ifelse (sourceWaterRainy == 7, "shared protected well/spring",
                                                  ifelse(sourceWaterRainy ==8, "unprotected well/spring",
                                                         ifelse(sourceWaterRainy == 9, "river/lake/pond",
                                                                ifelse(sourceWaterRainy == 10, "rainwater",
                                                                       "other")))))))))),
    chlorinateWater = ifelse( # Convert to binary
      chlorinateWater == 1, 1, 0),
    boilWater = ifelse( # Convert to binary
      boilWater == 1, 1, 0),
    ownOtherHome = ifelse(
      ownOtherHome == 1, 1, 0),
    ownCellPhone = ifelse(
      ownCellPhone == 1, 1, 0)
  ) %>% 
  mutate(anyWaterTreat = ifelse(
    chlorinateWater == 1 | boilWater == 1, 1, 0
  ))

setwd(masterDir)
write.csv(housing2014, "Dataout/housing2014.csv")
