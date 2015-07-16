# Merge various modules from LS/MS data on Ethiopia together at the household level.
# 
# Variables:
# 
#                         - hhID2012: household ID from 2012 surveys
#                         - hhID2014: household ID from 2014 surveys
#                         - year: year of survey (2012 or 2014)
#                         - regionComb: string containing the regions in the 
#                                       survey, grouped by the 7 largest.

# Nutrition:
#                         - dd2012B:  dietary diversity from 2012, calculated
#                                     using survey module 5B (7 day recall) 
#                         - dd2014B:  dietary diversity from 2014, calculated
#                                     using survey module 5B (7 day recall)
#                         - fcs2012Min:  food consumption score from 2012, 
#                                        calculated using survey module 5B 
#                                        using the MOST conservative estimate 
#                                        of consumption.
#                         - fcs2012Max:  food consumption score from 2012, 
#                                        calculated using survey module 5B 
#                                        using the LEAST conservative estimate 
#                                        of consumption.
#                         - fcs2012MinCat: factor characterization of FCS score.
#                         - fcs2012MaxCat: factor characterization of FCS score.
#                         - fcs2014Min:  food consumption score from 2014, 
#                                        calculated using survey module 5B 
#                                        using the MOST conservative estimate 
#                                        of consumption.
#                         - fcs2014Max:  food consumption score from 2014, 
#                                        calculated using survey module 5B 
#                                        using the LEAST conservative estimate 
#                                        of consumption.
#                         - fcs2014MinCat: factor characterization of FCS score.
#                         - fcs2014MaxCat: factor characterization of FCS score.

# Food Security:
#                         - worryLackFood: binary value if the hh worried they would lack food in the last 7 days
#                         - daysEatBadFood: days in the past week the hh ate less preferred foods 
#                         - daysLimitVariety: days in the past week the hh limited the variety of food they ate
#                         - daysRedAmt: days in the past week the hh reduced the amount of food consumed
#                         - daysRedNumMeals: days in the past week the hh reduced the number of meals conusmed
#                         - daysRedAdultIntake: days in the past week the hh reduced adult food intake to provide more food for kids    
#                         - daysBorrowFood: days in the past week the hh borrowed food from friends/neighbors
#                         - daysNoFoodSuppl: days in the past week the hh had no food in the house
#                         - daysFast: days in the past week the hh went 24 h without food
#                         - avgNumMealsAdults: average number of daily meals for adults in the hh
#                         - avgNumMealsKids: average number of daily meals for kids in the hh
#                         - hhSameDiet: does the hh all eat the same diet?            
#                         - menDietDiv: categorical value: do men eat more (+1), less (-1), or the same (0) diverse diets?           
#                         - womenDietDiv: categorical value: do women eat more (+1), less (-1), or the same (0) diverse diets?          
#                         - kidsDietDiv: categorical value: do children eat more (+1), less (-1), or the same (0) diverse diets?           
#                         - foodShortSit: binary value: did the hh experience a food shortage situation in the past year?
#                         - janFoodShort: binary value: did the hh experience a food shortage situation in January?  
#                         - febFoodShort: binary value: did the hh experience a food shortage situation in February?
#                         - marFoodShort: binary value: did the hh experience a food shortage situation in March?
#                         - aprFoodShort: binary value: did the hh experience a food shortage situation in April?          
#                         - mayFoodShort: binary value: did the hh experience a food shortage situation in May?         
#                         - juneFoodShort: binary value: did the hh experience a food shortage situation in June?
#                         - julyFoodShort: binary value: did the hh experience a food shortage situation in July?
#                         - augFoodShort: binary value: did the hh experience a food shortage situation in August?
#                         - septFoodShort: binary value: did the hh experience a food shortage situation in September
#                         - octFoodShort: binary value: did the hh experience a food shortage situation in October?
#                         - novFoodShort: binary value: did the hh experience a food shortage situation in November?
#                         - decFoodShort: binary value: did the hh experience a food shortage situation in December?
#                         - causeShort1: primary cause for the food shortages over the past year
#                         - causeShort1other: primary cause for the food shortages over the past year, if "other" (2014 only)    
#                         - causeShort2: secondary cause for the food shortages over the past year
#                         - causeShort2other: secondary cause for the food shortages over the past year, if "other" (2014 only)      
#                         - causeShort3: tertiary cause for the food shortages over the past year
#                         - causeShort3other: tertiary cause for the food shortages over the past year, if "other" (2014 only)     
#                         - daysEatBadFoodBin: binary value if the hh ate less preferred foods in the past week
#                         - daysLimitVarietyBin: binary value if the hh ate less varied foods in the past week
#                         - daysRedAmtBin: binary value if the hh reduced the amount of foods consumed in the past week
#                         - daysRedNumMealsBin: binary value if the hh reduced the number of meals in the past week
#                         - daysRedAdultIntakeBin: binary value if the hh reduced adult consumption in the past week
#                         - daysBorrowFoodBin: binary value if the hh borrowed food in the past week
#                         - daysNoFoodSupplBin: binary value if the hh lacked food in the house in the past week
#                         - daysFastBin: binary value if the hh fasted in the past week
#                         - menDietDivCat: categorical data if the men in the hh eat more, less, or same diverse diets
#                         - womenDietDivCat: categorical data if the women in the hh eat more, less, or same diverse diets       
#                         - kidsDietDivCat: categorical data if the kids in the hh eat more, less, or same diverse diets        
#                         - causeShort1cat: categorical data for causeShort1 (see LS/MS documentation for codes)        
#                         - causeShort2cat: categorical data for causeShort2 (see LS/MS documentation for codes)        
#                         - causeShort3cat: categorical data for causeShort3 (see LS/MS documentation for codes)       
#                         - numMonthFoodShort: number of months over the past year with at least one food shortage situation      
#                         - maxDaysLimit: maximum number of days in the past week the hh changed their food consumption (from "days..." vars)  

# Agriculture:            -"certificate": number of parcels containing a certificate of ownership.

                          
#                         - "right2Sell": number of parcels where the hh has the right to sell them (only 2014)
#                         - "tenured": number of parcels containing tenured land--  inherited, rented, or granted by local leaders
#                         - "numParcels": number of parcels owned by the hh
#                         - "pctCert": percent of parcels with a certificate
#                         - "pctRight2Sell": percent of parcels with the right to sell.
#                         - "pctTenured": percent of parcels that are tenured
#                         - "avgField": average field size across all the parcels.
#                         - "areaField": sum of all the parcels sizes, in m^2
#                         - "irrig": number of parcels with irrigation 
#                         - "fertilizer": number of parcels using any type of fertilizer
#                         - "urea": number of parcels using urea          
#                         - "DAP": number of parcels using DAP
#                         - "inorgFert": number of parcels using any other inorganic fertilizer (2014 only)
#                         - "manure": number of parcels using manure       
#                         - "compost": number of parcels using compost      
#                         - "orgFert": number of parcels using other organic fertilizer       
#                         - "extPgrm": number of parcels participating in the extension program (2014 only)       
#                         - "numFields": number of fields per household
#                         - "anyInorg": binary value if the household is using any inorganic fertilizer (urea, DAP, other inorganic fertilizer) on any of their parcels      
#                         - "anyOrg": binary value if the household is using any organic fertilizer (manure, compost, other organic fertilizer) on any of their parcels 
#                         - "fertPct": percent of parcels using any fertilizer
#                         - "ureaPct": percent of parcels using urea
#                         - "DAPpct": percent of parcels using DAP
#                         - "manurePct": percent of parcels using manure

# Housing:
#                         - hhExist: did the hh exist 12 months ago? (2012 only)
#                         - "yrsInDwelling": number of years the hh lived in the dwelling
#                         - "moInDwelling": months the hh lived in the dwelling
#                         - "rentOrOwn": categorical data: does the hh rent, own, live free of rent, or other in the dwelling
#                         - "numRooms": number of rooms in the dwelling
#                         - "wallMaterial": categorical data for what the walls of the house are made of
#                         - "roofMaterial": categorical data for what the roof of the house is made of    
#                         - "floorMaterial": categorical data for what the floors are made of
#                         - "typeKitchen":  categorical data for what type of kitchen is in the house
#                         - "ovenType": oven type         
#                         - "toilet": toilet type           
#                         - "bathType": bath type         
#                         - "solidWaste": type of solid waste facilities       
#                         - "sourceWaterRainy": source of water in the rainy season
#                         - "time2WaterRainy": amount of time it takes to get water in the rainy season (2014 only) 
#                         - "sourceWaterDry": source of water in the dry season
#                         - "time2WaterDry": amount of time it takes to get water in the dry season (2014 only)    
#                         - "costWater": average cost of water (2014 only)        
#                         - "boilWater": does the hh typically boil water before drinking?
#                         - "chlorinateWater": does the hh typicallly use chemical means to purify drinking water?  
#                         - "ownOtherHome": does the hh own another home?
#                         - "numHomes": number of homes owned by hh         
#                         - "sourceLight": source of light in the hh     
#                         - "sourceElec": source of electricity (2014 only)      
#                         - "costElec": cost of electricity  (2014 only)        
#                         - "timeElecFail": frequency of electrical disruption in the past week     
#                         - "ElecFailLastWeek": hours without electricity last week (2014 only)
#                         - "cookingFuel": type of cooking fuel used      
#                         - "ownCellPhone": does anyone own a cell phone? (2014 only)    
#                         - "costPhone": cost of cell phone / landline per month (2014 only)        
#                         - "toiletCat": categorical values for toilet type       
#                         - "waterCatDry": categorical value for water source in dry season      
#                         - "waterCatRainy": categorical value for water source in the rainy season     
#                         - "anyWaterTreat": binary value if the hh boils and/or chemically treates water  


# Set working directory to where the Ethiopia data is located.
setwd("~/Documents/USAID/Ethiopia/")
source("R/setupFncns.r")
masterDir = getwd()


# Base --------------------------------------------------------------------

base = read_dta("Dataout/hh_base.dta")

base = removeAttributes(base)

baseIDs = base %>% select(hhID2012 = household_id, hhID2014 = household_id2, year)

newbase = read_dta("Datain/wave2014/Pub_ETH_HouseholdGeovars_Y2.dta")


# Agriculture (land holdings and tenure; Mods 1-3 post-planting) ----------------------------------
source("R/agriculture module/agMod.r")

# Add in necessary columns to merge
hhAg2012 = hhAg2012 %>% 
  ungroup() %>% 
  mutate(year = 2012, right2Sell = NA, pctRight2Sell = NA, inorgFert = NA)


hhAg2014 = hhAg2014 %>%
  ungroup() %>% 
  select(-hhID2012) %>% 
  mutate(year = 2014) 

# Merge with base to get hhID2012 and hhID2014 for all.
agr12 = full_join(baseIDs, hhAg2012, by = c("hhID2012", "year")) %>% 
  filter(year == 2012)
agr14 = full_join(baseIDs, hhAg2014, by = c("hhID2014", "year")) %>% 
  filter(year == 2014)

agr = rbind(agr12, agr14)



# Food Security (Mod 7) ---------------------------------------------------
setwd(masterDir)

source("R/food security module/foodSecMod.r")

foodSec2012 = foodSec2012 %>% 
  select(-eaID2012,-region, -rural, -hhWeight, -ea, -hhIDTrunc, 
         -zone, -woreda, -town, -subcity, -kebele, -regionComb) %>% 
  mutate(year = 2012, causeShort1other = NA, causeShort2other = NA, causeShort3other = NA) 
foodSec2014 = foodSec2014 %>% 
  select(-hhID2012, -eaID2012, -eaID2014, -region, -rural, -hhWeight, 
                                     -ea, -hhIDTrunc,  -zone, -woreda, -town, -subcity, -kebele, -regionComb) %>% 
  mutate(year = 2014)



foodSec2012 = full_join(baseIDs, foodSec2012, by = c("hhID2012", "year")) %>% 
                          filter(year == 2012)

foodSec2014 = full_join(baseIDs, foodSec2014, by = c("hhID2014", "year")) %>% 
  filter(year == 2014)

foodSec = rbind(foodSec2012, foodSec2014)


# Housing (Mod 9) ---------------------------------------------------------

setwd(masterDir)
source("R/housing module/housingMod.r")

housing2012 = housing2012 %>% 
  select(-hhWeight, -eaID2012, -rural, -hhTrunc, -rural, 
         -zone, -woreda, -town, -subcity, -kebele, -ea, -regionComb) %>% 
  mutate(year = 2012, time2WaterRainy = NA, time2WaterDry = NA, costWater = NA,
         sourceElec = NA, costElec = NA, ElecFailLastWeek = NA, ownCellPhone = NA, costPhone = NA)

housing2014 = housing2014 %>% select(-hhID2012, -eaID2012, -rural, -eaID2014, 
                                     -hhWeight, -hhTrunc, -rural, -zone, -woreda, -town, -subcity, -kebele, -ea, -regionComb) %>% 
  mutate(year = 2014, hhExist = NA)


housing2012 = full_join(baseIDs, housing2012, by = c("hhID2012", "year")) %>% 
                          filter(year == 2012)
                        
housing2014 = full_join(baseIDs, housing2014, by = c("hhID2014", "year")) %>% 
                          filter(year == 2014)

housing = rbind(housing2012, housing2014)


# Nutrition (Mod 5B) ---------------------------------------------------------------


source("R/nutrition analysis/calcFCSEthiopia.R")

code = "~/Documents/USAID/Ethiopia/R/nutrition analysis/"
w1 = "~/Documents/USAID/Ethiopia/Datain/wave2012"
w2 = "~/Documents/USAID/Ethiopia/Datain/wave2014"

calcFCSEthiopia(code, w1, w2)

dd2012B = dd2012B %>% 
  mutate(year = 2012)

dd2014B = dd2014B %>% 
  mutate(year = 2014)

dd2012B = full_join(baseIDs, dd2012B, by = c("hhID2012", "year")) %>% 
  mutate(dd = dd2012B) %>% 
  select(-dd2012B) %>% 
  filter(year == 2012)

dd2014B = full_join(baseIDs, dd2014B, by = c("hhID2014", "year")) %>% 
  mutate(dd = dd2014B) %>% 
  select(-dd2014B) %>% 
  filter(year == 2014)

fcs2012 = hhAggr2012 %>% 
  mutate(fcsMin = fcsMin2012, fcsMax = fcsMax2012, fcsCatMin = fcsCatMin2012, fcsCatMax = fcsCatMax2012) %>% 
  select(-fcsMin2012, -fcsCatMin2012, -fcsMax2012, -fcsCatMax2012)

fcs2014 = hhAggr2014%>% 
  mutate(fcsMin = fcsMin2014, fcsMax = fcsMax2014, fcsCatMin = fcsCatMin2014, fcsCatMax = fcsCatMax2014) %>% 
  select(-fcsMin2014, -fcsCatMin2014, -fcsMax2014, -fcsCatMax2014)
  

fcsDD12 = full_join(dd2012B, fcs2012, c("hhID2012", "Beans, lentils, nuts", "Beef, sheep, goat, or other re", "Eggs", 
                                        "Enjera (teff)", "Fish", "Fruits", "Kocho/Bula", "Milk/yogurt/cheese/other dairy", 
                                        "Oils/fats/butter", "Other cereal", "Other condiments", "Pasta, Macaroni and Biscuits", "Potatoes", "Poulty", 
                                        "Sugar or sugar products", "Vegetables", "pulses", "veg", "fruit", "milk", "sugar", "oils" = "oil"))

fcsDD14 = full_join(dd2014B, fcs2014, c("hhID2014", "Beans, lentils, nuts", "Beef, sheep, goat, or other re", "Eggs", 
                                        "Enjera (teff)", "Fish", "Fruits", "Kocho/Bula", "Milk/yogurt/cheese/other dairy", 
                                        "Oils/fats/butter", "Other cereal", "Other condiments", "Pasta, Macaroni and Biscuits", "Potatoes", "Poulty", 
                                        "Sugar or sugar products", "Vegetables", "pulses", "veg", "fruit", "milk", "sugar", "oils" = "oil"))

fcsDD = rbind(fcsDD12, fcsDD14)


# Geo ---------------------------------------------------------------------


# # Read in geo data.
# # ftfZones = read.csv("~/GitHub/Ethiopia/Data/ETH_FTF_LSMS_join.csv")
ftfZones = fread("~/GitHub/Ethiopia/Data/ETH_FTF_LSMS_join.csv")

geoData = read_dta("Dataout/geovars.dta")
geoData = removeAttributes(geoData)


# Merge in the geoData with the FtF status.

geoFtF = full_join(geoData, ftfZones, by = c("latitude" = "latitude", "longitude" = "longitude"))



# Merge -------------------------------------------------------------------

hh_laura = full_join(baseIDs, fcsDD, by = c("hhID2012", "hhID2014", "year"))
hh_laura = full_join(hh_laura, foodSec, by = c("hhID2012", "hhID2014", "year"))
hh_laura = full_join(hh_laura, agr, by = c("hhID2012", "hhID2014", "year"))
hh_laura = full_join(hh_laura, housing, c("hhID2012", "hhID2014", "year"))

hh_laura = hh_laura %>% 
  mutate(household_id = as.character(hhID2012), household_id2 = as.character(hhID2014)) %>% 
  select(-hhID2014, -hhID2012)


write_dta(hh_laura, "hh_laura.dta")

write.csv(hh_laura, "hh_laura.csv")
