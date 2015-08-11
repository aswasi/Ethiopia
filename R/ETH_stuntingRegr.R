
setwd("~/GitHub/Ethiopia/")

source("R/setupFncns.r")


# Load data ---------------------------------------------------------------
setwd("~/GitHub/Ethiopia/")

childRaw = read_dta("Data/ETH_201508_Child_Analysis.dta")


childHealth = removeAttributes(childRaw)

childHealth = childHealth %>% 
  select(-X_merge, -hid) %>% 
  mutate(sex = ifelse(
    gender == 1, 'male', 
    ifelse(
      gender == 2, 'female', NA
    )
  ),
  regionName = ifelse(
    saq01 == 2 , "Afar",
    ifelse(saq01 == 5, "Somalie", 
           ifelse(saq01 == 6, "Benshagul Gumuz",
                  ifelse(saq01 == 12, "Gambella",
                         ifelse(saq01 == 13, "Harari",
                                ifelse(saq01 == 15, "Diredawa",
                                       ifelse(saq01 == 1, "Tigray",
                                              ifelse(saq01 == 3, "Amhara",
                                                     ifelse(saq01 == 4, "Oromia",
                                                            ifelse(saq01 == 7, "SNNP", 
                                                                   ifelse(saq01 == 14, "Addis Ababa", "unknown")
                                                            ))))))))))
  )



# Calculate Modified HFIAS ---------------------------------------------------------
# Based on FANTA Guidelines on food security
# (http://www.fantaproject.org/sites/default/files/resources/HFIAS_ENG_v3_Aug07.pdf)
#
# DIFFERENCES: 
# - In Ethiopia LS/MS, the questions were asked relative to the past WEEK, whereas FANTA uses past MONTH.
# Therefore, assuming all answers extrapolate.
# --> any answer of 1 - 2x in the past week is assumed to be equivalent to the answer 'sometimes' 
#     in the FANTA survey (3-10x in past 4 weeks)
# --> any answer of > 3 x in past week is tagged as "often" (> 10 x in past 4 weeks)
# --> any answer of 0 in past week assumed to extrapolate to entire month.
# 
# - In ETH LS/MS, the question "Did you or anyone in hh go to sleep hungry at night?" wasn't asked.  
#   Disregarded in calculations.
# - Two additional questions were asked:
#   1) Did adults reduce their intake?
#   2) Was any food borrowed?
#   These questions were converted into the same monthly estimate, but were only used in the HFIAS score, not HFIAS category.

childHealth = childHealth %>% 
  select(-q1_HFIAS, -q2_HFIAS, -q2a_HFIAS, -q3_HFIAS, -q3a_HFIAS, -q4_HFIAS,
         -q4a_HFIAS, -q5_HFIAS, -q5a_HFIAS, -q6_HFIAS, -q6a_HFIAS, -q7_HFIAS,
         -q7a_HFIAS, -q8_HFIAS, -q8a_HFIAS, -q9_HFIAS, -q9a_HFIAS) %>% 
  mutate(q1_HFIAS = ifelse(worryLackFood == 1, 2, 
                           ifelse(worryLackFood == 0, 0, NA)),
         q2_HFIAS = ifelse(daysEatBadFood == 0, 0, 
                           ifelse(daysEatBadFood == 1 | daysEatBadFood == 2, 2,
                                  ifelse(daysEatBadFood > 2, 3, NA))),
         q3_HFIAS = ifelse(daysLimitVariety == 0, 0, 
                           ifelse(daysLimitVariety == 1 | daysLimitVariety == 2, 2,
                                  ifelse(daysLimitVariety > 2, 3, NA))),
         q5_HFIAS = ifelse(daysRedAmt == 0, 0, 
                           ifelse(daysRedAmt == 1 | daysRedAmt == 2, 2,
                                  ifelse(daysRedAmt > 2, 3, NA))),
         q6_HFIAS = ifelse(daysRedNumMeals == 0, 0, 
                           ifelse(daysRedNumMeals == 1 | daysRedNumMeals == 2, 2,
                                  ifelse(daysRedNumMeals > 2, 3, NA))),
         q7_HFIAS = ifelse(daysNoFoodSuppl == 0, 0, 
                           ifelse(daysNoFoodSuppl == 1 | daysNoFoodSuppl == 2, 2,
                                  ifelse(daysNoFoodSuppl > 2, 3, NA))),
         q9_HFIAS = ifelse(daysFast == 0, 0, 
                           ifelse(daysFast == 1 | daysFast == 2, 2,
                                  ifelse(daysFast > 2, 3, NA))),
         q5b_HFIAS = ifelse(daysRedAdultIntake == 0, 0, 
                            ifelse(daysRedAdultIntake == 1 | daysRedAdultIntake == 2, 2,
                                   ifelse(daysRedAdultIntake > 2, 3, NA))),
         q20_HFIAS = ifelse(daysBorrowFood == 0, 0, 
                            ifelse(daysBorrowFood  == 1 | daysBorrowFood  == 2, 2,
                                   ifelse(daysBorrowFood  > 2, 3, NA))),
         wtModHFIASscore = (q1_HFIAS + q2_HFIAS + q3_HFIAS + q5b_HFIAS + q5_HFIAS +
                              q7_HFIAS + q9_HFIAS + q20_HFIAS),
         modHFIAS_cat = ifelse(q1_HFIAS < 2 & wtModHFIASscore < 2, 1, # food secure
                               ifelse(q2_HFIAS < 3 | 
                                        q3_HFIAS == 1 |
                                        q5_HFIAS == 0 & 
                                        q6_HFIAS == 0 &
                                        q7_HFIAS == 0 & 
                                        q9_HFIAS == 0, 2, # mildly food insecure
                                      ifelse(q5_HFIAS < 3 |
                                               q6_HFIAS < 3 &
                                               q7_HFIAS == 0 &
                                               q9_HFIAS == 0, 3, # moderately food insecure
                                             4) # severely food insecure
                               ))) %>%  
  ungroup()


childHealthPanel = childHealth %>% 
  filter(!is.na(year), !is.na(stunted), 
         ptrack == 2)



vars2test = c(
  # diet
  'fcsMin', 'fcsCatMin', 'dietDiv', "cereal_days",                   
  "starch_days", "veg_days", "fruit_days",                    
  "meat_days", "eggs_days", "fish_days",                     
  "legumes_days","milk_days", "fats_days",                     
  "sweet_days", "cond_days", "meat2_days",                    
  "oil_days", "staples_days",  "avgNumMealsKids", "kidsDietDiv", 
  "wtModHFIASscore", # weighted by the monthly extrapolation
  "modHFIAS_cat", # rough categorization of insecurity severity.
  "modHFIAS_score", # sum of the food sec vars (q2-9)
  "hfiasindex_urb", "hfiasindex_rur" , # PCA on entire sample (panel + not)
  "worryLackFood", "daysEatBadFood",
  "daysLimitVariety", "daysRedAmt", "daysRedNumMeals",               
  "daysRedAdultIntake", "daysBorrowFood", "daysNoFoodSuppl",  "daysFast", "maxDaysLimit",                                 
  "daysEatBadFoodBin",             
  "daysLimitVarietyBin", "daysRedAmtBin", "daysRedNumMealsBin",            
  "daysRedAdultIntakeBin","daysBorrowFoodBin","daysNoFoodSupplBin",            
  "daysFastBin", "foodShortSit", "numMonthFoodShort", "causeShort1","causeShort2",
  "causeShort3",      
  
  # wealth
  "wealthPanel", # raw PCA score on only the panel
  "wealthIndex", #raw PCA, entire sample                   
  "wlthSmooth", #deciles 
  "wealthQuints", #quintiles
  "iddirMemb",
  
  # shocks
  'rptShock' ,'assetShk', 'priceShk', 'hazardShk', 'crimeShk',
  'employShk', 'goodcope', 'badcope', 'othcope', "negCommShk",                    
  "posCommShk", "totNegShkComm", "totPosShkComm",                 
  "agShkComm", "hazardShkComm", "priceShkComm",
  "devprojShk", "psnpShk", "employShkComm",                 
  "healthShkComm", "healthFacComm", "infraShkComm", "educShkComm",                    
  
  # demographics
  'gender', 'region', 'saq01', 'rural', 'ftfzone', 'ageCat', 'ageMonths', 'ageYrs',
  'relig', 'religHoh', 'religSpouse', 'mxdreligHH', 
  
  
  # family
  "bioDadinHH", "bioDadJob",   "dadbioHoh", "mombioSpouse", "mombioHoh",  
  "occupHoh", "occupSpouse",
  "bioMominHH", "bioMomJob", "hhsize",
  "femhead", "ageSpouse", 'agehead',   'ageheadsq', # life expect
  "msize", 
  "fsize" ,"gendMix","under5",                        
  "under5m" ,"under5f","under15" ,                      
  "under15m","under15f", "under24"  ,                     
  "under24m", "under24f","youth15to24" ,                  
  "youth15to24m", "youth15to24f", "youth18to30",                   
  "youth18to30m", "youth18to30f","youth25to35" ,                  
  "youth25to35m","youth25to35f","over35under65",                 
  "over35under65m","over35under65f", "over64",                        
  "over64m","over64f", "femCount20_34","femCount35_59","femRatio20_34",                 
  "marriedHoh", "marriedHohm" , "marriedHohp",  'depRatio', #ratio <14, >65 as pct          
  "vulnHead",
  
  # education
  "literateHoh", "literateSpouse", "educHoh", "educSpouse",
  "educAdult", "educAdultM",
  "bioMomEduc", "bioDadEduc", "educAdultF",                     
  
  # time            
  'year',  
  
  # environment / agriculture
  'dist_popcenter', 'dist_market', 'dist_road', 'dist_admctr', 
  'areaField', "fsrad3_agpct", # % agriculture in the area
  "fsrad3_lcmaj",  "landQtile",
  "TLUtotal", "landOwn",
  
  # house
  "homeMaterial","mudHome", "stoneHome","metalRoof", "thatchRoof", "mudFloor", "noKitchen","indoorKitchen",                 
  "dungFuel", "sanitWaste", 
  "crowding", #rooms per capita
  "wasteFert", "wasteThrow",                                    
  "lightType", "elecLight",  "fireLight", "noElect", "electricity",
  "numRooms",                      
  "wallMaterial","roofMaterial", "floorMaterial",                 
  "typeKitchen","ovenType",                                            
  "bathType", "sourceLight", "timeElecFail", "cookingFuel",                     
  "ElecFailLastWeek",              
  
  
  # water
  "totWaterTime", "spouseWaterTime", "headWaterTime", "sourceWaterRainy", "sourceWaterDry",
  "protWaterRainy","protWaterDry", "protWaterAll", #protected water
  "treatWater", "waterRainyTime", "waterDryTime",  "boilWater", "chlorinateWater","waterCatDry",
  "waterCatRainy", "anyWaterTreat", "time2WaterRainy","time2WaterDry",
  
  # toilets
  "hasToilet", "toiletType", "wasteDisposal","flushToilet","noToilet",  "toilet",   "solidWaste",                     
  
  # distractions (primarily for education regressions)
  "totFirewdTime","spouseFireTime", "headFireTime",    "totAgTime","totNonagTime", "workedFree",  # worked somewhere else for free
  "hhlabor", "mlabor",                        
  "flabor", "mlaborShare", "flaborShare",  'borrowed',                    
  
  # health
  "respInfection", "illness", "totIllness", "malariaHH", "diarrheaHH",   "sickdays",                      
  "sickTreat", "treatType", "illnessType",
  "healthFacElec", "hospWthMedPers", "distNrstDoc", "freeBedNetTrt",                  
  "chDiarrhea")

y= c('stunting', 'wasting', 'stunted', 'wasted' ,'underweight', 'underwgt')

# Pre-regression: correlation exploration ---------------------------------
avgStunt = mean(childHealthPanel$stunting)


# Just for a quick glimpse... note: not even close to being accurate, 
# since weighting each category equally.  But a quick check, 
# and also to see where I need to clean up / remove variables.
stuntingAvg_byVar = NA

for (i in 1:length(vars2test)) {
  x = childHealthPanel  %>% 
    group_by_(vars2test[i])  %>% 
    summarise(avg = mean(stunting))  %>% 
    mutate(ratio = avg / avgStunt)
  
  numNA = sum(is.na(childHealthPanel %>% select_(vars2test[i])))
  
  stuntingAvg_byVar = rbind(stuntingAvg_byVar, data.frame(var = vars2test[i], ratio = sd(x$ratio), na = numNA, nObs = nrow(childHealthPanel) - numNA))
}

# Select ze data.
panelStunted = childHealthPanel %>% select(stunted, one_of(vars2test))
panelWasted = childHealthPanel %>% select(wasted, one_of(vars2test))
panelStunting = childHealthPanel %>% select(stunting, one_of(vars2test))
panelWasting = childHealthPanel %>% select(wasting, one_of(vars2test))

stuntedCor = cor(panelStunted, use = 'pairwise.complete.obs')
d3heatmap(stuntedCor[,1:2])


# Dataset cleanup, model selection ---------------------------------------------------------

# Remove variables with few observations; convert NAs to 0s where appropriate
# Fewer than 

# Set factors to appropriate levels


vars2remove = stuntingAvg_byVar %>% 
  filter(nObs < 4000) %>% 
  select(var)

panelStunted1 = panelStunted %>% 
  select(-one_of(t(vars2remove)))

panelWasted1 = panelWasted %>% 
  select(-one_of(t(vars2remove)))

panelStunted2 = scale(na.omit(panelStunted1))
panelWasted1 = na.omit(panelWasted1)

panelStunted2 = data.frame(panelStunted2) %>% 
  mutate(stunted = ifelse(stunted > 1, 1, 0))

# Regressions: stunted ---------------------------------------------------
stuntedRegr = glm(stunted ~ ., data = panelStunted2, family = binomial(link = "logit"))

wastedRegr = glm(wasted ~ ., data = panelWasted1, family = binomial(link = "logit"))

# Run cart to see which vars have greatest discrimination power
# RF model?

# Sensitivity analysis ----------------------------------------------------
coefplot(wastedRegr)
# What happens if add in non-panel data?
# What happens if fit ea. year separately?
