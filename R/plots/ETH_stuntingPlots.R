# Plots on stunting in Ethiopia, from LS/MS analysis.


# Colors ------------------------------------------------------------------
male="#6495ED"
female="#F08080"

# Setup functions ---------------------------------------------------------


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


# Pre-regression: correlation exploration ---------------------------------
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

avgStunt = mean(childHealthPanel$stunting)


# Just for a quick glimpse... note: not even c
stuntingAvg_byVar = NA

for (i in 1:length(vars2test)) {
  x = childHealthPanel  %>% 
    group_by_(vars2test[i])  %>% 
    summarise(avg = mean(stunting))  %>% 
    mutate(ratio = avg / avgStunt)
  
  numNA = sum(is.na(childHealthPanel %>% select_(vars2test[i])))
  
  stuntingAvg_byVar = rbind(stuntingAvg_byVar, data.frame(var = vars2test[i], ratio = sd(x$ratio), na = numNA))
}

# Regressions: stunting ---------------------------------------------------
# Select ze data.
panelStunted = childHealthPanel %>% select(stunted, one_of(vars2test))

stuntedCor = cor(panelStunted, use = 'pairwise.complete.obs')
d3heatmap(stuntedCor[,1:2])

# Stunt01 - male / female over time ---------------------------------------

jointplot = function(data, title,
                     yStunted = nrow(data)/2 - 300,
                     yNotStunted = nrow(data)/2 - 200){
  
  main = ggplot(data, aes(x = ageMonths, y = stunted, 
                          color = factor(sex),
                          fill = factor(sex))) +
    ggtitle(title) +
    theme_jointplot() +
    scale_color_manual(values = c('male' = male, 'female' = female), name = 'gender') +
    scale_fill_manual(values = c('male' = male, 'female' = female), name = 'gender') +
    ylab('proportion stunted') +
    # facet_wrap(~ year) +
    geom_smooth(method = 'loess', span = 1, alpha = 0.15) +
    coord_cartesian(ylim = c(0,1), xlim = c(0,60))
  
  
  
  empty = ggplot() + 
    geom_point(aes(x= 1, y = 1), colour = 'white') +
    theme_blankLH()
  
  
  
  xDistrib = ggplot(data, aes(x = ageMonths, 
                              color = factor(sex),
                              fill = factor(sex))) +
    theme_blankbox() +
    # facet_wrap(~ year) +
    geom_density(alpha = 0.2) +
    # geom_histogram(aes(y = ..density..), alpha = 0.2, position = 'dodge') +
    scale_color_manual(values = c('male' = male, 'female' = female)) +
    scale_fill_manual(values = c('male' = male, 'female' = female)) +
    coord_cartesian(xlim = c(0,60))
  
  yDistrib = ggplot(data, aes(x = stunted, 
                              color = factor(sex),
                              fill = factor(sex))) +
    scale_color_manual(values = c('male' = male, 'female' = female)) +
    scale_fill_manual(values = c('male' = male, 'female' = female)) +
    theme_blankLH() +
    coord_flip(xlim = c(-0.1, 1.2)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    annotate('text', x = 0.15, y = yNotStunted, label = 'not stunted', size = 6) +
    annotate('text', x = 1.15, y = yStunted, label = 'stunted', size = 6) +
    geom_bar(binwidth = 0.1, position = 'dodge')
  
  grid.arrange(xDistrib, empty, main, yDistrib, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1,4), clip = TRUE)
}



jointplot(childHealthPanel %>% filter(year == 2012), title = '2012', yStunted = 250, yNotStunted = 300)

jointplot(childHealthPanel %>% filter(year == 2014), title = '2014', yStunted = 150, yNotStunted = 200)




# Stunt02 - cohort tracking -----------------------------------------------
cohortStunting = childHealthPanel %>% 
  select(individual_id, stunting, year, sex) %>% 
  spread(year, stunting) %>% 
  filter(!is.na(`2012`), !is.na(`2014`)) %>% 
  mutate(diff = `2014` - `2012`)

cohortStunted = childHealthPanel %>% 
  select(individual_id, stunted, year, sex) %>% 
  spread(year, stunted) %>% 
  filter(!is.na(`2012`), !is.na(`2014`)) %>% 
  mutate(diffCat = `2014` - `2012`) %>% 
  mutate(changeCat = ifelse(
    diffCat == 1, 'stunted',
    ifelse(diffCat == -1, 'not stunted', 'no change')
  ))

cohort = full_join(cohortStunting, cohortStunted, by = c("individual_id", "sex"))

ggplot(cohort, aes(x = `2012.x`, y = changeCat, colour = sex)) +
  geom_vline(xint = -2, colour = 'purple', linetype = 2) +
  geom_point(size = 5, alpha = 0.3) +  
  theme_laura() + 
  xlab('2012 stunting z-score') +
  ylab('change in status')



ggplot(cohortStunting) +
  geom_point(aes(x = `2012`, y = `2014`, colour = sex),
             size = 5, alpha = 0.3) +
  scale_color_manual(values = c('male' = male, 'female' = female)) +
  theme_jointplot()

ggplot(cohortStunting) +
  geom_point(aes(x = `2012`, y = diff))

ggplot(cohortStunting) +
  geom_point(aes(x = `2012`, y = `2014`, colour = sex),
             size = 5, alpha = 0.3) +
  scale_color_manual(values = c('male' = male, 'female' = female)) +
  theme_jointplot()

ggplot(cohortStunted) +
  geom_jitter(aes(x = `2012`, y = `2014`, colour = sex),
              size = 5, alpha = 0.3) +
  scale_color_manual(values = c('male' = male, 'female' = female)) +
  theme_jointplot()


child12 = childHealthPanel %>% 
  filter(year == 2012)

child14 = childHealthPanel %>% 
  filter(year == 2014)

ggplot() +
  geom_smooth(aes(x = ageMonths + 24, y = stunted), data = child12) +
  geom_smooth(aes(x = ageMonths, y = stunted), data = child14, color = 'red')+
  coord_cartesian(xlim = c(30, 60), ylim = c(0, 0.6))


# Stunt03 - regions over time ---------------------------------------

ggplot(childHealthPanel, aes(x = ageMonths, y = stunted, 
                             color = factor(year),
                             fill = factor(year))) +
  theme_jointplot() +
  facet_wrap(~ gender) +
  geom_smooth(method = 'loess', span = 1, alpha = 0.1) +
  coord_cartesian(ylim = c(0,1)) 



ggplot(childHealthPanel, aes(x = ageMonths, y = stunted, 
                             color = factor(year),
                             fill = factor(year))) +
  theme_jointplot() +
  facet_wrap(~ region) +
  geom_smooth(method = 'loess', span = 1, alpha = 0.15) +
  coord_cartesian(ylim = c(0,1)) 




