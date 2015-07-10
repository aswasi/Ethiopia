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


# 2012 --------------------------------------------------------------------
setwd("Datain/wave2012/")

#!! Note: area conversion factors differ by region, zone, and woreda.


#_________________________________________________________________________________
# READ in data.
#_________________________________________________________________________________

rawPP1_2012 = read_dta('sect1_pp_w1.dta')
rawPP2_2012 = read_dta('sect2_pp_w1.dta')
rawPP3_2012 = read_dta('sect3_pp_w1.dta')
rawAreaConv = read_dta("ET_local_area_unit_conversion.dta")

Q32012 = pullAttributes(rawPP3_2012)
Q22012 = pullAttributes(rawPP2_2012)
Q12012 = pullAttributes(rawPP1_2012)


ag32012 = removeAttributes(rawPP3_2012)
ag22012 = removeAttributes(rawPP2_2012)
ag12012 = removeAttributes(rawPP1_2012)
areaConv = removeAttributes(rawAreaConv) # Area conversions
areaConv = areaConv %>% select(-zonename, -woredaname)


# MERGING ALL RELEVANT QUESTIONS TOGETHER.
# For everything: keeping hhID2012, hhID2012, indivID2012, indivID2012, parcel and other crop IDs.


# Section 1: Demographics -------------------------------------------------
# at the individual level.  Keeping only farm type (Q4) for the hh level and associated basic stats.
ag12012 = ag12012 %>% 
  select(holderID = holder_id, hhID2012 = household_id,  
         indivID2012 = individual_id, region = saq01,
         age = pp_s1q02, sex = pp_s1q03, farmType = pp_s1q04, holder = pp_s1q05) %>% 
  
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
    holder = ifelse(holder == "X", 1, 0),
    
    # Create categorical data
    sex = ifelse(sex == 1, "male", 
                 ifelse(sex == 2, "female", NA)),
    farmTypeCat = ifelse(farmType == 1, "crop",
                         ifelse(farmType == 2, "livestock",
                                ifelse(farmType == 3, "both",
                                       ifelse(farmType == 4, "none", NA))))
    # Create derived vars
  )





# Section 2 - Land Ownership ----------------------------------------------

#  at the parcel level.  Keeping Q3 (where got land)
ag22012 =  ag22012 %>% 
  select(holderID = holder_id, hhID2012 = household_id,  
         region = saq01, parcelID = parcel_id, 
         numFields = pp_s2q02, howGotParcel = pp_s2q03, 
         certificate = pp_s2q04) %>% 
  mutate(    
    regionComb = ifelse(
      region == 2 | region == 5 | region == 6 |
        region == 12 | region == 13 | region == 15,
      "other", ifelse(region == 1, "Tigray",
                      ifelse(region == 3, "Amhara",
                             ifelse(region == 4, "Oromia",
                                    ifelse(region == 7, "SNNP", 
                                           ifelse(region == 14, "Addis Ababa", "unknown")))))),
    # Convert things to binary)
    certificate = ifelse(certificate == 1, 1, 
                         ifelse(certificate == 2, 0, NA)),
    tenured = ifelse(howGotParcel < 4, 1, 0),
    
    # Categorical data
    howGotParcelCat = ifelse(howGotParcel == 1, "granted by local leaders",
                             ifelse(howGotParcel == 2, "inherited",
                                    ifelse(howGotParcel == 3, "rent",
                                           ifelse(howGotParcel == 4, "borrowed for free",
                                                  ifelse(howGotParcel == 5 | howGotParcel == 10, "moved in without permission",
                                                         ifelse(howGotParcel == 6 | howGotParcel == 11, "other", NA))))))
  )




# Section 3 - Land Area ---------------------------------------------------
# at the parcel level.

# ignoring parcels with land units "other" since no easy way to convert them.
ag32012 = ag32012 %>% 
  select(holderID = holder_id, hhID2012 = household_id, 
         region = saq01, zone = saq02, woreda = saq03, 
         parcelID = parcel_id, fieldID = field_id, 
         areaFieldSelf = pp_s3q02_a, areaSelfUnit = pp_s3q02_c, #!!
         statusField = pp_s3q03,
         GPS = pp_s3q04, 
         areaGPS = pp_s3q05_a, #!!
         ropeCompass = pp_s3q08_a, areaRC = pp_s3q08_b, 
         extPgrm = pp_s3q11, irrig = pp_s3q12, irrigSource = pp_s3q13,
         fertilizer = pp_s3q14, urea = pp_s3q15, DAP = pp_s3q18,
         manure = pp_s3q21, compost = pp_s3q23, orgFert = pp_s3q25) %>% 
  
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
    GPS = ifelse(GPS == 1, 1, 
                 ifelse(GPS == 2, 0, NA)),
    ropeCompass = ifelse(ropeCompass == 1, 1,
                         ifelse(ropeCompass == 2, 0, NA)),
    extPgrm = ifelse(extPgrm == 1, 1, 
                     ifelse(extPgrm == 2, 0, NA)),
    irrig = ifelse(irrig == 1, 1, 
                   ifelse(irrig == 2, 0, NA)),
    fertilizer = ifelse(fertilizer == 1, 1,
                        ifelse(fertilizer == 2, 0, NA)),
    urea = ifelse(urea == 1, 1,
                  ifelse(urea == 2, 0, NA)),
    DAP = ifelse(DAP == 1, 1, 
                 ifelse(DAP == 2, 0, NA)),
    manure = ifelse(manure == 1, 1,
                    ifelse(manure == 2, 0, NA)),
    compost = ifelse(compost == 1, 1,
                     ifelse(compost == 2, 0, NA)),
    orgFert = ifelse(orgFert == 1, 1, 
                     ifelse(orgFert == 2, 0, NA)),
    
    
    # Create categorical data
    statusFieldCat = ifelse(statusField == 1, "purestand",
                            ifelse(statusField == 2, "mixed crop",
                                   ifelse(statusField == 3, "pasture",
                                          ifelse(statusField == 4, "fallow",
                                                 ifelse(statusField == 5, "forest",
                                                        ifelse(statusField == 6, "prepared for belg season",
                                                               ifelse(statusField == 7, "other", NA))))))),
    irrigSourceCat = ifelse(irrigSource == 1, "river",
                            ifelse(irrigSource == 2, "lake",
                                   ifelse(irrigSource == 3, "pond",
                                          ifelse(irrigSource == 4, "harvested water",
                                                 ifelse(irrigSource == 5, "other", NA)))))
  )

# Create derived vars

# Convert area into m^2.
allRegions = ag32012  %>% 
  group_by(region, zone, woreda)  %>% 
  summarise(num = n()) %>% 
  select(-num)


# Add in conversions for hectacres and m^2.
ha = allRegions %>% mutate(local_unit = 1, conversion = 10000)
mSq = allRegions %>% mutate(local_unit = 2, conversion = 1)

areaConv = rbind(areaConv, ha, mSq)

# Merge areas into a single value.  Rope/compass used where possible; then GPS, then self-reported.
area2012 = left_join(ag32012, areaConv, 
                     by = c("region" = "region", "zone" = "zone", "woreda" = "woreda", "areaSelfUnit" = "local_unit")) %>% 
  mutate(areaSelfm2 = areaFieldSelf * conversion, 
         areaField = ifelse(ropeCompass == 1, areaRC,
                            ifelse(GPS == 1, areaGPS,
                                   ifelse(!is.na(areaSelfm2), areaSelfm2, NA)))
  )



# tester = area2012 %>% mutate(selfReported = ifelse(is.na(areaFieldSelf), 0, 1), 
#                              GPSorRC = ifelse(GPS == 1 | ropeCompass == 1,1,0))
# 
# tester %>% group_by(selfReported, GPSorRC) %>% summarise(num = n(), pct = percent(num/nrow(tester)))
# tester %>% group_by(selfReported, GPS, ropeCompass) %>% summarise(num = n(), pct = percent(num/nrow(tester)))
# 94.6% have either GPS or rope/compass.

# Comparison between self-reported area, GPS, rope and compass.
# Conclusion: self-reported area seems to be wholely unreliable, so it won't be used.

ggplot(area2012 %>% filter(GPS == 1, ropeCompass == 1), aes(x = areaGPS, y = areaRC)) +
  geom_point(alpha = 0.2, color = 'blue', size = 4) +
  theme_laura() +
  ylab('area measured by rope and compass') +
  xlab('area measured by GPS')


ggplot(area2012 %>% filter(GPS == 1, !is.na(areaSelfm2)), 
       aes(x = areaGPS, y = areaSelfm2, color = areaSelfUnit)) +
  geom_point(alpha = 0.5, size = 4) +
  scale_color_continuous(low = 'yellow', high = 'blue') +
  theme_laura() +
  ylab('area reported by self') +
  xlab('area measured by GPS') +
  theme(aspect.ratio = 1) +
  coord_cartesian(ylim = c(0, 1e5), xlim = c(0, 1e5))


ggplot(area2012 %>% filter(ropeCompass == 1, !is.na(areaSelfm2)), 
       aes(x = areaRC, y = areaSelfm2, color = areaSelfUnit)) +
  geom_point(alpha = 0.5, size = 4) +
  scale_color_continuous(low = 'yellow', high = 'blue') +
  theme_laura() +
  ylab('area reported by self') +
  xlab('area measured by RC') +
  theme(aspect.ratio= 1)
coord_cartesian(ylim = c(0, 1e4), xlim = c(0, 1e4))

ggplot(area2012 %>% filter(areaGPS < 1e4), aes(x=areaGPS)) +geom_histogram(binwidth = 100) 



ggplot(area2012 %>% filter(GPS == 1, ropeCompass == 1), aes(x = log10(areaGPS/areaRC))) +
  geom_histogram() +
  theme_laura()


ggplot(area2012, aes(x = log10(areaField))) +
  geom_histogram() +
  theme_laura()



# household rollup --------------------------------------------------------

# Calculate number of plots per household.
# Calculate average size of plots
# 
# holdersPerPlot = area2012 %>% 
#   count(hhID2012, holderID) # Very few dual holders.

# 3,118 unique hh in total.
hhArea2012 = area2012 %>% 
  group_by(hhID2012, region, regionComb) %>% 
  summarise(avgField = mean(areaField, na.rm = TRUE), 
            areaField = sum(areaField, na.rm = TRUE),
            irrig = sum(irrig, na.rm = TRUE),
            fertilizer = sum(fertilizer, na.rm = TRUE),
            urea = sum(urea, na.rm = TRUE),
            DAP = sum(DAP, na.rm = TRUE),
            manure = sum(manure, na.rm = TRUE),
            compost = sum(compost, na.rm = TRUE),
            orgFert = sum(orgFert, na.rm = TRUE),
            extPgrm = sum(extPgrm, na.rm = TRUE),
            numFields = n())

# Mutations for input variables.
hhArea2012 = hhArea2012 %>% 
  mutate(anyInorg = ifelse(sum(urea + DAP, na.rm = TRUE) > 0, 1, 0),
         anyOrg = ifelse(sum(manure + compost + orgFert, na.rm = TRUE) > 0, 1, 0),
         fertPct = fertilizer / numFields,
         ureaPct = urea / numFields,
         DAPpct = DAP / numFields,
         manurePct = manure / numFields
  )


# Calculate land tenure variables.
# 3,118 hh
# Tenured defined as: inherited, rented, or granted by local leaders; 
# Claimed as: moved in w/o permission, borrowed for free;
# NA: no response or "other"

hhLandTenure2012 = ag22012 %>% 
  group_by(hhID2012, hhID2012, region, regionComb) %>% 
  summarise(
    certificate = sum(certificate, na.rm = TRUE),
    tenured = sum(tenured, na.rm = TRUE),
    numParcels = n()) %>% 
  mutate(
    pctCert = certificate / numParcels,
    pctTenured = tenured / numParcels
  )


# Merge together
indivAg2012 = full_join(ag12012, ag22012, 
                        by = c("holderID", "hhID2012", "region", "regionComb"))

indivAg2012 = full_join(indivAg2012, area2012, 
                        c("holderID", "hhID2012",  "region", "regionComb", "parcelID"))


hhAg2012 = full_join(hhLandTenure2012, hhArea2012, 
                     by = c("hhID2012", "region", "regionComb"))

# Save 2012 vars ----------------------------------------------------------
rm(ha, mSq)

setwd(masterDir)
write.csv(hhAg2012, "Dataout/hhAg2012.csv")
write.csv(indivAg2012, "Dataout/indivAg2012.csv")




# 2014 --------------------------------------------------------------------

setwd("Datain/wave2014/")

#_________________________________________________________________________________
# READ in data.
#_________________________________________________________________________________

rawPP1_2014 = read_dta('sect1_pp_w2.dta')
rawPP2_2014 = read_dta('sect2_pp_w2.dta')
rawPP3_2014 = read_dta('sect3_pp_w2.dta')
# rawPP4_2014 = read_dta('sect4_pp_w2.dta')
# rawPP5_2014 = read_dta('sect5_pp_w2.dta')
# rawPP7_2014 = read_dta('sect7_pp_w2.dta')

# Q72014 = pullAttributes(rawPP7_2014)
# Q52014 = pullAttributes(rawPP5_2014)
# Q42014 = pullAttributes(rawPP4_2014)
Q32014 = pullAttributes(rawPP3_2014)
Q22014 = pullAttributes(rawPP2_2014)
Q12014 = pullAttributes(rawPP1_2014)


# ag72014 = removeAttributes(rawPP7_2014)
# ag52014 = removeAttributes(rawPP5_2014)
# ag42014 = removeAttributes(rawPP4_2014)
ag32014 = removeAttributes(rawPP3_2014)
ag22014 = removeAttributes(rawPP2_2014)
ag12014 = removeAttributes(rawPP1_2014)


# MERGING ALL RELEVANT QUESTIONS TOGETHER.
# For everything: keeping hhID2012, hhID2014, indivID2012, indivID2014, parcel and other crop IDs.


# Section 1: Demographics -------------------------------------------------
# at the individual level.  Keeping only farm type (Q4) for the hh level and associated basic stats.
ag12014 = ag12014 %>% 
  select(holderID = holder_id, hhID2012 = household_id, hhID2014 = household_id2, 
         indivID2012 = individual_id, indivID2014 = individual_id2, region = saq01,
         age = pp_s1q02, sex = pp_s1q03, farmType = pp_s1q04, holder = pp_s1q05) %>% 
  
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
    holder = ifelse(holder == "X", 1, 0),
    
    # Create categorical data
    sex = ifelse(sex == 1, "male", 
                 ifelse(sex == 2, "female", NA)),
    farmTypeCat = ifelse(farmType == 1, "crop",
                         ifelse(farmType == 2, "livestock",
                                ifelse(farmType == 3, "both",
                                       ifelse(farmType == 4, "none", NA))))
    # Create derived vars
  )





# Section 2 - Land Ownership ----------------------------------------------

#  at the parcel level.  Keeping Q3 (where got land)
ag22014 =  ag22014 %>% 
  select(holderID = holder_id, hhID2012 = household_id, hhID2014 = household_id2, 
         region = saq01, parcelID = parcel_id, parcelOwnRent = pp_s2q01b, 
         numFields = pp_s2q02, howGotParcel = pp_s2q03, right2Sell = pp_s2q03b,
         certificate = pp_s2q04) %>% 
  mutate(    
    regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", 
                                         ifelse(region == 14, "Addis Ababa", "unknown")))))),
    # Convert things to binary)
    parcelOwnRent = ifelse(parcelOwnRent == 1, 1, 
                           ifelse(parcelOwnRent == 2, 0, NA)),
    right2Sell = ifelse(right2Sell == 1, 1, 
                        ifelse(right2Sell == 2, 0, NA)),
    certificate = ifelse(certificate == 1, 1, 
                         ifelse(certificate == 2, 0, NA)),
    tenured = ifelse(howGotParcel < 4, 1, 0),
    
    # Categorical data
    howGotParcelCat = ifelse(howGotParcel == 1, "granted by local leaders",
                             ifelse(howGotParcel == 2, "inherited",
                                    ifelse(howGotParcel == 3, "rent",
                                           ifelse(howGotParcel == 4, "borrowed for free",
                                                  ifelse(howGotParcel == 5, "moved in without permission",
                                                         ifelse(howGotParcel == 6, "other", NA))))))
)




# Section 3 - Land Area ---------------------------------------------------
# at the parcel level.

# ignoring parcels with land units "other" since no easy way to convert them.
ag32014 = ag32014 %>% 
  select(holderID = holder_id, hhID2012 = household_id, hhID2014 = household_id2, 
         region = saq01, zone = saq02, woreda = saq03, 
         parcelID = parcel_id, fieldID = field_id, 
         areaFieldSelf = pp_s3q02_a, areaSelfUnit = pp_s3q02_c, statusField = pp_s3q03,
         statusFieldOther = pp_s3q03_other, leftFallowPast10y = pp_s3q03c,
         GPS = pp_s3q04, areaGPS = pp_s3q05_a, 
         ropeCompass = pp_s3q08_a, areaRC = pp_s3q08_b, 
         extPgrm = pp_s3q11, irrig = pp_s3q12, irrigSource = pp_s3q13, irrigSourceOther = pp_s3q13_other,
         fertilizer = pp_s3q14, urea = pp_s3q15, DAP = pp_s3q18, inorgFert = pp_s3q20a, 
         manure = pp_s3q21, compost = pp_s3q23, orgFert = pp_s3q25) %>% 
  
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
    GPS = ifelse(GPS == 1, 1, 
                 ifelse(GPS == 2, 0, NA)),
    ropeCompass = ifelse(ropeCompass == 1, 1,
                         ifelse(ropeCompass == 2, 0, NA)),
    extPgrm = ifelse(extPgrm == 1, 1, 
                     ifelse(extPgrm == 2, 0, NA)),
    irrig = ifelse(irrig == 1, 1, 
                   ifelse(irrig == 2, 0, NA)),
    fertilizer = ifelse(fertilizer == 1, 1,
                        ifelse(fertilizer == 2, 0, NA)),
    urea = ifelse(urea == 1, 1,
                  ifelse(urea == 2, 0, NA)),
    DAP = ifelse(DAP == 1, 1, 
                 ifelse(DAP == 2, 0, NA)),
    inorgFert = ifelse(inorgFert == 1, 1, 
                       ifelse(inorgFert == 2, 0, NA)),
    manure = ifelse(manure == 1, 1,
                    ifelse(manure == 2, 0, NA)),
    compost = ifelse(compost == 1, 1,
                     ifelse(compost == 2, 0, NA)),
    orgFert = ifelse(orgFert == 1, 1, 
                     ifelse(orgFert == 2, 0, NA)),
    
    
    # Create categorical data
    statusFieldCat = ifelse(statusField == 1, "cultivated",
                            ifelse(statusField == 2, "pasture",
                                   ifelse(statusField == 3, "fallow",
                                          ifelse(statusField == 4, "forest",
                                                 ifelse(statusField == 5, "prepared for belg season",
                                                        ifelse(statusField == 6, "rented",
                                                               ifelse(statusField == 7, "other", NA))))))),
    irrigSourceCat = ifelse(irrigSource == 1, "river",
                            ifelse(irrigSource == 2, "lake",
                                   ifelse(irrigSource == 3, "pond",
                                          ifelse(irrigSource == 4, "harvested water",
                                                 ifelse(irrigSource == 5, "other", NA)))))
  )

# Create derived vars

# Convert area into m^2.
allRegions = ag32014  %>% 
  group_by(region, zone, woreda)  %>% 
  summarise(num = n()) %>% 
  select(-num)


# Add in conversions for hectacres and m^2.
ha = allRegions %>% mutate(local_unit = 1, conversion = 10000)
mSq = allRegions %>% mutate(local_unit = 2, conversion = 1)

areaConv = rbind(areaConv, ha, mSq)

# Merge areas into a single value.  Rope/compass used where possible; then GPS, then self-reported.
area2014 = left_join(ag32014, areaConv, 
                     by = c("region" = "region", "zone" = "zone", "woreda" = "woreda", "areaSelfUnit" = "local_unit")) %>% 
  mutate(areaSelfm2 = areaFieldSelf * conversion, 
         areaField = ifelse(ropeCompass == 1, areaRC,
                            ifelse(GPS == 1, areaGPS,
                                   ifelse(!is.na(areaSelfm2), areaSelfm2, NA)))
  )

# How many have no conversion factor?
# ! 48.8%!!! 6130/33029
# area2014 %>% filter(is.na(conversion), !is.na(areaFieldSelf)) %>% group_by(areaSelfUnit) %>% summarise(num = n()) %>% arrange(desc(num))
# areaSelfUnit  num
# 1            8 5672
# 2            3 3424
# 3            4 3322
# 4            7 2636
# 5            5  591
# 6            6  465
# 7           NA   20

# # Check the conversions were done properly.
# area2014 %>% filter(!is.na(areaSelfm2)) %>% 
#   group_by(areaSelfUnit) %>% 
#   summarise(mean = roundMean(areaSelfm2), sd = roundStd(areaSelfm2), 
#             median = median(areaSelfm2, na.rm = TRUE), min = min(areaSelfm2, na.rm = TRUE), 
#             max = max(areaSelfm2, na.rm = TRUE), num = n())
# # Self-reported
# # areaSelfUnit     mean         sd    median       min        max   num
# # 1            1 97546.82 1892465.34 10000.000   0.00000 45030000.0  1132
# # 2            2   140.67     354.77    42.000   0.10000     5000.0  2276
# # 3            3  2106.05   11554.18  1032.733   0.00000   575478.6 13389
# # 4            4  4047.20   44192.31   307.908  12.66600   776715.0   752
# # 5            5  2143.00    5635.57  1112.930  10.86001   108600.1   827
# # 6            6  4545.53   15279.64  2483.076 248.30760   214513.1   227
# 
# # By GPS
# area2014 %>% filter(GPS == 1) %>% 
#   group_by(areaSelfUnit) %>% 
#   summarise(mean = roundMean(areaGPS), sd = roundStd(areaGPS), 
#             median = median(areaGPS, na.rm = TRUE), min = min(areaGPS, na.rm = TRUE), 
#             max = max(areaGPS, na.rm = TRUE), num = n())
# # areaSelfUnit     mean       sd   median  min       max   num
# # 1            1 14281.44 44737.86 5951.560 0.00  812179.8   926
# # 2            2   491.72  4024.45   92.265 0.00   80059.0  2006
# # 3            3  2009.62 12392.11 1031.250 0.00 1264075.0 15535
# # 4            4   340.82  2650.59  175.250 0.00  146206.0  3978
# # 5            5  2214.08 11595.46 1118.610 0.06  304690.0  1318
# # 6            6  2661.43  3484.65 1836.200 0.00   48740.5   645
# # 7            7   342.62   398.22  249.350 0.00    6873.9  2546
# # 8            8   384.99  1410.20   56.320 0.00   31505.0  5493
# # 9           NA  3283.55 11562.47  633.670 4.65   75861.0    45
# 
# # GPS
# # mean       sd median min     max   num
# # 1 1679.05 11992.82  430.5   0 1264075 32492
# 
# # By rope and compass
# area2014 %>% filter(ropeCompass == 1) %>% 
#   group_by(areaSelfUnit) %>% 
#   summarise(mean = roundMean(areaRC), sd = roundStd(areaRC), 
#                                        median = median(areaRC, na.rm = TRUE), min = min(areaRC, na.rm = TRUE), 
#                                        max = max(areaRC, na.rm = TRUE), num = n())

# Rope/Compass
# mean     sd median min  max  num
# 1 120.02 376.33  58.92   0 9559 9819

# areaSelfUnit    mean      sd  median   min     max  num
# 1            1  258.50  915.00  23.800  0.80 4948.32   62
# 2            2   78.17   74.79  56.960  0.11  501.61 1314
# 3            3  281.22  726.42  99.580  0.00 9559.00 1758
# 4            4   89.45  137.40  65.690  0.71 3674.60 1989
# 5            5   67.90   55.31  50.550  2.68  190.46   55
# 6            6 1399.84 1830.11 716.430  8.09 8182.02   53
# 7            7   93.85   59.91  85.100  1.19  464.52  795
# 8            8   62.05  102.41  37.165  0.00 2190.40 3791
# 9           NA   15.48    4.35  15.485 12.41   18.56    28

# Comparison between self-reported area, GPS, rope and compass.
# tester = area2014 %>% mutate(selfReported = ifelse(is.na(areaFieldSelf), 0, 1), 
#                              GPSorRC = ifelse(GPS == 1 | ropeCompass == 1,1,0))

# tester %>% group_by(selfReported, GPSorRC) %>% summarise(num = n(), pct = percent(num/nrow(tester)))
# tester %>% group_by(selfReported, GPS, ropeCompass) %>% summarise(num = n(), pct = percent(num/nrow(tester)))
# 93.8% have either GPS or rope/compass.

# Conclusion: self-reported area seems to be wholely unreliable, so it won't be used.

ggplot(area2014 %>% filter(GPS == 1, ropeCompass == 1), aes(x = areaGPS, y = areaRC)) +
  geom_point(alpha = 0.2, color = 'blue', size = 4) +
  theme_laura() +
  ylab('area measured by rope and compass') +
  xlab('area measured by GPS')


ggplot(area2014 %>% filter(GPS == 1, !is.na(areaSelfm2)), 
       aes(x = areaGPS, y = areaSelfm2, color = areaSelfUnit)) +
  geom_point(alpha = 0.5, size = 4) +
  scale_color_continuous(low = 'yellow', high = 'blue') +
  theme_laura() +
  ylab('area reported by self') +
  xlab('area measured by GPS') +
  theme(aspect.ratio = 1) +
  coord_cartesian(ylim = c(0, 1e5), xlim = c(0, 1e5))


ggplot(area2014 %>% filter(ropeCompass == 1, !is.na(areaSelfm2)), 
       aes(x = areaRC, y = areaSelfm2, color = areaSelfUnit)) +
  geom_point(alpha = 0.5, size = 4) +
  scale_color_continuous(low = 'yellow', high = 'blue') +
  theme_laura() +
  ylab('area reported by self') +
  xlab('area measured by RC') +
  theme(aspect.ratio= 1)
coord_cartesian(ylim = c(0, 1e4), xlim = c(0, 1e4))

ggplot(area2014 %>% filter(areaGPS < 1e4), aes(x=areaGPS)) +geom_histogram(binwidth = 100) 



ggplot(area2014 %>% filter(GPS == 1, ropeCompass == 1), aes(x = log10(areaGPS/areaRC))) +
  geom_histogram() +
  theme_laura()


ggplot(area2014, aes(x = log10(areaField))) +
  geom_histogram() +
  theme_laura()



# household rollup --------------------------------------------------------

# Calculate number of plots per household.
# Calculate average size of plots
# 
# holdersPerPlot = area2014 %>% 
#   count(hhID2014, holderID) # Very few dual holders.

# 3,629 unique hh in total.
hhArea2014 = area2014 %>% 
  group_by(hhID2014, hhID2012, region, regionComb) %>% 
  summarise(avgField = mean(areaField, na.rm = TRUE), 
            areaField = sum(areaField, na.rm = TRUE),
            irrig = sum(irrig, na.rm = TRUE),
            fertilizer = sum(fertilizer, na.rm = TRUE),
            urea = sum(urea, na.rm = TRUE),
            DAP = sum(DAP, na.rm = TRUE),
            inorgFert = sum(inorgFert, na.rm = TRUE),
            manure = sum(manure, na.rm = TRUE),
            compost = sum(compost, na.rm = TRUE),
            orgFert = sum(orgFert, na.rm = TRUE),
            extPgrm = sum(extPgrm, na.rm = TRUE),
            # areaInUse = sum(areaInUse, na.rm = TRUE),
            numFields = n())

# Mutations for input variables.
hhArea2014 = hhArea2014 %>% 
  mutate(anyInorg = ifelse(sum(urea + DAP + inorgFert, na.rm = TRUE) > 0, 1, 0),
         anyOrg = ifelse(sum(manure + compost + orgFert, na.rm = TRUE) > 0, 1, 0),
         fertPct = fertilizer / numFields,
         ureaPct = urea / numFields,
         DAPpct = DAP / numFields,
         manurePct = manure / numFields
  )


# Calculate land tenure variables.
# 3,631 hh
# Tenured defined as: inherited, rented, or granted by local leaders; 
# Claimed as: moved in w/o permission, borrowed for free;
# NA: no response or "other"

hhLandTenure2014 = ag22014 %>% 
  group_by(hhID2014, hhID2012, region, regionComb) %>% 
  summarise(
    certificate = sum(certificate, na.rm = TRUE),
    right2Sell = sum(right2Sell, na.rm = TRUE),
    tenured = sum(tenured, na.rm = TRUE),
    numParcels = n()) %>% 
  mutate(
    pctCert = certificate / numParcels,
    pctRight2Sell = right2Sell / numParcels,
    pctTenured = tenured / numParcels
  )


# Merge together
indivAg2014 = full_join(ag12014, ag22014, 
                        by = c("holderID", "hhID2012", "hhID2014", "region", "regionComb"))

indivAg2014 = full_join(indivAg2014, area2014, 
                        c("holderID", "hhID2012", "hhID2014", "region", "regionComb", "parcelID"))
                        

hhAg2014 = full_join(hhLandTenure2014, hhArea2014, 
                     by = c("hhID2014", "hhID2012", "region", "regionComb"))

# Save 2014 vars ----------------------------------------------------------
rm(ha, mSq)

setwd(masterDir)
write.csv(hhAg2014, "Dataout/hhAg2014.csv")
write.csv(indivAg2014, "Dataout/indivAg2014.csv")