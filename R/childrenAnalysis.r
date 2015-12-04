
# Introduction ------------------------------------------------------------
# Within the Ethiopia LSMS panel data between 2011/2012 and 2013/2014, 
# we noticed inconsistencies in children's ages and sexes between years.
# Children reported as being male in the first panel would be reported as 
# female in the second year, and their ages would change by an amount much
# smaller or larger than 24 months.
#
# This script is an attempt to document and, if possible, correct those data.
#
# Laura Hughes, November 2015, lhughes@usaid.gov
# https://github.com/flaneuse
# 
# Data source: http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:23406371~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html




# Import cleaned data (to harvest ptrack, etc.) ------------------------------
source("~/GitHub/Ethiopia/R/setupFncns.r") 
source("~/GitHub/Ethiopia/R/loadETHpanel.r")


# 2012 --------------------------------------------------------------------

setwd("~/Documents/USAID/Ethiopia/Datain/wave2012/")

#_________________________________________________________________________________
# READ in raw data: cover
#_________________________________________________________________________________

rawCover2012 = read_dta('sect_cover_hh_w1.dta')

coverQuest2012 <- pullAttributes(rawCover2012)

cover2012 = removeAttributes(rawCover2012)


#_________________________________________________________________________________
# READ in data: health (--> stunting)
#_________________________________________________________________________________

rawHealth2012 = read_dta('sect3_hh_w1.dta')

healthQuest2012 <- pullAttributes(rawHealth2012)

health2012 = removeAttributes(rawHealth2012)


#_________________________________________________________________________________
# READ in data: stunting
#_________________________________________________________________________________

rawRoster2012 = read_dta('sect1_hh_w1.dta')

rosterQuest2012 <- pullAttributes(rawRoster2012)

roster2012 = removeAttributes(rawRoster2012)


# 2014 --------------------------------------------------------------------

setwd("~/Documents/USAID/Ethiopia/Datain/wave2014/")

#_________________________________________________________________________________
# READ in data: cover
#_________________________________________________________________________________

rawCover2014 = read_dta('sect_cover_hh_w2.dta')

coverQuest2014 <- pullAttributes(rawCover2014)

cover2014 = removeAttributes(rawCover2014)


#_________________________________________________________________________________
# READ in data: health (--> stunting)
#_________________________________________________________________________________

rawHealth2014 = read_dta('sect3_hh_w2.dta')

healthQuest2014 <- pullAttributes(rawHealth2014)

health2014 = removeAttributes(rawHealth2014)


#_________________________________________________________________________________
# READ in data: roster
#_________________________________________________________________________________

rawRoster2014 = read_dta('sect1_hh_w2.dta')

rosterQuest2014 <- pullAttributes(rawRoster2014)

roster2014 = removeAttributes(rawRoster2014)



# Pull out relevant vars --------------------------------------------------
health2012 = health2012 %>% 
  mutate(household_id = as.character(household_id), individual_id = as.character(individual_id)) %>% 
  select(household_id, individual_id,
         ea_id = ea_id, hhID_health = hh_s3q00,
         under5 = hh_s3q20, birthDay_health = hh_s3q21_a, 
         birthMonth_health = hh_s3q21_b, birthYear_health = hh_s3q21_c,
         weight = hh_s3q22, height = hh_s3q23, resultMeas = hh_s3q24) %>% 
  mutate(year = 2012) %>% 
  filter(!is.na(weight)) # Filter out only those children who have a reported weight.

health2014 = health2014 %>% 
  mutate(household_id = as.character(household_id), individual_id = as.character(individual_id), 
         household_id2 = as.character(household_id2), individual_id2 = as.character(individual_id2)) %>% 
  select(household_id, household_id2, individual_id, individual_id2,
         ea_id = ea_id, ea_id2 = ea_id2, hhID2_health = hh_s3q00, 
         under5 = hh_s3q20, birthDay_health = hh_s3q21_a, 
         birthMonth_health = hh_s3q21_b, birthYear_health = hh_s3q21_c,
         weight = hh_s3q22, height = hh_s3q23, resultMeas = hh_s3q24) %>% 
  mutate(year = 2014) %>% 
  filter(!is.na(weight)) # Filter out only those children who have a reported weight.

cover2012 = cover2012 %>% 
  mutate(household_id = as.character(household_id)) %>% 
select(household_id,
           ea_id = ea_id,  hhID_cover = saq08,
         hhSize = hh_saq09, 
         interview1Day = hh_saq13_a, interview1Month = hh_saq13_b, interview1Year = hh_saq13_c,
         interview2Day = hh_saq17_a, interview2Month = hh_saq17_b, interview2Year = hh_saq17_c,
         interview3Day = hh_saq21_a, interview3Month = hh_saq21_b, interview3Year = hh_saq21_c,
         missing1Cover = hh_saq16_a, missing1Roster = hh_saq16_b, missing1Health = hh_saq16_d,
         missing2Cover = hh_saq20_a, missing2Roster = hh_saq20_b, missing2Health = hh_saq20_d,
         missing3Cover = hh_saq24_a, missing3Roster = hh_saq24_b, missing3Health = hh_saq24_d) %>% 
  mutate(year = 2012, 
         interviewYearHealth = ifelse(missing3Health == 'X', NA,
                                      ifelse(missing2Health == 'X', interview3Year,
                                             ifelse(missing1Health == 'X', interview2Year,
                                                    interview1Year))),
         interviewMonthHealth = ifelse(missing3Health == 'X', NA,
                                       ifelse(missing2Health == 'X', interview3Month,
                                              ifelse(missing1Health == 'X', interview2Month,
                                                     interview1Month)))
  ) 

cover2014 = cover2014 %>% 
  mutate(household_id = as.character(household_id), 
         household_id2 = as.character(household_id2)) %>% 
  select(household_id, household_id2,
         ea_id = ea_id, ea_id2 = ea_id2, hhID2_cover = saq08,
         hhSize = hh_saq09, 
         interview1Day = hh_saq13_a, interview1Month = hh_saq13_b, interview1Year = hh_saq13_c,
         interview2Day = hh_saq17_a, interview2Month = hh_saq17_b, interview2Year = hh_saq17_c,
         interview3Day = hh_saq21_a, interview3Month = hh_saq21_b, interview3Year = hh_saq21_c,
         missing1Cover = hh_saq16_a, missing1Roster = hh_saq16_b, missing1Health = hh_saq16_d,
         missing2Cover = hh_saq20_a, missing2Roster = hh_saq20_b, missing2Health = hh_saq20_d,
         missing3Cover = hh_saq24_a, missing3Roster = hh_saq24_b, missing3Health = hh_saq24_d) %>% 
  mutate(year = 2014,          
         interviewYearHealth = ifelse(missing3Health == 'X', NA,
                                      ifelse(missing2Health == 'X', interview3Year,
                                             ifelse(missing1Health == 'X', interview2Year,
                                                    interview1Year))),
         interviewMonthHealth = ifelse(missing3Health == 'X', NA,
                                       ifelse(missing2Health == 'X', interview3Month,
                                              ifelse(missing1Health == 'X', interview2Month,
                                                     interview1Month)))) 

roster2012= roster2012 %>% 
  mutate(household_id = as.character(household_id), individual_id = as.character(individual_id)) %>% 
  select(household_id, individual_id,
         ea_id = ea_id,  hhID_roster = saq08,
         hhMemberID_roster = hh_s1q00, rel2Head = hh_s1q02, sex = hh_s1q03,
         ageYrs_roster = hh_s1q04_a, ageMonths_roster = hh_s1q04_b) %>% 
  mutate(year = 2012) 


roster2014 = roster2014 %>% 
  mutate(household_id = as.character(household_id), individual_id = as.character(individual_id), 
         household_id2 = as.character(household_id2), individual_id2 = as.character(individual_id2)) %>% 
  select(household_id, household_id2, individual_id, individual_id2,
         ea_id = ea_id, ea_id2 = ea_id2, hhID2_roster = saq08,
         hhMemberID_roster = hh_s1q00, rel2Head = hh_s1q02, sex = hh_s1q03,
         ageYrs_roster = hh_s1q04_a, ageMonths_roster = hh_s1q04_b, 
         sexCorrect = hh_s1q04d, correctedSex = hh_s1q04e, ageCorrect = hh_s1q04f,
         birthDay_roster = hh_s1q04g_1,
         birthMonth_roster = hh_s1q04g_2, birthYear_roster = hh_s1q04g_3,
         correctedAge = hh_s1q04h) %>% 
  mutate(year = 2014) 



# Select out just the households in the panel -----------------------------
# panel  = data  %>% select(household_id, household_id2, ea_id, ea_id2, year) %>% 
#   mutate(ptrack = TRUE)
# 
# 
# cover2014 = full_join(panel, cover2014, by = c("household_id" = "household_id", 
#                                                "household_id2" = "household_id2",
#                                                "ea_id" = "ea_id",
#                                                "ea_id2" = "ea_id2", 
#                                                "year" = "year")) %>% 
#   filter(year == 2014, ptrack == TRUE)
# 
# cover2012 = full_join(panel, cover2012, by = c("household_id" = "household_id", 
#                                                "ea_id" = "ea_id",
#                                                "year" = "year")) %>% 
#   filter(year == 2012, ptrack == TRUE)
# 
# 
# 
# roster2014 = full_join(panel, roster2014, by = c("household_id" = "household_id", 
#                                                  "household_id2" = "household_id2",
#                                                  "ea_id" = "ea_id",
#                                                  "ea_id2" = "ea_id2", 
#                                                  "year" = "year")) %>% 
#   filter(year == 2014, ptrack == TRUE, ageYrs_roster < 10 | is.na(ageYrs_roster))
# 
# 
# roster2012 = full_join(panel, roster2012, by = c("household_id" = "household_id", 
#                                                  "ea_id" = "ea_id",
#                                                  "year" = "year")) %>% 
#   filter(year == 2012, ptrack == TRUE, ageYrs_roster < 10 | is.na(ageYrs_roster))
# 
# 
# 
# 
# health2014 = full_join(panel, health2014, by = c("household_id" = "household_id", 
#                                                  "household_id2" = "household_id2",
#                                                  "ea_id" = "ea_id",
#                                                  "ea_id2" = "ea_id2", 
#                                                  "year" = "year")) %>% 
#   filter(year == 2014, ptrack == TRUE)
# 
# 
# health2012 = full_join(panel, health2012, by = c("household_id" = "household_id", 
#                                                  "ea_id" = "ea_id",
#                                                  "year" = "year")) %>% 
#   filter(year == 2012, ptrack == TRUE)



# merge individual datasets together --------------------------------------

all2012 = left_join(health2012, cover2012, by = c("household_id" = "household_id","ea_id" = "ea_id",  "year"))

all2012 = left_join(all2012, roster2012, by = c("household_id" = "household_id", "ea_id" = "ea_id", "year",
                                                "individual_id" = "individual_id"))


all2014 = left_join(health2014, cover2014, by = c("household_id" = "household_id", "household_id2" = "household_id2",
                                                  "ea_id" = "ea_id", "ea_id2" = "ea_id2", "year"))

all2014 = left_join(all2014, roster2014, by = c("household_id" = "household_id", "household_id2" = "household_id2", 
                                                "ea_id" = "ea_id", "ea_id2" = "ea_id2", "year", 
                                                "individual_id" = "individual_id", "individual_id2"= "individual_id2"))

# Calculate a rough age, based on the health data. ------------------------
# Ethiopian Amharic calendars are annoyingly different than Gregorian.  As a first pass, assuming dates are roughly equivalent to 
# Gregorian ones.

# Also correct the heights: ~ 100ish a year are physically unreasonable.

all2014 = all2014 %>% 
  mutate(ageMonthsEst = interviewMonthHealth - birthMonth_health + 12*(interviewYearHealth - birthYear_health),
         ageMonth_roster_total = ifelse(is.na(ageYrs_roster), ageMonths_roster, # Convert age on the roster to a total number of months.
                                        ifelse(is.na(ageMonths_roster), ageYrs_roster *12,
                                               ageMonths_roster + 12* ageYrs_roster)),
         diffAge = ageMonthsEst - ageMonth_roster_total, # Difference in age b/w the estimate and what's reported on the roster.
         agesAgree = abs(diffAge) <= 2,
         diffCorr = abs(correctedAge * 12 - ageMonthsEst) <= 2,
         correctedHeight = ifelse(height < 1.4, height * 100,
                                  ifelse(height > 140, height / 10, height)), # Fix heights that make no sense. 
         correctedHeight = ifelse(correctedHeight < 140 & correctedHeight > 40, correctedHeight, NA) # Double check heights are within a reasonable range.
  ) %>% 
  select(household_id, household_id2, year, individual_id, individual_id2,
         sex, sexCorrect, correctedSex, birthMonth_roster, birthMonth_health,
         ageYrs_roster, ageMonths_roster, ageMonth_roster_total, ageMonthsEst, 
         ageCorrect, correctedAge, diffAge, agesAgree, diffCorr, weight, height, correctedHeight)


all2012 = all2012 %>% 
  mutate(ageMonthsEst = interviewMonthHealth - birthMonth_health + 12*(interviewYearHealth - birthYear_health),
         ageMonth_roster_total = ifelse(is.na(ageYrs_roster), ageMonths_roster,
                                        ifelse(is.na(ageMonths_roster), ageYrs_roster *12,
                                               ageMonths_roster + 12* ageYrs_roster)),
         diffAge = ageMonthsEst - ageMonth_roster_total,
         agesAgree = abs(diffAge) <= 2) %>% 
  select(household_id, year, individual_id, 
         sex, weight, height,
         ageYrs_roster, ageMonths_roster, ageMonth_roster_total, ageMonthsEst, 
         diffAge, agesAgree, birthMonth_health)


# Merge two years together ------------------------------------------------

children = full_join(all2012, all2014, 
                     by = c("household_id", "individual_id"))

children = children %>% 
  mutate(ageGap = ageMonth_roster_total.x - ageMonth_roster_total.y,
         estGap = ageMonthsEst.x - ageMonthsEst.y,
         largeGap = abs(ageGap) > 30 | abs(ageGap) < 18,
         largeEst = abs(estGap) > 30 | abs(estGap) < 18) %>% 
  # filter(largeGap == TRUE) %>% 
  select(household_id, household_id2, individual_id, individual_id2, 
         ageYrs_roster.x, ageMonths_roster.x, ageMonth_roster_total.x,
         ageMonthsEst.x, ageYrs_roster.y, ageMonths_roster.y, ageMonth_roster_total.y, ageMonthsEst.y,
         ageGap, estGap, largeEst, largeGap, agesAgree.x, agesAgree.y, sexCorrect) %>% 
  mutate(agesAgree = ifelse(agesAgree.x == FALSE | agesAgree.y == FALSE, FALSE, TRUE))

ggplot(children, aes(x = ageMonth_roster_total.x, y = ageMonth_roster_total.y,
                     colour = factor(agesAgree))) + 
  geom_point(size = 3,  alpha  = 0.2)+theme_box_ygrid()+ xlab('2012') + ylab('2014') + 
  theme(axis.title.y = element_text()) +
  coord_cartesian(ylim = c(0, 100))


# Okay.  These kids aren’t alright.  Let’s filter out just the kid --------
maybeOkay = children %>% 
  filter(largeGap == FALSE, sexCorrect != 2) %>% 
  select(household_id, household_id2, individual_id, individual_id2)

x = child %>% 
  filter(individual_id %in% maybeOkay$individual_id)




# Examining the child dataset ---------------------------------------------
stuntingDB = child  %>% 
  select(household_id, household_id2, 
         individual_id, individual_id2, year, ageMonths,
         stunting, stunted) %>% 
  spread(year, ageMonths)


#   group_by(household_id, household_id2, individual_id, individual_id2, year) %>% 
#   mutate(age_lagged = lag(ageMonths, order_by = year))
#   

#   mutate(TLU_lagged = lag(TLUtotal, order_by = year))



# Is height reasonable? ---------------------------------------------------
# Source for comparison: http://www.who.int/childgrowth/en/
# Using girls for lower bound: http://www.who.int/childgrowth/standards/sft_lhfa_girls_p_0_2.pdf?ua=1
# At birth, 3rd percentile  == 45.6 cm
# And boys for upper bound: http://www.who.int/childgrowth/standards/sft_lhfa_boys_p_2_5.pdf?ua=1, http://www.cdc.gov/growthcharts/html_charts/statage.htm
# 118.7 cm @ 5 y, 138.7 @ 8 y.
# Therefore: correcting if it's < 1.4 (presumably 1.4 m), or > 140 (presumably > 140 mm)
heightData = all2014 %>% 
  mutate(heightLab = ifelse(height < 45 | height > 140, '#2166ac', '#abd9e9'))

ggplot(heightData, aes(x = height, fill = heightLab)) + 
  geom_histogram(binwidth = 5) +
  scale_fill_identity() +
  theme_jointplot()



ggplot(heightData, aes(x = ageMonthsEst, y = height, colour = heightLab)) + 
  geom_point(size = 4, alpha = 0.2) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0, 90)) +
  theme_jointplot() +
  coord_cartesian(ylim = c(0, 140))


ggplot(heightData, aes(x = ageMonthsEst, y = correctedHeight, colour = heightLab)) + 
  geom_point(size = 4, alpha = 0.2) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0, 90)) +
  theme_jointplot() +
  coord_cartesian(ylim = c(0, 140))

ggplot(child, aes(x = ageM, y = cheight, colour = factor(stunted))) + 
  geom_point(size = 4, alpha = 0.2) +
  scale_x_continuous(limits = c(0, 90)) +
  theme_jointplot() +
  coord_cartesian(ylim = c(0, 140))


# Do the ages agree? ------------------------------------------------------

ggplot(x, aes(x = ageMonth_roster_total, y = ageMonthsEst)) + geom_point(size = 3, alpha = 0.3) + theme_jointplot() +
  coord_cartesian(ylim = c(0, 100))


