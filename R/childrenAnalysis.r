
# Import full data (to harvest ptrack, etc.) ------------------------------


source("~/GitHub/Ethiopia/R/setupFncns.r") 
source("~/GitHub/Ethiopia/R/loadETHpanel.r")


# 2012 --------------------------------------------------------------------

setwd("~/Documents/USAID/Ethiopia/Datain/wave2012/")

#_________________________________________________________________________________
# READ in data: cover
#_________________________________________________________________________________

rawCover2012 = read_dta('sect_cover_hh_w1.dta')

coverQuest2012 <- pullAttributes(rawCover2012)

cover2012 = removeAttributes(rawCover2012)


#_________________________________________________________________________________
# READ in data: stunting
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
# READ in data: stunting
#_________________________________________________________________________________

rawHealth2014 = read_dta('sect3_hh_w2.dta')

healthQuest2014 <- pullAttributes(rawHealth2014)

health2014 = removeAttributes(rawHealth2014)


#_________________________________________________________________________________
# READ in data: stunting
#_________________________________________________________________________________

rawRoster2014 = read_dta('sect1_hh_w2.dta')

rosterQuest2014 <- pullAttributes(rawRoster2014)

roster2014 = removeAttributes(rawRoster2014)



# Pull out relevant vars --------------------------------------------------
health2012 = health2012 %>% 
  select(household_id_health = household_id, individual_id_health = individual_id, 
         ea_id_health = ea_id, hhID_health = hh_s3q00,
         under5 = hh_s3q20, birthDay_health = hh_s3q21_a, 
         birthMonth_health = hh_s3q21_b, birthYear_health = hh_s3q21_c,
         weight = hh_s3q22, height = hh_s3q23, resultMeas = hh_s3q24) %>% 
  mutate(year = 2012) %>% 
  filter(!is.na(weight))

health2014 = health2014 %>% 
  select(household_id_health = household_id, individual_id_health = individual_id, 
         household_id2_health = household_id2, individual_id2_health = individual_id2, 
         ea_id_health = ea_id, ea_id2_health = ea_id2, hhID2_health = hh_s3q00, 
         under5 = hh_s3q20, birthDay_health = hh_s3q21_a, 
         birthMonth_health = hh_s3q21_b, birthYear_health = hh_s3q21_c,
         weight = hh_s3q22, height = hh_s3q23, resultMeas = hh_s3q24) %>% 
  mutate(year = 2014) %>% 
  filter(!is.na(weight))

cover2012 = cover2012 %>% 
  select(household_id_cover = household_id, 
         ea_id_cover = ea_id,  hhID_cover = saq08,
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
  select(household_id_cover = household_id, household_id2_cover = household_id2,
         ea_id_cover = ea_id, ea_id2_cover = ea_id2, hhID2_cover = saq08,
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
  select(household_id_roster = household_id, individual_id_roster = individual_id, 
         ea_id_roster = ea_id,  hhID_roster = saq08,
         hhMemberID_roster = hh_s1q00, rel2Head = hh_s1q02, sex = hh_s1q03,
         ageYrs_roster = hh_s1q04_a, ageMonths_roster = hh_s1q04_b) %>% 
  mutate(year = 2012) 


roster2014 = roster2014 %>% 
  select(household_id_roster = household_id, individual_id_roster = individual_id, 
         household_id2_roster = household_id2, individual_id2_roster = individual_id2, 
         ea_id_roster = ea_id, ea_id2_roster = ea_id2, hhID2_roster = saq08,
         hhMemberID_roster = hh_s1q00, rel2Head = hh_s1q02, sex = hh_s1q03,
         ageYrs_roster = hh_s1q04_a, ageMonths_roster = hh_s1q04_b, 
         sexCorrect = hh_s1q04d, correctedSex = hh_s1q04e, ageCorrect = hh_s1q04f,
         birthDay_roster = hh_s1q04g_1,
         birthMonth_roster = hh_s1q04g_2, birthYear_roster = hh_s1q04g_3,
         correctedAge = hh_s1q04h) %>% 
  mutate(year = 2014) 



# Select out just the households in the panel -----------------------------
panel  = data  %>% select(household_id, household_id2, ea_id, ea_id2, year) %>% 
  mutate(ptrack = TRUE)


cover2014 = full_join(panel, cover2014, by = c("household_id" = "household_id_cover", 
                                               "household_id2" = "household_id2_cover",
                                               "ea_id" = "ea_id_cover",
                                               "ea_id2" = "ea_id2_cover", 
                                               "year" = "year")) %>% 
  filter(year == 2014, ptrack == TRUE)

cover2012 = full_join(panel, cover2012, by = c("household_id" = "household_id_cover", 
                                               "ea_id" = "ea_id_cover",
                                               "year" = "year")) %>% 
  filter(year == 2012, ptrack == TRUE)



roster2014 = full_join(panel, roster2014, by = c("household_id" = "household_id_roster", 
                                                 "household_id2" = "household_id2_roster",
                                                 "ea_id" = "ea_id_roster",
                                                 "ea_id2" = "ea_id2_roster", 
                                                 "year" = "year")) %>% 
  filter(year == 2014, ptrack == TRUE, ageYrs_roster < 10 | is.na(ageYrs_roster))


roster2012 = full_join(panel, roster2012, by = c("household_id" = "household_id_roster", 
                                                 "ea_id" = "ea_id_roster",
                                                 "year" = "year")) %>% 
  filter(year == 2012, ptrack == TRUE, ageYrs_roster < 10 | is.na(ageYrs_roster))




health2014 = full_join(panel, health2014, by = c("household_id" = "household_id_health", 
                                                 "household_id2" = "household_id2_health",
                                                 "ea_id" = "ea_id_health",
                                                 "ea_id2" = "ea_id2_health", 
                                                 "year" = "year")) %>% 
  filter(year == 2014, ptrack == TRUE)


health2012 = full_join(panel, health2012, by = c("household_id" = "household_id_health", 
                                                 "ea_id" = "ea_id_health",
                                                 "year" = "year")) %>% 
  filter(year == 2012, ptrack == TRUE)



# merge individual datasets together --------------------------------------

all2012 = full_join(health2012, cover2012, by = c("household_id", "household_id2", "ea_id", "ea_id2", "year", "ptrack"))

all2012 = left_join(all2012, roster2012, c("household_id", "household_id2", "ea_id", "ea_id2", "year", "ptrack",
                                           "individual_id_health" = "individual_id_roster"))


all2014 = full_join(health2014, cover2014, by = c("household_id", "household_id2", "ea_id", "ea_id2", "year", "ptrack"))

all2014 = left_join(all2014, roster2014, c("household_id", "household_id2", "ea_id", "ea_id2", "year", "ptrack",
                                           "individual_id_health" = "individual_id_roster", "individual_id2_health"= "individual_id2_roster"))

# Calculate a rough age, based on the health data. ------------------------
# Ethiopian Amharic calendars are annoyingly different than Gregorian.  As a first pass, assuming dates are roughly equivalent to 
# Gregorian ones.
all2014 = all2014 %>% 
  mutate(ageMonthsEst = interviewMonthHealth - birthMonth_health + 12*(interviewYearHealth - birthYear_health),
         ageMonth_roster_total = ifelse(is.na(ageYrs_roster), ageMonths_roster,
                                        ifelse(is.na(ageMonths_roster), ageYrs_roster *12,
                                               ageMonths_roster + 12* ageYrs_roster)),
         diffAge = ageMonthsEst - ageMonth_roster_total,
         agesAgree = abs(diffAge) <= 2,
         diffCorr = abs(correctedAge * 12 - ageMonthsEst) <= 2) %>% 
  select(household_id, household_id2, year, individual_id_health, individual_id2_health,
         sex, sexCorrect, correctedSex, birthMonth_roster, birthMonth_health,
         ageYrs_roster, ageMonths_roster, ageMonth_roster_total, ageMonthsEst, 
         ageCorrect, correctedAge, diffAge, agesAgree, diffCorr)


all2012 = all2012 %>% 
  mutate(ageMonthsEst = interviewMonthHealth - birthMonth_health + 12*(interviewYearHealth - birthYear_health),
         ageMonth_roster_total = ifelse(is.na(ageYrs_roster), ageMonths_roster,
                                        ifelse(is.na(ageMonths_roster), ageYrs_roster *12,
                                               ageMonths_roster + 12* ageYrs_roster)),
         diffAge = ageMonthsEst - ageMonth_roster_total,
         agesAgree = abs(diffAge) <= 2) %>% 
  select(household_id, year, individual_id_health, 
         sex,
         ageYrs_roster, ageMonths_roster, ageMonth_roster_total, ageMonthsEst, 
       diffAge, agesAgree, birthMonth_health)

children = full_join(all2012, all2014, 
                     by = c("household_id", "individual_id_health"))

children = children %>% 
  mutate(ageGap = ageMonth_roster_total.x - ageMonth_roster_total.y,
         estGap = ageMonthsEst.x - ageMonthsEst.y,
         largeGap = abs(ageGap) > 30 | abs(ageGap) < 18,
         largeEst = abs(estGap) > 30 | abs(estGap) < 18) %>% 
  # filter(largeGap == TRUE) %>% 
  select(household_id, household_id2, individual_id_health, individual_id2_health, 
         ageYrs_roster.x, ageMonths_roster.x, ageMonth_roster_total.x,
         ageMonthsEst.x, ageYrs_roster.y, ageMonths_roster.y, ageMonth_roster_total.y, ageMonthsEst.y,
         ageGap, estGap, largeEst, largeGap, agesAgree.x, agesAgree.y, sexCorrect) %>% 
  mutate(agesAgree = ifelse(agesAgree.x == FALSE | agesAgree.y == FALSE, FALSE, TRUE))

ggplot(children, aes(x = ageMonth_roster_total.x, y = ageMonth_roster_total.y,
       colour = factor(agesAgree))) + 
  geom_point(size = 3,  alpha  = 0.2)+theme_box_ygrid()+ xlab('2012') + ylab('2014') + theme(axis.title.y = element_text())


# Okay.  These kids aren’t alright.  Let’s filter out just the kid --------
maybeOkay = children %>% 
  filter(largeGap == FALSE, sexCorrect != 2) %>% 
  select(household_id, household_id2, individual_id_health, individual_id2_health)
  
x = child %>% 
  filter(individual_id %in% maybeOkay$individual_id_health)




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



