source("~/GitHub/Ethiopia/R/setupFncns.r") 


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
  mutate(year = 2012)

health2014 = health2014 %>% 
  select(household_id_health = household_id, individual_id_health = individual_id, 
         household_id2_health = household_id2, individual_id2_health = individual_id2, 
         ea_id_health = ea_id, ea_id2_health = ea_id2, hhID2_health = hh_s3q00, 
         under5 = hh_s3q20, birthDay_health = hh_s3q21_a, 
         birthMonth_health = hh_s3q21_b, birthYear_health = hh_s3q21_c,
         weight = hh_s3q22, height = hh_s3q23, resultMeas = hh_s3q24) %>% 
  mutate(year = 2014)

cover2012 = cover2012 %>% 
  select(household_id_cover = household_id, 
         ea_id_cover = ea_id,  hhID_cover = saq08,
         hhSize = hh_saq09, 
         interview1Day = hh_saq13_a, interview1Month = hh_saq13_b, interview1Year = hh_saq13_c,
         interview2Day = hh_saq17_a, interview2Month = hh_saq17_b, interview2Year = hh_saq17_c,
         interview3Day = hh_saq21_a, interview3Month = hh_saq21_b, interview3Year = hh_saq21_c,
         missing1Cover = hh_saq16_a, missing1Roster = hh_saq16_b, missing1Health = hh_saq16_d,
         missing2Cover = hh_saq20_a, missing2Roster = hh_saq20_b, missing2Health = hh_saq20_d,
         missing3Cover = hh_saq24_a, missing3Roster = hh_saq24_b, missing3Health = hh_saq24_d)

cover2014 = cover2014 %>% 
  select(household_id_cover = household_id, household_id2_cover = household_id2,
         ea_id_cover = ea_id, ea_id2_cover = ea_id2, hhID2_cover = saq08,
         hhSize = hh_saq09, 
         interview1Day = hh_saq13_a, interview1Month = hh_saq13_b, interview1Year = hh_saq13_c,
         interview2Day = hh_saq17_a, interview2Month = hh_saq17_b, interview2Year = hh_saq17_c,
         interview3Day = hh_saq21_a, interview3Month = hh_saq21_b, interview3Year = hh_saq21_c,
         missing1Cover = hh_saq16_a, missing1Roster = hh_saq16_b, missing1Health = hh_saq16_d,
         missing2Cover = hh_saq20_a, missing2Roster = hh_saq20_b, missing2Health = hh_saq20_d,
         missing3Cover = hh_saq24_a, missing3Roster = hh_saq24_b, missing3Health = hh_saq24_d)

roster2012= roster2012 %>% 
  select(household_id_roster = household_id, individual_id_roster = individual_id, 
         ea_id_roster = ea_id,  hhID_roster = saq08,
         hhMemberID_roster = hh_s1q00, rel2Head = hh_s1q02, sex = hh_s1q03,
         ageYrs_roster = hh_s1q04_a, ageMonths_roster = hh_s1q04_b, 
         over10 = hh_s1q06)


roster2014 = roster2014 %>% 
  select(household_id_roster = household_id, individual_id_roster = individual_id, 
                  household_id2_roster = household_id2, individual_id2_roster = individual_id2, 
         ea_id_roster = ea_id, ea_id2_roster = ea_id2, hhID2_roster = saq08,
        hhMemberID_roster = hh_s1q00, rel2Head = hh_s1q02, sex = hh_s1q03,
        ageYrs_roster = hh_s1q04_a, ageMonths_roster = hh_s1q04_b, 
        sexCorrect = hh_s1q04d, correctedSex = hh_s1q04e, ageCorrect = hh_s1q04f,
        birthDay_roster = hh_s1q04g_1,
        birthMonth_roster = hh_s1q04g_2, birthYear_roster = hh_s1q04g_3,
        correctedAge = hh_s1q04h, over10 = hh_s1q06)



# merge by year -----------------------------------------------------------

indiv2012 = full_join(cover2012, roster2012, 
                      by = c("household_id_cover" = "household_id_roster",
                             "ea_id_cover" = "ea_id_roster", "hhID_cover" = "hhID_roster"))

# loses one?
indiv2012 = full_join(indiv2012, health2012, 
                      by = c("household_id_cover" = "household_id_health"))
                             # "ea_id_cover" = "ea_id_health", "hhID_cover" = "hhID_health"))

# Calculate age -----------------------------------------------------------

# -- Convert from Eth. calendar --



# Test merge --------------------------------------------------------------
health = rbind(health2012, health2014)

health %>% 
  group_by(household_id_health, individual_id_health, year) %>% 
  mutate(lag(weight))

x  = full_join(health2012, health2014, by =  c("household_id_health", "individual_id_health", 
                                               "ea_id_health", "hhID_health"))
