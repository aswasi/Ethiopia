
#_________________________________________________________________________________
# Read in and cleanup Ethiopia LS/MS data on the education module (Section 2).
#
# When you run source('R/educationMod.r'), 6 variables are created:
# - education2012: table containing education module 2 from 2012.
# - education2014: table containing education module 2 from 2014.
# - educationQuest2012: survey questions from 2012 ('label' in Stata file)
# - educationQuest2014: survey questions from 2014 ('label' in Stata file)
# - raweducation2012: raw table from 2012 from Stata import (includes label and labels)
# - raweducation2014: raw table from 2014 from Stata import (includes label and labels)
#
# This function will also save two files in the "Dataout" directory:
# - education2012 --> education2012.csv
# - education2014 --> education2014.csv
#
# Note: column names for 2012/2014 are identical.
#
# Laura Hughes, lhughes@usaid.gov, July 2015
#_________________________________________________________________________________
setwd("~/Documents/USAID/Ethiopia/")
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

raweducation2012 = read_dta('sect2_hh_w1.dta')

educationQuest2012 <- pullAttributes(raweducation2012)

education2012 = removeAttributes(raweducation2012)

eduCols = c("indivID2012", "hhID2012", "eaID2012", "rural", "hhWeight", "region", 
            "zone", "woreda", "town","subcity", "kebele", "ea", "hhIDTrunc", 
            "hhMemberID", "over5y", "literate",
            "attendedSchool", "whyGo2School", "highestGrade", "inSchool",
            "whyNotInSchool", "gradeInSchool", "schoolOrgType", "absentFromSchool",
            "whyAbsent", "howGet2School", "time2School", "scholarship",
            "amtScholarship", "amt2EduFees", "amt2EduSuppl", "goNextYr")

colnames(education2012) = eduCols

# Clean up dataset; combine regions into one.
education2012 = education2012 %>% 
  mutate(regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", 
                                         ifelse(region == 14, "Addis Ababa", "unknown")))))),
    over5y = ifelse(over5y == "X", 1, ifelse (over5y == "",0,NA)),
    # Convert things to binary
    literate = ifelse(literate == 1, 1, ifelse(literate == 2, 0, NA)),
    attendedSchool = ifelse(attendedSchool == 1, 1, ifelse(attendedSchool == 2, 0, NA)),
    inSchool = ifelse(inSchool == 1, 1, ifelse(inSchool == 2, 0, NA)),
    absentFromSchool = ifelse(absentFromSchool == 1, 1,
                              ifelse(absentFromSchool == 2, 0, NA))
  )

setwd(masterDir)
write.csv(education2012, "Dataout/education2012.csv")


#_________________________________________________________________________________
#_________________________________________________________________________________
# 2014
#_________________________________________________________________________________
setwd(masterDir)
setwd("Datain/wave2014/")

#_________________________________________________________________________________
# READ in data.
#_________________________________________________________________________________

raweducation2014 = read_dta('sect2_hh_w2.dta')

educationQuest2014 <- pullAttributes(raweducation2014)

education2014 = removeAttributes(raweducation2014)

eduCols = c("hhID2012", "hhID2014", "indivID2012", "indivID2014", 
            "eaID2012", "eaID2014","rural", "hhWeight", "region", 
            "zone", "woreda", "town","subcity", "kebele", "ea", "hhIDTrunc",
            "hhMemberID", "hhMemberName", "over5y", "literate",
            "attendedSchool", "whyGo2School", "highestGrade", "inSchool",
            "whyNotInSchool", "gradeInSchool", "schoolOrgType", "absentFromSchool",
            "whyAbsent", "howGet2School", "time2School", "scholarship",
            "amtScholarship", "amt2EduFees", "amt2EduSuppl", "goNextYr")

colnames(education2014) = eduCols

# Clean up dataset; combine regions into one.
education2014 = education2014 %>% 
  mutate(regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", 
                                         ifelse(region == 14, "Addis Ababa", "unknown")))))),
    over5y = ifelse(over5y == "X", 1, ifelse (over5y == "",0,NA)),
    # Convert things to binary
    literate = ifelse(literate == 1, 1, ifelse(literate == 2, 0, NA)),
    attendedSchool = ifelse(attendedSchool == 1, 1, ifelse(attendedSchool == 2, 0, NA)),
    inSchool = ifelse(inSchool == 1, 1, ifelse(inSchool == 2, 0, NA)),
    absentFromSchool = ifelse(absentFromSchool == 1, 1,
                              ifelse(absentFromSchool == 2, 0, NA))
    # scholarship = ifelse(scholarship == 1, 1, ifelse(scholarship == 2, 0, NA)),
    # goNextYr = ifelse(goNextYr == 1, 1, ifelse(goNextYr == 2, 0, NA))
    )


# hh level aggregation
# highest education hh
# convert and merge grades.
# mean education
# % literate hh
# mother literate -- also in sec 1
# literacy by age
# school grade by age
# truancy by district

# 44% illiterate
education2014  %>% filter(over5y == 1)  %>% group_by(literate)  %>% summarise(num = n()) %>% mutate(pct = num/sum(num))
education2014  %>% filter(over5y == 1)  %>% group_by(regionComb, literate)  %>% summarise(num = n()) %>% mutate(pct = round(num/sum(num),2))

setwd(masterDir)
write.csv(education2014, "Dataout/education2014.csv")


# inidividual characteristics ---------------------------------------------

setwd("~/Documents/USAID/Ethiopia/Datain/wave2012/")
one12 = read_dta("sect1_hh_w1.dta"); 
indivChar12 = removeAttributes(one12)
indivChar12 = indivChar12  %>% select(indivID2012  = individual_id, ageY = hh_s1q04_a, ageM = hh_s1q04_b, 
                                      sex = hh_s1q03, relHH = hh_s1q02,
                                      relig = hh_s1q07, married = hh_s1q08, regionBorn = hh_s1q11,
                                      bioDadinHH = hh_s1q12, bioDadEduc = hh_s1q15, bioDadJob = hh_s1q20,
                                      bioMominHH = hh_s1q16, bioMomEduc = hh_s1q19, bioMomJob = hh_s1q21)
indivEd12 = full_join(indivChar12, education2012, by= "indivID2012")
indivEd12 = indivEd12 %>% 
  mutate(year = 2012) %>% 
  select(-eaID2012, -ea)

#----
setwd("~/Documents/USAID/Ethiopia/Datain/wave2014/")
one14 = read_dta("sect1_hh_w2.dta");
# oneQ=pullAttributes(one14)

indivChar14 = removeAttributes(one14)
indivChar14 = indivChar14  %>% select(indivID2014  = individual_id2, indivID2012  = individual_id, ageY = hh_s1q04_a, ageM = hh_s1q04_b, 
                                      sex = hh_s1q03, relHH = hh_s1q02,
                                      relig = hh_s1q07, married = hh_s1q08, regionBorn = hh_s1q11,
                                      bioDadinHH = hh_s1q12, bioDadEduc = hh_s1q15, bioDadJob = hh_s1q20,
                                      bioMominHH = hh_s1q16, bioMomEduc = hh_s1q19, bioMomJob = hh_s1q21)

indivEd14 = full_join(indivChar14, education2014, by= c("indivID2014", "indivID2012"))
indivEd14 = indivEd14 %>% 
  mutate(year = 2014) %>% 
  select(-eaID2012, -eaID2014, -ea,  -hhMemberName)

setwd(masterDir)
write.csv(indivChar14, "Dataout/indiv2014.csv")
write.csv(indivChar12, "Dataout/indiv2012.csv")


# Combine Years -----------------------------------------------------------
ids12 = indivEd12 %>% 
  select(indivID2012, hhID2012)

ids14 = indivEd14 %>% 
  select(indivID2012, indivID2014, hhID2012, hhID2014)

base = full_join(ids12, ids14) # to get all indivIDs and hhIDs


indivEd12 = full_join(base, indivEd12, by = c("indivID2012", "hhID2012"))
indivEd14= full_join(base, indivEd14 %>% select(-hhID2012, -indivID2012), by = c("indivID2014", "hhID2014"))

indivEd = rbind(indivEd12, indivEd14)

setwd(masterDir)
write.csv(indivEd, "Dataout/indiv_education.csv")


# # plotting ----------------------------------------------------------------
# 
# ggplot(indivEd14, aes(x = ageY, y = highestGrade)) + geom_point()
# ggplot(indivEd14, aes(x = ageY, y = literate)) + geom_jitter(alpha = 0.1) + theme_bw()
# 
# litAge14 = indivEd14 %>%
#   filter(!is.na(literate)) %>% 
#   group_by(ageY) %>% 
#   summarise(totNum = n(), litPct = mean(literate))
# 
# 
# 

# 
# litAge12 = indivEd12 %>% 
#   filter(!is.na(literate)) %>% 
#   group_by(ageY) %>% 
#   summarise(totNum = n(), litPct = mean(literate))
# 
# ggplot(indivEd12, aes(x = ageY, y = highestGrade)) + geom_point()
# ggplot(indivEd12, aes(x = ageY, y = literate)) + geom_jitter(alpha = 0.1) + theme_bw()
# 
# ggplot(litAge, aes(x = ageY, y = litPct)) + 
#   geom_point(size = 3) +
#   theme_bw() +
#   coord_cartesian(x = c(4.5, 100)) +
#   ylab("percent literate") +
#   xlab("age") +
#   theme(axis.text = element_text(size = 14), 
#         axis.title = element_text(size = 16, face = "bold"), 
#         title = element_text(size = 18, face = "bold")) +
#   ggtitle("Literacy per age (2014)")
# 
# mergedAge = full_join(litAge12, litAge, by = "ageY") %>% 
#   filter(ageY > 4)
# 
# 
# ggplot(mergedAge, aes(x = ageY, y = litPct.x)) + 
#   geom_area(alpha = 0.5, colour = "blue") +
#   geom_area(aes(x = ageY, y = litPct.y), 
#             alpha = 0.5, colour = "orange") +
#   theme_bw() +
#   coord_cartesian(x = c(4.5, 100)) +
#   ylab("percent literate") +
#   xlab("age") +
#   theme(axis.text = element_text(size = 14), 
#         axis.title = element_text(size = 16, face = "bold"), 
#         title = element_text(size = 18, face = "bold")) +
#   ggtitle("Literacy per age") +
#   scale_color_manual(values = c("2012" = "blue", "2014" = "orange"))
# 
# 
# # posPrice = tim %>% filter(priceShk == 1)
# # negPrice = tim %>% filter(priceShk == 0)
# # 
# # leaflet() %>% addTiles()  %>% 
# #   # addCircles(negPrice$lon_dd_mod, negPrice$lat_dd_mod, radius = 5, fillColor = "blue",
# #                    # fillOpacity = 0.1, weight = 0) %>% setView(38, 8, zoom = 6) %>% 
# #   addCircleMarkers(posPrice$lon_dd_mod, posPrice$lat_dd_mod, radius = 5, fillColor = "red",
# #                    fillOpacity = 0.1, weight = 0) %>% setView(38, 8, zoom = 6)
