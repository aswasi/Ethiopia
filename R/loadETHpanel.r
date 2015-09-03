setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")

setwd("~/Documents/USAID/Ethiopia/Dataout/")

dataRaw = read_dta("ETH_201508_analysis_panel.dta")

data = removeAttributes(dataRaw)

childRaw = read_dta("ETH_201508_Child_Analysis.dta")

child = removeAttributes(childRaw)

warning('filtering out non-panel households and children.')

data = data %>% 
  filter(ptrack == 2) %>% 
  mutate(religion = ifelse(religHoh == 1, 
                           "Orthodox",
                           ifelse(religHoh == 3,
                                  "Protestant",
                                  ifelse(religHoh == 4,
                                         "Muslim",
                                         ifelse(religHoh == 7 | religHoh == 2 | religHoh == 6 | religHoh == 5,
                                                "other", NA)))),
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
                                                                   )))))))))),
         eduFcat = ifelse(educAdultF == 0, 'no education',
                          ifelse((educAdultF == 1 | educAdultF == 2 | educAdultF == 3), 'primary',
                                 ifelse((educAdultF == 4 | educAdultF == 5), 'secondary',
                                        ifelse(educAdultF == 6, 'tertiary', NA)))),
         eduMcat = ifelse(educAdultM == 0, 'no education',
                          ifelse((educAdultM == 1 | educAdultM == 2 | educAdultM == 3), 'primary',
                                 ifelse((educAdultM == 4 | educAdultM == 5), 'secondary',
                                        ifelse(educAdultM == 6, 'tertiary', NA)))),
         ageLinear = cut(agehead, c(8, 25, 35, 45, 55, 100))
         # dist_FTFzone_smooth = ntile(dist_FTFzone, 6)
         ) 
# %>% 
#   group_by(household_id)  %>% 
#   mutate(TLU_lagged = lag(TLUtotal, order_by = year))


child = child %>% 
  filter(ptrack == 2) %>% 
  mutate(religion = ifelse(religHoh == 1, 
                           "Orthodox",
                           ifelse(religHoh == 3,
                                  "Protestant",
                                  ifelse(religHoh == 4,
                                         "Muslim",
                                         ifelse(religHoh == 7 | religHoh == 2 | religHoh == 6 | religHoh == 5,
                                                "other", NA)))),
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
                                                                   )))))))))),
         eduFcat = ifelse(educAdultF == 0, 'no education',
                          ifelse((educAdultF == 1 | educAdultF == 2 | educAdultF == 3), 'primary',
                                 ifelse((educAdultF == 4 | educAdultF == 5), 'secondary',
                                        ifelse(educAdultF == 6, 'tertiary', NA)))),
         eduMcat = ifelse(educAdultM == 0, 'no education',
                          ifelse((educAdultM == 1 | educAdultM == 2 | educAdultM == 3), 'primary',
                                 ifelse((educAdultM == 4 | educAdultM == 5), 'secondary',
                                        ifelse(educAdultM == 6, 'tertiary', NA))))
  )
