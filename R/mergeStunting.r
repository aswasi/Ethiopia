# Ethiopia LS/MS analysis
#
# Function to import data on child level stunting and merge with the household-level data.
#
# Laura Hughes, lhughes@usaid.gov, July 2015


# Import data -------------------------------------------------------------
setwd("~/GitHub/Ethiopia/")

source("R/food security module/setupFncns.r")

# Load inidividual education data.
setwd("~/GitHub/Ethiopia/")
source("R/educationMod.R")

indivEd = indivEd %>% 
  mutate(household_id = hhID2012, household_id2 = hhID2014, individual_id = indivID2012, individual_id2 = indivID2014) %>% 
  select(-hhID2012, -hhID2014, -hhWeight, -hhIDTrunc, -indivID2012, -indivID2014,
         -rural, -region, -regionComb)

setwd("~/GitHub/Ethiopia/")
hhRaw = read_dta("Data/ETH_201507_LSMS_All.dta") 

hh =  removeAttributes(hhRaw) %>% 
  mutate(stuntingHH = stunting, underweightHH = underweight, wastingHH = wasting, BMIhh = BMI, numChildHH = childTag) %>% 
  select(-stunting, -underweight, -wasting, -BMI, -X_merge, -childTag)

childRaw = read_dta("Data/ETH_201506_cHealth.dta")

childHealth = removeAttributes(childRaw)

childHealth = childHealth %>% 
#   mutate(household_id = as.character(household_id), household_id2 = as.character(household_id2),
#          ea_id = as.character(ea_id), ea_id2 = as.character(ea_id2)) %>% 
  select(-X_merge, -hid)



# Merge data --------------------------------------------------------------

# Merge education / individual data with child health data.
indiv_ed_health = full_join(childHealth, indivEd, 
                            by = c("household_id", "household_id2", "individual_id", "individual_id2", "year"))


# Merge with hh data.
indiv = full_join(hh, indiv_ed_health, by = c("household_id",
                                          "year"))
#                                           , "pw", "saq02", "saq03", "saq04", "saq05", "saq06", "saq07", 
#                                           "saq08", "ptrack", "pw2", "illness", 
#                                           "totIllness", "malariaHH", "diarrheaHH", "respInfection", "chDiarrhea"))

child = indiv %>% 
  filter(childTag == 1)



# Exploratory plots -------------------------------------------------------

colors = brewer.pal(11, "Spectral")

ggplot(child, aes(x = BMI, y = stunting)) +
  theme_laura() + 
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])

ggplot(child, aes(x = underweight, y = stunting)) +
  theme_laura() + 
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])


ggplot(child, aes(x = numChildHH, y = stunting)) +
  theme_laura() + 
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])

ggplot(child, aes(x = fcsMin, y = stunting)) +
  theme_laura() + 
  facet_wrap(~year) +
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])

ggplot(child, aes(x = noToilet, y = stunting)) +
  theme_laura() + 
  facet_wrap(~year) +
  geom_jitter(size = 4, alpha = 0.3, color = colors[sample(11,1)])


ggplot(childHealth, aes(x = ageMonths, y = stunting)) +
  theme_laura() + 
  facet_wrap(~year) +
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])

ggplot(childHealth, aes(x = ageMonths, y = stunting)) +
  theme_laura() + 
  facet_wrap(~ year + gender) +
  geom_smooth()

ggplot(childHealth, aes(x = ageMonths, y = stunting)) +
  theme_laura() + 
  facet_wrap(~ year + gender) +
  geom_smooth()

ggplot(childHealth, aes(x = ageMonths, y = stunted, fill = factor(year))) +
  theme_laura() +
  facet_wrap(~ region) +
  geom_smooth()

ggplot(childHealth, aes(x = ageMonths, y = stunted, fill = factor(gender))) +
  theme_laura() +
  facet_wrap(~ region) +
  geom_smooth()
  
ggplot(childHealth, aes(x = ageMonths, y = stunting, color = year)) +
  theme_laura() 
  


