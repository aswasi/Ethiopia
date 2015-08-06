# Plots on stunting in Ethiopia, from LS/MS analysis.


# Setup functions ---------------------------------------------------------


setwd("~/GitHub/Ethiopia/")

# source("R/mergeStunting.r")


# Load data ---------------------------------------------------------------

childRaw = read_dta("Data/ETH_201506_cHealth.dta")

childHealth = removeAttributes(childRaw)

childHealth = childHealth %>% 
  select(-X_merge, -hid) %>% 
  mutate(regionName = ifelse(
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
         sex = ifelse(
           gender == 1, 'male', ifelse(
             gender == 2, 'female', NA)
           )
         )

childHealthPanel = childHealth %>% 
  filter(ptrack == 2)

# Stunt01 - male / female over time ---------------------------------------

ggplot(childHealthPanel, aes(x = ageMonths, y = stunted, 
                             color = factor(year),
                             fill = factor(year))) +
  theme_laura() +
  facet_wrap(~ sex) +
  geom_smooth() +
  coord_cartesian(ylim = c(0,1)) +
  geom_rug()



childHealth = childHealth %>% 
  filter(!is.na(year), !is.na(stunted), region != "")

ggplot(childHealth, aes(x = ageMonths, y = stunted, 
                             color = factor(year),
                             fill = factor(year))) +
  theme_laura() +
  facet_wrap(~ gender) +
  geom_smooth() +
  coord_cartesian(ylim = c(0,1)) +
  geom_rug()

ggplot(childHealth %>% filter(year == 2012, !is.na(stunted)), 
       aes(x = ageMonths, y = stunted)) +
  theme_laura() +
  facet_wrap(~ gender) +
  geom_smooth() +
  coord_cartesian(ylim = c(0,1)) +
  geom_rug()


# Stunt02 - male / female over time ---------------------------------------

ggplot(childHealth, aes(x = ageMonths, y = stunted, 
                             color = factor(sex),
                             fill = factor(sex))) +
  theme_laura() +
  facet_wrap(~ regionName) +
  geom_smooth(method = 'loess', span = 1) +
  coord_cartesian(ylim = c(0,1)) +
  geom_rug()

ggplot(childHealth, aes(x = ageMonths, y = stunted, 
                        color = factor(year)
                        )) +
  theme_laura() +
  facet_wrap(~ gender) +
  geom_smooth(method = 'loess', span = 1, alpha = 0.3) +
  coord_cartesian(ylim = c(0,1)) +
  geom_rug()


ggplot(indiv, aes(x = ageMonths, y = stunted, 
                        color = factor(year),
                        fill = factor(year))) +
  theme_laura() +
  facet_wrap(~ ftfzone) +
  geom_smooth(method = 'loess', span = 1) +
  coord_cartesian(ylim = c(0,1)) +
  geom_rug()


