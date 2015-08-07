# Plots on stunting in Ethiopia, from LS/MS analysis.


# Colors ------------------------------------------------------------------
male="#6495ED"
female="#F08080"

# Setup functions ---------------------------------------------------------


setwd("~/GitHub/Ethiopia/")

source("R/setupFncns.r")


# Load data ---------------------------------------------------------------
setwd("~/GitHub/Ethiopia/")

childRaw = read_dta("Data/ETH_201506_cHealth.dta")


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

childHealthPanel = childHealth %>% 
  filter(!is.na(year), !is.na(stunted), 
         ptrack == 2)

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




