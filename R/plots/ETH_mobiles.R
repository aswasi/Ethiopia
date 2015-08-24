
# Load data ---------------------------------------------------------------
setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")


# price = read.csv('Analysis/priceShkWide.txt', skip = 1 , sep = '\t')

source("~/GitHub/Ethiopia/R/loadETHpanel.r")

data = data %>% 
  filter(ptrack == 2)


data14 = data %>% 
  filter(year == 2014) %>% 
  mutate(eduFcat = ifelse(educAdultF == 0, 'no education',
                          ifelse((educAdultF == 1 | educAdultF == 2 | educAdultF == 3), 'primary',
                                 ifelse((educAdultF == 4 | educAdultF == 5), 'secondary',
                                        ifelse(educAdultF == 6, 'tertiary', NA)))),
         eduMcat = ifelse(educAdultM == 0, 'no education',
                          ifelse((educAdultM == 1 | educAdultM == 2 | educAdultM == 3), 'primary',
                                 ifelse((educAdultM == 4 | educAdultM == 5), 'secondary',
                                        ifelse(educAdultM == 6, 'tertiary', NA)))))



# panelize ----------------------------------------------------------------
mobileData = data %>% 
  select(household_id, household_id2, year, region, saq01, 
         wlthSmooth, wealthIndex2012, wealthIndex2014, wealthIndex,
         watch, phone, mobile, tv, dvd, sat,
         ftfzone, religHoh, femhead, literateHoh, agehead) %>% 
  gather(item, owns, -household_id, -household_id2, -year, -region, -saq01,
         -ftfzone, -religHoh, -femhead, -literateHoh, -agehead, 
         -wlthSmooth, -wealthIndex2012, -wealthIndex2014, -wealthIndex)


# wealth v. region --------------------------------------------------------
ggplot(mobileData, aes(x = wlthSmooth, y = owns, colour = item)) +
  geom_smooth(method = "loess", alpha = 0.00, size = 1.15, span = 1.5)  +
  facet_wrap(~ftfzone) +   
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), labels=c("", "very poor","poor", "above average")) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  theme_jointplot()


# Not much difference for ftfzones.  

ggplot(mobileData %>% filter(item == 'mobile'), aes(x = wlthSmooth, y = owns, colour = factor(religHoh))) +
  geom_smooth(method = "loess", alpha = 0.00, size = 1.15, span = 1.5)  +
  # facet_wrap(~ftfzone) +   
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), labels=c("", "very poor","poor", "above average")) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  theme_jointplot()
