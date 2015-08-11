source("~/GitHub/Ethiopia/R/loadETHpanel.r")

x=data %>%
  group_by(ea_id, year, region) %>%
  summarise(num = n(), comm= mean(priceShkComm, na.rm = T), indiv = mean(priceShk))  %>% mutate(diff = comm-indiv)
ggplot(x, aes(x = diff))+geom_histogram()+facet_wrap(~year) + theme_laura()


# Pseudo-map --------------------------------------------------------------

colorNoShk = '#bababa'
colorAgree = brewer.pal(11, 'RdYlBu')[6]
colorIndiv = brewer.pal(11, 'RdYlBu')[11]
colorComm = brewer.pal(11, 'RdYlBu')[1]

data = data %>% 
  mutate(
    priceShk_commHH = 
      ifelse(priceShk == 1 & priceShkComm  == 1, colorAgree,
             ifelse(priceShk == 0 & priceShkComm  == 0, colorNoShk,
                    ifelse(priceShk > priceShkComm, colorIndiv,
                           ifelse(priceShk < priceShkComm, colorComm, NA)))),
    healthShk_commHH = 
      ifelse(healthShk == 1 & healthShkComm  == 1, colorAgree,
             ifelse(healthShk == 0 & healthShkComm  == 0, colorNoShk,
                    ifelse(healthShk > healthShkComm, colorIndiv,
                           ifelse(healthShk < healthShkComm, colorComm, NA)))),
    hazardShk_commHH = 
      ifelse(hazardShk == 1 & hazardShkComm  == 1, colorAgree,
             ifelse(hazardShk == 0 & hazardShkComm  == 0, colorNoShk,
                    ifelse(hazardShk > hazardShkComm, colorIndiv,
                           ifelse(hazardShk < hazardShkComm, colorComm, NA))))
  )

ggplot(data, aes(x = longitude, y = latitude, colour = priceShk_commHH)) + 
  geom_point(alpha = 0.3, size = 7) +
  scale_color_identity() +
  theme_blankLH() +
  ggtitle('PRICE: blue = hh reports shock; red = community does; yellow = both')

ggplot(data, aes(x = longitude, y = latitude, colour = priceShk_commHH)) + 
  geom_jitter(alpha = 0.3, size = 5, position = position_jitter(height = .15, width = 0.15)) +
  scale_color_identity() +
  theme_blankLH() +
  ggtitle('PRICE (jittered): blue = hh reports shock; red = community does; yellow = both reported; grey = no shocks')


ggplot(data, aes(x = longitude, y = latitude, colour = healthShk_commHH)) + 
  geom_jitter(alpha = 0.3, size = 5, position = position_jitter(height = .15, width = 0.15)) +
  scale_color_identity() +
  theme_blankLH() +
  ggtitle('HEALTH (jittered): blue = hh reports shock; red = community does; yellow = both reported; grey = no shocks')


ggplot(data, aes(x = longitude, y = latitude, colour = hazardShk_commHH)) + 
  geom_jitter(alpha = 0.3, size = 5, position = position_jitter(height = .15, width = 0.15)) +
  scale_color_identity() +
  theme_blankLH() +
  ggtitle('HAZARD (jittered): blue = hh reports shock; red = community does; yellow = both reported; grey = no shocks')
