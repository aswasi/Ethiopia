# Plots for overview about how LAMs work.

# Import cleaned data (to harvest ptrack, etc.) ------------------------------
source("~/GitHub/Ethiopia/R/setupFncns.r") 
source("~/GitHub/Ethiopia/R/loadETHpanel.r")

ggplot(data %>% filter(year == 2014), aes(x = wealthSmooth2014, y = priceShk)) + 
  geom_smooth(alpha = 0.15, colour = '#bd0026') + 
  theme_yGrid() + 
  coord_cartesian(ylim = c(0, 0.22)) +
  scale_y_continuous(
    labels = percent) +
  scale_x_continuous(limits = c(1, 10),
                     breaks = 1:10,
                     labels = c('low', '','', '', 'medium',
                                '', '', '', 'high',''))+
  ylab('wealth') +
  ggtitle('Percent of households reporting price shocks is higher for the poor.')


ggsave("~/Documents/USAID/LAM talks/wealthPrice.pdf",
       width = 4, height = 2,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

x = data %>% 
  filter(year == 2014) %>% 
  slice(sample(1:7552/2, 20)) %>% 
  mutate(wealthIndex2014 = round(wealthIndex2014,2),
         id = lapply(1:20, function(x) paste0('hh', x))) %>% 
  select(id, bed, tv, bike, 
         `mud home` = mudHome, electricity = elecLight,
         `wealth index` = wealthIndex2014, `wealth decile` = wealthSmooth2014)

library(readr)
write_csv('~/GitHub/Ethiopia/R/plots/wealthTable.csv', x)
write_csv(x, 'wealthTable.csv')
dput(x, 'wealthTable.csv')
