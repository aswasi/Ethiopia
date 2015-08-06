source("R/food security module/setupFncns.r")
setwd("~/GitHub/Ethiopia/")
hhRaw = read_dta("Data/ETH_201507_LSMS_All.dta") 

hh =  removeAttributes(hhRaw) %>% 
  mutate(stuntingHH = stunting, underweightHH = underweight, wastingHH = wasting, BMIhh = BMI, numChildHH = childTag) %>% 
  select(-stunting, -underweight, -wasting, -BMI, -X_merge, -childTag)

hh12 = hh %>% 
  filter(year == 2012, ptrack == 2)


hh14 = hh %>% 
  filter(year == 2014, ptrack == 2)

fillColor = 'purple'

# DD01: Dietary Diversity over regions ------------------------------------
# National dd value.
avgDD12 = mean(hh12$dietDiv, na.rm = TRUE)

regionOrder = hh12 %>% 
  group_by(regionName) %>% 
  summarise(avg = mean(dietDiv), n = n()) %>% 
  arrange(avg)

hh12$regionName = factor(hh12$regionName, regionOrder$regionName[1:11])


ggplot(hh12, aes(x = dietDiv)) +
  geom_density(alpha = 0.4, fill = fillColor) +
  ggtitle("dietary diversity (2012)") +
  ylab("percent of households") +
  xlab("dietary diversity score") +
  theme_leftTitle() +
  theme(          strip.background = element_blank(),
                  strip.text = element_text(colour = colText),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~regionName,  ncol = 2)

ggplot(hh12, aes(x = regionName, y = dietDiv)) +
  geom_violin(alpha = 0.4, fill = fillColor) +
  ggtitle("dietary diversity (2012)") +
  xlab("region") +
  ylab("dietary diversity score") +
  theme_leftTitle() +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = colText),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) 

ggplot(hh12, aes(x = regionName, y = dietDiv)) +
  geom_boxplot(alpha = 0.4, fill = fillColor) +
  ggtitle("dietary diversity (2012)") +
  xlab("region") +
  ylab("dietary diversity score") +
  theme_leftTitle() +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = colText),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) 

ggplot(hh12, aes(x = regionName, y = dietDiv)) +
  geom_jitter(size = 3, colour = fillColor, 
              alpha = 0.07, position = position_jitter(w = 0.4, h = 0.2))+
  geom_violin(alpha = 0.2, fill = fillColor) +
  ggtitle("dietary diversity (2012)") +
  geom_hline(yint = avgDD12, alpha = 0.3) +
  xlab("region") +
  ylab("dietary diversity score") +
  theme_leftTitle() +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = colText),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) 


# DD02: dietary diversity over regions (2014) -----------------------------
avgDD14 = mean(hh14$dietDiv, na.rm = TRUE)

hh14$regionName = factor(hh14$regionName, regionOrder$regionName[1:11])

ggplot(hh14, aes(x = regionName, y = dietDiv)) +
  geom_jitter(size = 3, colour = fillColor, 
              alpha = 0.07, position = position_jitter(w = 0.4, h = 0.2))+
  geom_violin(alpha = 0.2, fill = fillColor) +
  ggtitle("dietary diversity (2014)") +
  geom_hline(yint = avgDD14, alpha = 0.3) +
  xlab("region") +
  ylab("dietary diversity score") +
  theme_leftTitle() +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = colText),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) 



# DD04: heatmap of foods --------------------------------------------------

regions = hhPanel %>% 
  select(household_id, household_id2, dietDiv, year, regionName, fcsMin, religHoh)

ddReg = left_join(regions, dd2014B, by = c("household_id2" = "hhID2014"))

ddReg = ddReg %>% 
  filter(year == 2014)

dd2014_heat = ddReg  %>%  
  group_by(regionName) %>% 
  summarise(cereals = mean(cerealsBin),
            spices = mean(spicesBin), 
            oils = mean(oilsBin),
            pulses = mean(pulsesBin),
            sugar = mean(sugarBin), 
            vegetables = mean(vegBin),
            tubers = mean(tubersBin),
            dairy = mean(milkBin),
            meat = mean(meatBin), 
            fruits  = mean(fruitBin), 
            eggs = mean(eggsBin), 
            fish = mean(fishBin), 
            `dietary diversity` = mean(dietDiv, na.rm = TRUE),
            dd = mean(dd2014B)
  ) %>% 
  arrange(desc(`dietary diversity`))

countryAvg = ddReg  %>%  
  summarise(cereals = mean(cerealsBin), 
            pulses = mean(pulsesBin), 
            tubers = mean(tubersBin), 
            dairy = mean(milkBin), 
            spices = mean(spicesBin), 
            meat = mean(meatBin), 
            sugar = mean(sugarBin), 
            eggs = mean(eggsBin), 
            fish = mean(fishBin), 
            vegetables = mean(vegBin),
            fruits  = mean(fruitBin), 
            oils = mean(oilsBin),
            `dietary diversity` = mean(dietDiv, na.rm = TRUE),
            dd = mean(dd2014B))


# differential DD
rel_DD2014 = dd2014_heat %>% 
  mutate(cereals = cereals - countryAvg$cereals,
         pulses = pulses - countryAvg$pulses,
         tubers = tubers - countryAvg$tubers,
         dairy = dairy - countryAvg$dairy,
         spices = spices - countryAvg$spices,
         meat = meat - countryAvg$meat,
         sugar = sugar - countryAvg$sugar,
         eggs = eggs - countryAvg$eggs,
         fish = fish - countryAvg$fish,
         vegetables = vegetables - countryAvg$vegetables,
         fruits = fruits - countryAvg$fruits,
         oils = oils - countryAvg$oils)

write.csv(dd2014_heat, 'dd2014_heat.csv')
write.csv(rel_DD2014, 'rel_DD2014_heat.csv')

