source("~/GitHub/Ethiopia/R/loadETHpanel.r")

library(forcats)
library(llamar)

ddReg = data %>% 
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
            dietDiv = mean(dietDiv, na.rm = TRUE)
  ) %>% 
  arrange(desc(dietDiv))

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
            dietDiv = mean(dietDiv, na.rm = TRUE))


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

# write.csv(dd2014_heat, 'dd2014_heat.csv')
# write.csv(rel_DD2014, 'rel_DD2014_heat.csv')

rel_DD2014 = rel_DD2014 %>% 
  gather(food, rel_mean, -regionName, -dietDiv)

rel_DD2014$regionName = 
  factor(rel_DD2014$regionName,
         rev(dd2014_heat$regionName))



# rename for demo ---------------------------------------------------------
all_ethiopia = gather(countryAvg, food, avg, -dietDiv) %>% 
  mutate(regionName = 'Ethiopia')

all_ethiopia$food = factor(all_ethiopia$food)
all_ethiopia$food = fct_reorder(all_ethiopia$food, all_ethiopia$avg, .desc = TRUE)

dd2014_heat = dd2014_heat %>% 
  gather(food, avg, -regionName, -dietDiv)

eth_adm1 = full_join(rel_DD2014, dd2014_heat, by = c("regionName", "dietDiv", "food"))

eth_adm1$food = factor(eth_adm1$food , levels = levels(all_ethiopia$food))
