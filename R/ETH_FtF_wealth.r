# Exploration: why is wealth so different in FtF zones at baseline in Ethiopia?
# Laura Hughes, lhughes@usaid.gov, September 2015.

# Load data ---------------------------------------------------------------
setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")


# price = read.csv('Analysis/priceShkWide.txt', skip = 1 , sep = '\t')

source("~/GitHub/Ethiopia/R/loadETHpanel.r")

vars2test =c('ax', 'bed', 'bike','blanket', 'car', 'cart', 'clothing', 'dungFuel', 'dvd',
                'elecLight', 'fireLight', 'flushToilet', 'indoorKitchen', 'jewel', 'metalRoof',
'mitad', 'mobile', 'moto', 'mudFloor', 'mudHome', 'noKitchen', 'hasToilet',
'ownHouse', 'phone', 'plough', 'protWaterDry', 'protWaterRainy', 'pump', 'radio', 
'refrig', 'sat', 'sew', 'shelf', 'sickle', 'sofa', 'stoneHome', 'stove', 'thatchRoof',
'tv', 'watch', 'weave', 'well', 'wasteFert', 'wasteThrow', 'roomsPC')

tests = NA

for (varName in vars2test){
  
  print(varName)
  
  ctrl = data %>% 
    filter(year == 2012, ftfzone_5km == 0) %>% 
    select_(varName)
  
  ctrl = ctrl[,1]
  
  ftf = data %>% 
    filter(year == 2012, ftfzone_5km == 1) %>% 
    select_(varName)
  
  ftf = ftf[,1]
  
  tests= rbind(tests,broom::tidy(t.test(ctrl, ftf)))
}


tests = tests %>% 
  mutate(signif = ifelse(p.value < 0.05, TRUE, FALSE),
         variable = c('NA', vars2test),
  sign = ifelse(estimate2 < estimate1, 'lower', 'higher'),
  diff = estimate2 - estimate1,
  pct = (estimate2-estimate1)/estimate1)

x =tests  %>% filter(signif == TRUE) %>% 
  select(variable, sign, estimate1, estimate2, diff, pct, p.value)
