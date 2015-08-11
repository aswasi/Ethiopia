setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")

dataRaw = read_dta("Data/ETH_201508_analysis_panel.dta")

data = removeAttributes(dataRaw)

childRaw = read_dta("Data/ETH_201508_Child_Analysis.dta")

child = removeAttributes(childRaw)