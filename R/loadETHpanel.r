setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")

data = read_dta("Data/ETH_201508_analysis_panel.dta")

data = removeAttributes(data)