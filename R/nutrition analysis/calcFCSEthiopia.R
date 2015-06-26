calcFCSEthiopia <- function(code, w1, w2){
  
  #______________________________________________________________________________
  # Code to import World Bank LS/MS data and calculate dietary diversity and
  # food consumption scores using multiple methods.
  # 
  # For documentation on the calculation methods, see the associated Rmarkdown
  # file: EthiopiaFCS_Laura.Rmd
  # 
  # INPUTS: - code: string containing the working directory where the code is.
  #         - w1: string specifying working directory for wave1 data (2011/2012)
  #         - w2: string specifying working directory for wave2 data (2013/2014)
  #
  # 
  # OUTPUTS: - foodSecCalcs:  a data frame containing all the calculations, at
  #                           the household level: 
  #                         - hhID2012: household ID from 2012 surveys
  #                         - hhID2014: household ID from 2014 surveys
  #                         - regionComb: string containing the regions in the 
  #                                       survey, grouped by the 7 largest.
  #                         - dd2012A:  dietary diversity from 2012, calculated
  #                                     using survey module 5A (24 h recall) 
  #                         - dd2014A:  dietary diversity from 2014, calculated
  #                                     using survey module 5A (24 h recall) 
  #                         - dd2012B:  dietary diversity from 2012, calculated
  #                                     using survey module 5B (7 day recall) 
  #                         - dd2014B:  dietary diversity from 2014, calculated
  #                                     using survey module 5B (7 day recall)
  #                         - fcs2012Min:  food consumption score from 2012, 
  #                                        calculated using survey module 5B 
  #                                        using the MOST conservative estimate 
  #                                        of consumption.
  #                         - fcs2012Max:  food consumption score from 2012, 
  #                                        calculated using survey module 5B 
  #                                        using the LEAST conservative estimate 
  #                                        of consumption.
  #                         - fcs2012MinCat: factor characterization of FCS score.
  #                         - fcs2012MaxCat: factor characterization of FCS score.
  #                         - fcs2014Min:  food consumption score from 2014, 
  #                                        calculated using survey module 5B 
  #                                        using the MOST conservative estimate 
  #                                        of consumption.
  #                         - fcs2014Max:  food consumption score from 2014, 
  #                                        calculated using survey module 5B 
  #                                        using the LEAST conservative estimate 
  #                                        of consumption.
  #                         - fcs2014MinCat: factor characterization of FCS score.
  #                         - fcs2014MaxCat: factor characterization of FCS score.
  #
  # NOTE: other variables and useful things are calculated in this module-- like the
  # comparison across regions in Ethiopia, and the contribution of each food group to
  # the calculated value. Documentation provided in indiv. R files.
  #
  # Laura Hughes, USAID, lhughes@usaid.gov
  # June 2015
  #______________________________________________________________________________
  
  # Load the functions / files for the files to calculate everything.
  # for me: 
#   code = "~/Documents/USAID/Ethiopia/R/"
#   w1 = "~/Documents/USAID/Ethiopia/Datain/wave2012"
#   w2 = "~/Documents/USAID/Ethiopia/Datain/wave2014"
  
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  
  # Helper functions for importing.
  source(paste0(code, "removeAttributes.r"))
  source(paste0(code, "pullAttributes.r"))

  
  #______________________________________________________________________________
  # Calculate 2012 stats.
  #______________________________________________________________________________
  setwd(w1)
  
  # Dietary diversity from 2012, Module A.
  source(paste0(code, "EthiopiaDietaryDiversity_2012.r"))
  
  # Dietary diversity from 2012, Module B.
  source(paste0(code, "EthiopiaDD_ModB_2012.r"))  
  
  # Food security from 2012.
  source(paste0(code, "EthiopiaFCS2012.r"))
  
  #______________________________________________________________________________
  # Calculate 2014 stats.
  #______________________________________________________________________________
  setwd(w2)
  
  # Dietary diversity from 2014, Module A.
  source(paste0(code, "EthiopiaDietaryDiversity_2014.r"))
  
  # Dietary diversity from 2014, Module B.
  source(paste0(code, "EthiopiaDD_ModB_2014.r"))  
  
  # Food security from 2014.
  source(paste0(code, "EthiopiaFCS2014.r"))
  
  
  #______________________________________________________________________________
  # Merge everything together.
  #______________________________________________________________________________
  comb1 = full_join(dd2012A, dd2012B, by = "hhID2012")
  comb2 = full_join(dd2014A, dd2014B, by = "hhID2014")
  comb3 = full_join(comb1, hhAggr2012, by = "hhID2012")
  comb4 = full_join(comb2, hhAggr2014, by = "hhID2014")
  
  foodSecCalcs = full_join(comb3, comb4, by = c("hhID2012", "regionComb")) %>% 
    select(hhID2012, hhID2014, regionComb = regionComb, dd2012A, dd2012B, 
           dd2014A, dd2014B, fcsMin2012, fcsMax2012, fcsCatMin2012, 
           fcsCatMax2014, fcsMin2014, fcsMax2014, fcsCatMin2014, 
           fcsCatMax2014)
  
  
  return(foodSecCalcs)
}