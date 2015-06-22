/*-------------------------------------------------------------------------------
# Name:		XX_dietDiv
# Purpose:	Process household food consumption information
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/17
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close 
log using "$pathlog/XX_dietDiv.txt", replace

* Load module on food aggregates for dietary diversity score
use "$wave1/sect5b_hh_w1.dta", clear

/* Create dietary diversity variable consisting of following food groups:
  1) Cereals - cereal_days & pulses (x)
  2) White roots and tubers - Starches (x)
  3) Vegetables (x)
  4) Fruits (x)
  5) Meat (x)
  6) Eggs (x) 
  7) Fish and other seafood (x)
  8) Legumes, nuts and seeds (x)
  9) Milk and milk products (x)
  10) Oils an fats 
  11) Sweets*
  12) Spices condiments and beverages	*/

g byte cereal = inlist(hh_s5bq00, 1, 2, 4) == 1 & hh_s5bq01 == 1
g byte starch = inlist(hh_s5bq00, 3, 16) == 1 & hh_s5bq01 == 1
g byte veg =  inlist(hh_s5bq00, 7) == 1 & hh_s5bq01 == 1
g byte fruit =  inlist(hh_s5bq00, 8) == 1 & hh_s5bq01 == 1
g byte meat =  inlist(hh_s5bq00, 9, 10) == 1 & hh_s5bq01 == 1
g byte eggs =  inlist(hh_s5bq00, 11) == 1 & hh_s5bq01 == 1
g byte fish =  inlist(hh_s5bq00, 12) == 1 & hh_s5bq01 == 1
g byte legumes =  inlist(hh_s5bq00, 6) == 1 & hh_s5bq01 == 1
g byte milk =  inlist(hh_s5bq00, 14) == 1 & hh_s5bq01 == 1
g byte fats =  inlist(hh_s5bq00, 13) == 1 & hh_s5bq01 == 1
g byte sweet =  inlist(hh_s5bq00, 5) == 1 & hh_s5bq01 == 1
g byte cond =  inlist(hh_s5bq00, 15) == 1 & hh_s5bq01 == 1  

local dietLab cereal starch veg fruit meat eggs fish legumes milk fats sweet cond 
foreach x of local dietLab {
	la var `x' "Consumed `x' in last 7 days"
} 

* Check  households not reporting any consumption
egen tmp = rsum(cereal starch veg fruit meat eggs fish legumes milk fats sweet cond)
egen tmpsum = total(tmp), by(household_id)
* for checking which HH are missing all consumption information
br if tmpsum == 0

* Keep derived data (FCS & dietary diversity scores) and HHID
ds(hh_s* saq* ea_id), not
keep `r(varlist)'

* Collapse down to household level using max option, retain labels
qui include "$pathdo/copylabels.do"
ds(household_id), not
collapse (max) `r(varlist)', by(household_id)
qui include "$pathdo/attachlabels.do"

egen dietDiv = rsum(cereal starch veg fruit meat eggs fish legumes milk fats sweet cond)
la var dietDiv "Dietary diversity (12 food groups)"
recode dietDiv (0 = .) 
g year = 2012
sa "$pathout/dietdiv_2012.dta", replace

*** Load 2014 data and repeat process ***
clear
use "$wave2/sect5b_hh_w2.dta", clear

g byte cereal = inlist(hh_s5bq00, 1, 2, 4) == 1 & hh_s5bq01 == 1
g byte starch = inlist(hh_s5bq00, 3, 16) == 1 & hh_s5bq01 == 1
g byte veg =  inlist(hh_s5bq00, 7) == 1 & hh_s5bq01 == 1
g byte fruit =  inlist(hh_s5bq00, 8) == 1 & hh_s5bq01 == 1
g byte meat =  inlist(hh_s5bq00, 9, 10) == 1 & hh_s5bq01 == 1
g byte eggs =  inlist(hh_s5bq00, 11) == 1 & hh_s5bq01 == 1
g byte fish =  inlist(hh_s5bq00, 12) == 1 & hh_s5bq01 == 1
g byte legumes =  inlist(hh_s5bq00, 6) == 1 & hh_s5bq01 == 1
g byte milk =  inlist(hh_s5bq00, 14) == 1 & hh_s5bq01 == 1
g byte fats =  inlist(hh_s5bq00, 13) == 1 & hh_s5bq01 == 1
g byte sweet =  inlist(hh_s5bq00, 5) == 1 & hh_s5bq01 == 1
g byte cond =  inlist(hh_s5bq00, 15) == 1 & hh_s5bq01 == 1  

local dietLab cereal starch veg fruit meat eggs fish legumes milk fats sweet cond 
foreach x of local dietLab {
	la var `x' "Consumed `x' in last 7 days"
} 

* Check  households not reporting any consumption
egen tmp = rsum(cereal starch veg fruit meat eggs fish legumes milk fats sweet cond)
egen tmpsum = total(tmp), by(household_id2)
* for checking which HH are missing all consumption information
* br if tmpsum == 0

* Keep derived data (FCS & dietary diversity scores) and HHID
ds(hh_s* saq* ea_id household_id ea_id2), not
keep `r(varlist)'

qui include "$pathdo/copylabels.do"
ds(household_id2), not
collapse (max) `r(varlist)', by(household_id2)
qui include "$pathdo/attachlabels.do"

egen dietDiv = rsum(cereal starch veg fruit meat eggs fish legumes milk fats sweet cond)
la var dietDiv "Dietary diversity (12 food groups)"
recode dietDiv (0 = .) 
g year = 2014
