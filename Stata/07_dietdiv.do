/*-------------------------------------------------------------------------------
# Name:		07_dietDiv
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
log using "$pathlog/07_dietDiv.txt", replace
di in yellow "`c(current_date)' `c(current_time)'"

* Load module on food aggregates for dietary diversity score
use "$wave1/sect5b_hh_w1.dta", clear

/* Create dietary diversity variable consisting of following food groups:
  1) Cereals - cereal_days & pulses (x)
  2) White roots and tubers - Starches (x)
  3) Vegetables (x)
  4) Fruits (x)
  5) Meat (x)
  6) Eggs (x) 0
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
g byte meat2 = inlist(hh_s5bq00, 9, 10, 11, 12) == 1 & hh_s5bq01 == 1  
g byte oil = inlist(hh_s5bq00, 15, 13) == 1 & hh_s5bq01 == 1  
g byte staples = inlist(hh_s5bq00, 1, 2, 3, 4, 16) == 1 & hh_s5bq01 == 1

local dietLab cereal starch veg fruit meat eggs fish legumes milk fats sweet cond meat2 oil staples
foreach x of local dietLab {
	la var `x' "Consumed `x' in last 7 days"
	g `x'_days = hh_s5bq02 if `x' == 1
	replace `x'_days = 0 if `x'_days == .
} 

* Check  households not reporting any consumption
egen tmp = rsum(cereal starch veg fruit meat eggs fish legumes milk fats sweet cond)
egen tmpsum = total(tmp), by(household_id)
* for checking which HH are missing all consumption information
*br if tmpsum == 0

* Calculate food consumption score* Create variables to calculate Food Consumption Score 
g cerealFCS = cereal_days * 2
g starchFCS = starch_days * 2
g staplesFCS = staples_days * 2

g legumesFCS = legumes_days * 3

* Both weighted by 1
g vegFCS = veg_days
g fruitFCS = fruit_days

* meat, poultry, fish, eggs
g meatFCS = meat2_days * 4
g milkFCS = milk_days * 4

g sweetFCS = sweet_days * 0.5
g oilFCS = oil_days * 0.5

* Keep derived data (FCS & dietary diversity scores) and HHID
ds(hh_s* saq* ea_id), not
keep `r(varlist)'

* Collapse down to household level using max option, retain labels
qui include "$pathdo/copylabels.do"
ds(household_id), not
collapse (max) `r(varlist)', by(household_id)
qui include "$pathdo/attachlabels.do"

* Calculate two metrics
egen dietDiv = rsum(cereal starch veg fruit meat eggs fish legumes milk fats sweet cond)
la var dietDiv "Dietary diversity (12 food groups)"
recode dietDiv (0 = .) 
g year = 2012

egen FCS = rsum2(staplesFCS legumesFCS vegFCS fruitFCS meatFCS milkFCS sweetFCS oilFCS)
recode FCS (0 = .)

clonevar FCS_categ = FCS 
recode FCS_categ (0/21 = 0) (21.5/35 = 1) (35.1/53 = 2) (53/112 = 3)
lab def fcscat 0 "Poor" 1 " Borderline" 2 " Acceptable low" 3 "Acceptable high"
lab val FCS_categ fcscat
la var FCS_categ "Food consumption score category"
tab FCS_cat, mi


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
g byte meat2 = inlist(hh_s5bq00, 9, 10, 11, 12) == 1 & hh_s5bq01 == 1  
g byte oil = inlist(hh_s5bq00, 15, 13) == 1 & hh_s5bq01 == 1  
g byte staples = inlist(hh_s5bq00, 1, 2, 3, 4, 16) == 1 & hh_s5bq01 == 1 

local dietLab cereal starch veg fruit meat eggs fish legumes milk fats sweet cond meat2 oil staples
foreach x of local dietLab {
	la var `x' "Consumed `x' in last 7 days"
	g `x'_days = hh_s5bq02 if `x' == 1
	replace `x'_days = 0 if `x'_days == .
} 

* Check  households not reporting any consumption
egen tmp = rsum(cereal starch veg fruit meat eggs fish legumes milk fats sweet cond)
egen tmpsum = total(tmp), by(household_id2)
* for checking which HH are missing all consumption information
* br if tmpsum == 0

* Calculate food consumption score* Create variables to calculate Food Consumption Score 
g cerealFCS = cereal_days * 2
g starchFCS = starch_days * 2
g staplesFCS = staples_days * 2
g legumesFCS = legumes_days * 3

* Both weighted by 1
g vegFCS = veg_days
g fruitFCS = fruit_days

* meat, poultry, fish, eggs
g meatFCS = meat2_days * 4
g milkFCS = milk_days * 4
g sweetFCS = sweet_days * 0.5
g oilFCS = oil_days * 0.5


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

egen FCS = rsum2(staplesFCS legumesFCS vegFCS fruitFCS meatFCS milkFCS sweetFCS oilFCS)
recode FCS (0 = .)

clonevar FCS_categ = FCS 
recode FCS_categ (0/21 = 0) (21.5/35 = 1) (35.1/53 = 2) (53/112 = 3)
lab def fcscat 0 "Poor" 1 " Borderline" 2 " Acceptable low" 3 "Acceptable high"
lab val FCS_categ fcscat
la var FCS_categ "Food consumption score category"
tab FCS_cat, mi
la var FCS "Food Consumption Score"

clonevar hid = household_id2 

sa "$pathout/dietdiv_2014.dta", replace
pappend dietdiv_2012 dietdiv_2014 dietdiv_all

/* Merge into base, use the update option to not overwrite the data
clear
use "$pathout/hh_base.dta", clear
merge 1:1 household_id year using "$pathout/dietdiv_2012.dta", gen(_2012) update replace
merge 1:1 household_id2 year using "$pathout/dietdiv_2014.dta", gen(_2014) update 

drop _2012 _2014

sa "$pathout/dietdiv_all.dta", replace
*/
