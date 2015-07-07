/*-------------------------------------------------------------------------------
# Name:		09_housing
# Purpose:	Process housing assets and create basic infrastructure index
# Author:	Tim Essam, Ph.D.
# Created:	2015/07/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

capture log close
clear
log using "$pathlog/09_housing.txt", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off

* Household quality information
u "$wave1\sect9_hh_w1.dta"

clonevar ageHouse = hh_s9q02_a
g byte ownHouse = inlist(hh_s9q03, 1) == 1
g byte rentHouse = inlist(hh_s9q03, 3) == 1

* Dwelling materials and type
clonevar houseSize = hh_s9q04
recode houseSize (0 = 1)
g byte mudHome = inlist(hh_s9q05, 1) == 1
g byte stoneHome = inlist(hh_s9q05, 4, 5, 6, 7, 8, 9) == 1
g byte metalRoof = inlist(hh_s9q06, 1) == 1
g byte thatchRoof = inlist(hh_s9q06, 3, 4, 5) == 1
g byte mudFloor = inlist(hh_s9q07, 1) == 1

* Kitchen - for WASH analysis
g byte noKitchen = inlist(hh_s9q08, 1) == 1
g byte indoorKitchen = inlist(hh_s9q08, 2, 4) == 1
g byte dungFuel = inlist(hh_s9q21, 4, 5) == 1

* Sanitation - for WASH analysis
g byte flushToilet = inlist(hh_s9q10, 1, 2) == 1
g byte noToilet = inlist(hh_s9q10, 7, 8) == 1
g byte sanitWaste = inlist(hh_s9q12, 1, 2, 7) ==1 
g byte wasteFert = inlist(hh_s9q12, 5) == 1
g byte wasteThrow = inlist(hh_s9q12, 4, 3) == 1
g byte protWaterRainy = inlist(hh_s9q13, 1, 2, 3, 4, 5, 6, 7) == 1
g byte protWaterDry = inlist(hh_s9q14, 1, 2, 3, 4, 5, 6, 7) == 1
g byte protWaterAll = (protWaterRainy == 1 & protWaterDry == 1)
g byte treatWater = inlist(hh_s9q15, 1) == 1 | inlist(hh_s9q16, 1) == 1

* Lighting
g byte elecLight = inlist(hh_s9q19, 1, 2, 3) == 1
g byte fireLight = inlist(hh_s9q19, 9, 10, 11, 12) == 1 
g byte noElect = inlist(hh_s9q20, 1) == 1
g byte electricity = noElect != 1

la var ownHouse "HH owns house"
la var rentHouse "HH rent's house"
la var mudHome"Dwelling made primarily of mud"
la var stoneHome "Dwelling made primarily of stone"
la var metalRoof "Roof made primarily of metal"
la var thatchRoof "Roof made primarily of thatch"
la var mudFloor "Mud floor"
la var noKitchen "No dedicated kitchen space"
la var indoorKitchen "Household has a separate room inside for kitchen"
la var dungFuel "Household uses dung or crop residue for cooking"
la var flushToilet "Household has a flush toilet"
la var noToilet "Household does not have designated toilet area"
la var sanitWaste "Household has trash services available"
la var wasteFert "Uses waste for fertilizer"
la var wasteThrow "Throws away waste"
la var protWaterRainy "Protected water source in rainy season"
la var protWaterDry "Protected water source in dry season"
la var treatWater "Household boils or purify's water"
la var elecLight "Household has electricity connectionf or lighting"
la var fireLight "Household relies on fire/gas source for light"
la var electricity "Household has electricty connection"
la var noElect "household does not have electricity"
la var protWaterAll "Water source protected year round"

* Collapse, relabel and save to pathout for mergine w/ 2014 data
ds(hh_s* saq* ea_id), not
keep `r(varlist)'

include "$pathdo/copylabels.do"
ds(household_id), not
#delimit ;
	collapse (max) `r(varlist)',
	by(household_id) fast; 
#delimit cr
* Reapply variable lables & value labels
include "$pathdo/attachlabels.do"

g year = 2012

* Merge in geovariables and sampling weights
merge 1:1 household_id using "$pathraw/Pub_ETH_HouseholdGeovariables_Y1.dta", gen(geo_merge)

* Create household infrastructure index
capture macro drop infraVars
global infraVars "houseSize mudFloor noToilet mudHome metalRoof electricity protWaterAll indoorKitchen"

factor  $infraVars if rural == 1, pcf
qui predict infraindex_rur if rural == 1
alpha $infraVars if rural == 1
scree
loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

factor  $infraVars if rural == 0, pcf
qui predict infraindex_urb if rural == 0
alpha $infraVars if rural == 0
scree
loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

sa "$pathout/housing_2012.dta", replace

* -----------------------------------------------------------------------------------*

****************************
* --- Process 2014 data ---*
****************************

* Household quality information
u "$wave2\sect9_hh_w2.dta", clear

clonevar ageHouse = hh_s9q02_a
g byte ownHouse = inlist(hh_s9q03, 1) == 1
g byte rentHouse = inlist(hh_s9q03, 3) == 1

* Dwelling materials and type
clonevar houseSize = hh_s9q04
recode houseSize (0 = 1)
g byte mudHome = inlist(hh_s9q05, 1) == 1
g byte stoneHome = inlist(hh_s9q05, 4, 5, 6, 7, 8, 9) == 1
g byte metalRoof = inlist(hh_s9q06, 1) == 1
g byte thatchRoof = inlist(hh_s9q06, 3, 4, 5) == 1
g byte mudFloor = inlist(hh_s9q07, 1) == 1

* Kitchen - for WASH analysis
recode hh_s9q08 (0 = 1)
g byte noKitchen = inlist(hh_s9q08, 1) == 1
g byte indoorKitchen = inlist(hh_s9q08, 2, 4) == 1
g byte dungFuel = inlist(hh_s9q21, 4, 5) == 1

* Sanitation - for WASH analysis
g byte flushToilet = inlist(hh_s9q10, 1, 2) == 1
g byte noToilet = inlist(hh_s9q10, 7, 8) == 1
g byte wasteFert = inlist(hh_s9q12, 5) == 1
g byte sanitWaste = inlist(hh_s9q12, 1, 2, 7) ==1 
g byte wasteThrow = inlist(hh_s9q12, 4, 3) == 1
g byte protWaterRainy = inlist(hh_s9q13, 1, 2, 3, 4, 5, 6, 7) == 1
g byte protWaterDry = inlist(hh_s9q14, 1, 2, 3, 4, 5, 6, 7) == 1
g byte protWaterAll = (protWaterRainy == 1 & protWaterDry == 1)
g byte treatWater = inlist(hh_s9q15, 1) == 1 | inlist(hh_s9q16, 1) == 1

clonevar waterRainyTime = hh_s9q13_a
clonevar waterDryTime = hh_s9q14_a 

* Lighting
g byte elecLight = inlist(hh_s9q19_a, 1, 2, 3) == 1
g byte fireLight = inlist(hh_s9q19_a, 9, 10, 11, 12) == 1 
g byte electricity = hh_s9q19_b ! = .

recode hh_s9q22 (2 = 0)
clonevar phone = hh_s9q22

la var ownHouse "HH owns house"
la var rentHouse "HH rent's house"
la var mudHome"Dwelling made primarily of mud"
la var stoneHome "Dwelling made primarily of stone"
la var metalRoof "Roof made primarily of metal"
la var thatchRoof "Roof made primarily of thatch"
la var mudFloor "Mud floor"
la var noKitchen "No dedicated kitchen space"
la var indoorKitchen "Household has a separate room inside for kitchen"
la var dungFuel "Household uses dung or crop residue for cooking"
la var flushToilet "Household has a flush toilet"
la var noToilet "Household does not have designated toilet area"
la var sanitWaste "Household has trash services available"
la var wasteFert "Uses waste for fertilizer"
la var wasteThrow "Throws away waste"
la var protWaterRainy "Protected water source in rainy season"
la var protWaterDry "Protected water source in dry season"
la var treatWater "Household boils or purify's water"
la var elecLight "Household has electricity connectionf or lighting"
la var fireLight "Household relies on fire/gas source for light"
la var electricity "Household has electricty connection"
la var protWaterAll "Water source protected year round"

* Collapse, relabel and save to pathout for mergine w/ 2014 data
ds(hh_s* household_id saq* ea_id*), not
keep `r(varlist)'

include "$pathdo/copylabels.do"
ds(household_id2), not
#delimit ;
	collapse (max) `r(varlist)',
	by(household_id2) fast; 
#delimit cr
* Reapply variable lables & value labels
include "$pathdo/attachlabels.do"

g year = 2014

merge 1:1 household_id2 using "$wave2/Pub_ETH_HouseholdGeovars_Y2.dta", gen(geo_merge)

* Create household infrastructure index
capture macro drop infraVars
global infraVars "houseSize mudFloor noToilet mudHome metalRoof electricity protWaterAll indoorKitchen phone"

factor  $infraVars if rural == 1, pcf
qui predict infraindex_rur if rural == 1
alpha $infraVars if rural == 1
scree
loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

factor  $infraVars if rural != 1, pcf
qui predict infraindex_urb if rural != 1
alpha $infraVars if rural != 1
scree
loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

sa "$pathout/housing_2014.dta", replace
pappend housing_2012 housing_2014 housing_all
