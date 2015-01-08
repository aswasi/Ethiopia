/*-------------------------------------------------------------------------------
# Name:		101_PreliminaryAnalysis
# Purpose:	Create preliminary analysis Ethiopia 
# Author:	Tim Essam, Ph.D.
# Created:	12/28/2014
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/
capture log close
clear
log using "$pathlog/prelimAnalysis", replace

* Load
use "$pathraw\sect8_hh_w1.dta" 


* Look at shocks
tab hh_s8q0a
tab hh_s8q00
tab hh_s8q01
tab hh_s8q01, nol

* Flag observations with a shock
g byte rptShock = hh_s8q01==1

tab hh_s8q0a if rptShock == 1
la var rptShock "Household reported a shock"

* Create preliminary shock variables (first or 2nd most severe shock)
/*ag 		= other crop damage; input price increase; death of livestock
* conflit 	= theft/robbery/violence
* disaster 	= drought, flood, heavy rains, landslides, fire
* financial	= loss of non-farm job
* priceup	= price rise of food item
* pricedown = price fall of food items
* health	= death of hh member; illness of hh member
* other 	= loss of house; displacement; other */ 
g byte ag 		= inlist(hh_s8q00, 111, 112, 108) &  inlist(hh_s8q02, 1, 2)
g byte conflict = inlist(hh_s8q00, 114, 117) &  inlist(hh_s8q02, 1, 2)
g byte disaster = inlist(hh_s8q00, 104, 105, 106, 107, 113) &  inlist(hh_s8q02, 1, 2)
g byte financial= inlist(hh_s8q00, 103) &  inlist(hh_s8q02, 1, 2)
g byte health 	= inlist(hh_s8q00, 101, 102) &  inlist(hh_s8q02, 1, 2)
g byte other 	= inlist(hh_s8q00, 115, 116, 118) &  inlist(hh_s8q02, 1, 2)
g byte priceup	= inlist(hh_s8q00, 110) &  inlist(hh_s8q02, 1, 2)
g byte pricedown= inlist(hh_s8q00, 109) &  inlist(hh_s8q02, 1, 2)

la var ag "Agriculture"
la var conflict "Conflict"
la var disaster "Disaster"
la var financial "Financial"
la var health "Health"
la var other "Other"
la var priceup "Price rise"
la var pricedown "Price fall"


* Collapse data to househld level and merge back with GIS info
ds (hh_s8*), not
keep `r(varlist)'

* Clone the geographic variables but with new names
local geo "region zone woreda town subcity kebele EA hid"
local n: word count `geo'
forvalues i = 1/`n' {
	local a: word `i' of `geo'
	clonevar `a' = saq0`i'
	drop saq0`i'
	}
*end

* Collapse everything down to HH-level using max values for all vars
* Copy variable labels to reapply after collapse
include "$pathdo/copylabels.do"

#delimit ;
	collapse (max) ag conflict disaster financial health other priceup pricedown,
	by(household_id ea_id hid region zone woreda town subcity kebele EA rural) fast; 
	
#delimit cr
	
* Reapply variable lables & value labels
include "$pathdo/attachlabels.do"
	
* Create total shock count
egen totShock = rsum2(ag conflict disaster financial health other priceup pricedown)
la var totShock "Total housheold shocks reported"

* Merge in geovariables and sampling weights
merge 1:1 household_id using "$pathraw/Pub_ETH_HouseholdGeovariables_Y1.dta", gen(geo_merge)

* Extra household weights
preserve
clear
use "$pathraw/sect1_hh_w1.dta"
collapse pw saq01, by(household_id ea_id)
save "$pathout/hhweights.dta", replace
restore

* Merge data together with sampling weights and create geovars
merge 1:1 household_id using "$pathout/hhweights.dta", gen(hhwgt_merge)
egen geoGroup = group(LAT_DD_MOD LON_DD_MOD )


* Survey set the data for projections
svyset ea_id [pweight=pw], strata(saq01) singleunit(centered)

* Summarize shocks over population and by shock type
svy:mean ag conflict disaster financial health other priceup pricedown totShock

* 
