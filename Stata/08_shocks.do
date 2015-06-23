/*-------------------------------------------------------------------------------
# Name:		08_shocks
# Purpose:	Process shocks data form 2012 and 2014
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/23
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/
capture log close
clear
log using "$pathlog/08_shocks.txt", replace
di in yellow "`c(current_date)' `c(current_time)'"

* Load
use "$wave1\sect8_hh_w1.dta" 


* Look at shocks -- Price rise of food item number 1 shock, followed by drought
tab hh_s8q0a if hh_s8q01 ==1, sort



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

local vlist death ill occup drought flood landsld h20log crop prFall prRise inpRise livestock fire crime2 involLoss displcd unrest
local i = 101 
foreach x of local vlist {
	g byte `x'Shk = inlist(hh_s8q00, `i') & rptShock == 1 &  inlist(hh_s8q02, 1, 2)
	la var `x'Shk "HH reported `x' shock as first or second most severe"
	g byte `x'ShkSev = inlist(hh_s8q00, `i') & rptShock == 1 &  inlist(hh_s8q02, 1)
	la var `x'ShkSev "HH reported `x' shock as most severe"
	g byte `x'Shkany = inlist(hh_s8q00, `i') & rptShock == 1 &  inlist(hh_s8q02, 1)
	la var `x'Shkany "HH reported `x' shock of any severity"
}

* Create standard categories for shocks using WB methods
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

* Create shock buckets
/* World Bank classifications
	Prices (inputs, outputs, food)
	Hazards (natural, droughts, floods)
	Employment (jobs, wages)
	Assets (house, land, livestock)
	Health (death, illness)
	Crime & Safety (theft, violence)	*/

g byte assetShk = inlist(hh_s8q00, 108, 112, 115, 116) &  inlist(hh_s8q02, 1, 2)
g byte crimeShk = inlist(hh_s8q00, 114, 117) &  inlist(hh_s8q02, 1, 2)
g byte employShk = inlist(hh_s8q00, 103) &  inlist(hh_s8q02, 1, 2)
g byte hazardShk = inlist(hh_s8q00, 104, 105, 106, 107, 113) &  inlist(hh_s8q02, 1, 2)
g byte healthShk = inlist(hh_s8q00, 101, 102) &  inlist(hh_s8q02, 1, 2)
g byte priceShk = inlist(hh_s8q00, 109, 110, 111) &  inlist(hh_s8q02, 1, 2)

la var assetShk "Assets (house, land, livestock) shock"
la var crimeShk "Crime & Safety (theft, violence) shock"
la var employShk "Employment (jobs, wages) shock"
la var hazardShk "Hazard (natural, droughts, floods) shock"
la var healthShk "Health (death, illness) shock"
la var priceShk "Price (inputs, outputs, food) shock"

/* Coping Mechanisms - What are good v. bad coping strategies? From (Heltberg et al., 2013)
	http://siteresources.worldbank.org/EXTNWDR2013/Resources/8258024-1352909193861/
	8936935-1356011448215/8986901-1380568255405/WDR15_bp_What_are_the_Sources_of_Risk_Oviedo.pdf
	Good Coping: use of savings, credit, asset sales, additional employment, 
					migration, and assistance
	Bad Coping: increases vulnerabiliy* compromising health and edudcation 
				expenses, productive asset sales, conumsumption reductions 
				*/

g byte goodcope = inlist(hh_s8q04_a, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 16) & rptShock == 1 
g byte badcope 	= inlist(hh_s8q04_a,  5, 9, 11, 13, 14, 15, 17) & rptShock == 1
g byte othcope = inlist(hh_s8q04_a, 18, 19, 20, 25, 60)

* Result of top 3 shocks
g fPriceInc = 


* Label variables
la var goodcope "Good primary coping strategy"
la var badcope "Bad primary coping strategy"
la var incReduc "Income reduction due to shock"
la var assetReduc "Asset reduction due to shock"
la var foodProdReduc "Food production reduction due to shock"
la var foodPurchReduc "Food purchase reduction due to shock"
















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

* Other shocks
g byte othershock = (other==1 | pricedown ==1 | conflict ==1 | financial == 1)

* Summarize shocks over population and by shock type
svy:mean ag conflict disaster financial health other priceup pricedown totShock othershock

* Check results over regions
svy:mean ag conflict disaster financial health other priceup pricedown totShock othershock, over(region)

* Export a cut of data to .csv for sharing (if needed)
 export delimited using "$pathexport\LSMS.shocks.exploratory.csv", replace
