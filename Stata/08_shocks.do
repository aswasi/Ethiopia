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


preserve
	set more off
	/* Create an exploratory heatmap of shock data; First create some new variables */
	recode hh_s8q01 (2 = 0)
	egen pctShock = mean(hh_s8q01), by(hh_s8q0a)
	egen totShock = total(hh_s8q01), by(hh_s8q0a)
	tab hh_s8q02, gen(rank)
	egen rankShock1 = mean(rank1), by(hh_s8q0a)
	egen rankShock2 = mean(rank2), by(hh_s8q0a)

	ren hh_s8q03_a income
	ren hh_s8q03_b assets
	ren hh_s8q03_c foodprod
	ren hh_s8q03_d foodstock
	ren hh_s8q03_e foodpurch

	local welfvars income assets foodprod foodstock foodpurch
	foreach x of local welfvars {
		tab `x', gen(`x'_wf)
	}
	*end



	egen shockFreqYr = mean(hh_s8q05), by(hh_s8q0a)
	egen shockFreq5yr = mean(hh_s8q06), by(hh_s8q0a)

	* Investigate collapsed results in R using d3heatmap package
	collapse (mean) pctShock totShock rankShock1 rankShock2 income_wf1-shockFreq5yr, by(hh_s8q0a)

	* Replace 1, 2, 3 with increase, decrease, no change
	rename pct* *
	rename *_wf1* *_increase
	rename *_wf2* *_decrease
	rename *_wf3* *_nochange


	ren rankShock1 mostSevere
	ren rankShock2 severe

	export delimited "$pathexport/shock_heatmap.csv", replace

restore




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
g byte ag 		= inlist(hh_s8q00, 111, 112, 108) &  inlist(hh_s8q02, 1, 2, 3)
g byte conflict = inlist(hh_s8q00, 114, 117) &  inlist(hh_s8q02, 1, 2, 3)
g byte disaster = inlist(hh_s8q00, 104, 105, 106, 107, 113) &  inlist(hh_s8q02, 1, 2, 3)
g byte financial= inlist(hh_s8q00, 103) &  inlist(hh_s8q02, 1, 2, 3)
g byte health 	= inlist(hh_s8q00, 101, 102) &  inlist(hh_s8q02, 1, 2, 3)
g byte other 	= inlist(hh_s8q00, 115, 116, 118) &  inlist(hh_s8q02, 1, 2, 3)
g byte priceup	= inlist(hh_s8q00, 110) &  inlist(hh_s8q02, 1, 2, 3)
g byte pricedown= inlist(hh_s8q00, 109) &  inlist(hh_s8q02, 1, 2, 3)

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



* Create variable tracking impact of food price rise and drought on househohlds (two most common)
ren hh_s8q03_a income
ren hh_s8q03_b assets
ren hh_s8q03_c foodprod
ren hh_s8q03_d foodstock
ren hh_s8q03_e foodpurch

local welfvars income assets foodprod foodstock foodpurch
foreach x of local welfvars {
	tab `x', gen(`x'_wf)
	}
*end

local welfvars income assets foodprod foodstock foodpurch
	foreach x of local welfvars {
	g byte `x'_prShk = `x'_wf2 == 1 if hh_s8q00 == 110
	g byte `x'_dryShk = `x'_wf2 == 1 if hh_s8q00 == 104
	g byte `x'_illShk = `x'_wf2 == 1 if hh_s8q00 == 102
	la var `x'_prShk "`x' welfare decrease due to food price shock"
	la var `x'_dryShk "`x' welfare decrease due to drought shock"
	la var `x'_illShk "`x' welfare decrease due to household illness shock"
	}
*end

egen prShkwf = rsum(income_wf2 assets_wf2 foodprod_wf2 foodstock_wf2 foodpurch_wf2) if hh_s8q00== 110
egen dryShkwf = rsum(income_wf2 assets_wf2 foodprod_wf2 foodstock_wf2 foodpurch_wf2) if hh_s8q00 == 104
egen illShkwf = rsum(income_wf2 assets_wf2 foodprod_wf2 foodstock_wf2 foodpurch_wf2) if hh_s8q00 == 102
recode hh_s8q01 (2 = 0)
egen totShocks = total(hh_s8q01), by(household_id)


la var prShkwf  "Total categories across which welfare declined due to food price rise"
la var dryShkwf "Total categories across which welfare declined due to drought"
la var illShkwf "Total categories across which welfare declined due to illness of hh member"

* Clone the geographic variables but with new names
local geo "region zone woreda town subcity kebele EA hid"
local n: word count `geo'
forvalues i = 1/`n' {
	local a: word `i' of `geo'
	clonevar `a' = saq0`i'
	drop saq0`i'
	}
*end

recode region (2 5 6 12 13 15 = 20)
labmm SAQ01 20 "Other regions"
la val region SAQ01

* Collapse data to househld level and merge back with GIS info
ds (hh_s8* income assets foodprod foodstock foodpurch *_wf1 *_wf2 *_wf3), not
keep `r(varlist)'


* Collapse everything down to HH-level using max values for all vars
* Copy variable labels to reapply after collapse
include "$pathdo/copylabels.do"
ds(household_id ea_id hid region zone woreda town subcity kebele EA rural), not
#delimit ;
	collapse (max) `r(varlist)',
	by(household_id ea_id hid region zone woreda town subcity kebele EA rural) fast; 
#delimit cr
* Reapply variable lables & value labels
include "$pathdo/attachlabels.do"

* Create a total shocks variable which cuts data into buckets
clonevar totalShocks = totShocks
recode totalShocks (11 8 7 6 5 4  = 3)
la def ts 0 "No Shocks" 1 "One" 2 "Two" 3 "Three or more"
la val totalShocks ts

* Create total shock count from the defined buckets
egen totShock2 = rsum2(ag conflict disaster financial health other priceup pricedown)
la var totShock2 "Total housheold shocks reported"

* Merge in geovariables and sampling weights
merge 1:1 household_id using "$pathraw/Pub_ETH_HouseholdGeovariables_Y1.dta", gen(geo_merge)

* Extra household weights
preserve
clear
use "$pathraw/sect1_hh_w1.dta"
collapse pw saq01, by(household_id ea_id)
save "$pathout/hhweights2012.dta", replace
restore

* Merge data together with sampling weights and create geovars
merge 1:1 household_id using "$pathout/hhweights2012.dta", gen(hhwgt_merge)
egen geoGroup = group(LAT_DD_MOD LON_DD_MOD )

* Survey set the data for projections
svyset ea_id [pweight=pw], strata(saq01) singleunit(centered)

* Other shocks
g byte othershock = (other==1 | pricedown ==1 | conflict ==1 | financial == 1)

* Summarize shocks over population and by shock type
svy:mean assetShk crimeShk employShk hazardShk healthShk priceShk

* Check results over regions
svy:mean assetShk crimeShk employShk hazardShk healthShk priceShk, over(region)

svy:mean goodcope badcope othcope, over(region)

* Export a cut of data to .csv for sharing (if needed)
 export delimited using "$pathexport\LSMS.shocks.exploratory2012.csv", replace

****************************
* --- Process 2014 data ---*
****************************


