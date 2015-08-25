/*-------------------------------------------------------------------------------
# Name:		101_analysisShocks.do
# Purpose:	Run basic data validation and analysis of shocks for the panel
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/17
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathout/101_analysisShocks.do", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off

* May have to change below depending on what happends in 100
use $pathout/ETH_201507_LSMS_ALL.dta, clear

bys household_id (year): gen idfind = _n
drop if idfind == 3
drop if idfind == 2 & year == 2012
drop idfind

/* TODO: */

* Recode religion and rural for HOH; Mission has interest in how religion is correlated with shocks
recode religHoh (2 = 3) (5 6 8 = 7)
recode rural (0 = 2)

* Recode the regional variable eliminating space in B-G
label list SAQ01 
labmm SAQ01 6 "B-Gumuz"
lab val saq01 SAQ01

label list rural
labmm rural 2 "Sm.Town" 3 "Lg.Town"
lab val rural rural

label list ftflab
labmm ftflab 99 "Missing"
replace ftfzone = 99 if ftfzone == .
lab val ftfzone ftflab

* Calculate key statistics keeping only the point estimate, se, lower and upper bound and append the count
* Should these go on GitHub repository or in the output folder? 
/* NOTE: Below is only for panel; National and urb/rur are for all of survey 
	INSTALL: net install dm79.pkg*/

forvalue i=2012(2)2014 {
		mean assetShk hazardShk healthShk priceShk if year ==  `i' & ptrack == 2, over(saq01)
		matrix A = r(table)
		matrix N = e(_N)
		matselrc A B, r(1 2 5 6)
		mat C = B\N
		matrix rownames C = mean se lowerB upperB Nobs
		matrix list C
	mat2txt, matrix(C) saving("$pathreg/shock_stats_hh_`i'") replace

	mat drop A B C N
	* Repeat the process for the community variables
	mean agShkComm hazardShkComm healthShkComm priceShkComm if year == `i' & ptrack == 2, over(saq01)
		matrix A = r(table)
		matrix N = e(_N)
		matselrc A B, r(1 2 5 6)
		mat C = B\N
		matrix rownames C = mean se lowerB upperB Nobs
		matrix list C
	mat2txt, matrix(C) saving("$pathreg/shock_stats_comm_`i'") replace
	mat drop A B C N

	mean assetShk hazardShk healthShk priceShk if year ==  `i' & ptrack == 2, over(ftfzone)
		matrix A = r(table)
		matrix N = e(_N)
		matselrc A B, r(1 2 5 6)
		mat C = B\N
		matrix rownames C = mean se lowerB upperB Nobs
		matrix list C
	mat2txt, matrix(C) saving("$pathreg/shock_stats_FTF_`i'") replace
		mat drop A B C N
}
* end loop

* Calculate statistics using survey weights for urban v rural
preserve 
	keep if year == 2012
	svyset ea_id [pweight=pw], strata(saq01) singleunit(centered) 
* Calculate national statistics for shocks using weights
	svy: mean assetShk hazardShk healthShk priceShk rptShock
	matrix A = r(table)
		matrix N = e(_N)
		matselrc A B, r(1 2 5 6)
		mat C = B\N
		matrix rownames C = mean se lowerB upperB Nobs
		matrix list C

		matrix drop A B N
	* Repeat for urban rural split
	svy: mean assetShk hazardShk healthShk priceShk rptShock, over(rural)
		matrix A = r(table)
		matrix N = e(_N)
		matselrc A B, r(1 2 5 6)
		mat D = B\N
		matrix rownames D = mean se lowerB upperB Nobs
		matrix list D
		matrix E = C,D
	mat2txt, matrix(C) saving("$pathreg/shock_stats_2012") replace
restore

preserve 
	keep if year == 2014
	svyset ea_id2 [pweight=pw2], strata(saq01) singleunit(centered) 
* Calculate national statistics for shocks using weights
	svy: mean assetShk hazardShk healthShk priceShk rptShock
	matrix A = r(table)
		matrix N = e(_N)
		matselrc A B, r(1 2 5 6)
		mat C = B\N
		matrix rownames C = mean se lowerB upperB Nobs
		matrix list C

		matrix drop A B N
	* Repeat for urban rural split
	svy: mean assetShk hazardShk healthShk priceShk rptShock, over(rural)
		matrix A = r(table)
		matrix N = e(_N)
		matselrc A B, r(1 2 5 6)
		mat D = B\N
		matrix rownames D = mean se lowerB upperB Nobs
		matrix list D
		matrix E = C,D
	mat2txt, matrix(C) saving("$pathreg/shock_stats_2014") replace
restore

* -------------------------------- Wealth Index ---------------------------------
* Create quintile for different wealth indices & check how key variables fall along distribution
* (Plot results in R for prettifying)
g byte landOwn = (numParcels !=0 & numParcels!=.)
g byte hasToilet = (noToilet == 0)
g roomsPC = houseSize / hhsize

#delimit ; 
local wealthVars ax bed bike blanket car cart clothing dungFuel dvd
		elecLight fireLight flushToilet indoorKitchen jewel metalRoof
		mitad mobile moto mudFloor mudHome noKitchen hasToilet
		ownHouse phone plough protWaterDry protWaterRainy pump radio 
		refrig sat sew shelf sickle sofa stoneHome stove thatchRoof
		tv watch weave well wasteFert wasteThrow roomsPC;
 #delimit cr 

* First, try creating global wealth index over all households in just 2012
/* NOTE: including the 30 or so households that score high has a strong effect
on the predicted pca; Also, weighting also seems to have a strong effect; */

factor `wealthVars' [aweight = pw] if year == 2012 , pcf means
predict wealthIndex2012 if year == 2012
*replace wealthIndex2012 = . if wealthIndex2012>5
g byte pcaFilter = (wealthIndex2012 >6)

factor `wealthVars' [aweight = pw2] if year == 2014 & rural !=3 , pcf means
predict wealthIndex2014 if year == 2014 & rural !=3
replace wealthIndex2014 = . if wealthIndex2014>5
*g byte pcaFilter = (wealthIndex2014 >6)
winsor2 wealthIndex2012 wealthIndex2014, replace cuts(0 99)

factor `wealthVars' if ptrack == 2, pcf means
predict wealthPanel if ptrack == 2
winsor2 wealthPanel if ptrack == 2, replace cuts(0 99)

histogram wealthIndex2014, by(region)
histogram wealthIndex2012, by(region)
histogram wealthPanel if year == 2012, by(region)
histogram wealthPanel if year == 2014, by(region)
winsor2 wealthPanel, replace cuts(0 99)


* Look at the change in wealth from year-to-year
bys household_id (year): g wlthChg = wealthPanel[2]-wealthPanel[1] if ptrack==2
winsor2 wlthChg, replace cuts(1 99)
g wealthIndex = wealthIndex2012
replace wealthIndex = wealthIndex2014 if year == 2014
* Use a 30-tile grouping to show how shocks vary by wealth holdings; The wealth index 	
* is stack
xtile wealthSmooth2012 = wealthIndex2012 [pweight = pw] if year == 2012, nq(10)
xtile wealthSmooth2014 = wealthIndex2014 [pweight = pw2] if year == 2014, nq(10)
xtile wealthIndexSmooth2012 = wealthPanel if year == 2012, nq(10)
xtile wealthIndexSmooth2014 = wealthPanel if year == 2014, nq(10)
g wlthSmooth = wealthIndexSmooth2012
replace wlthSmooth = wealthIndexSmooth2014 if year == 2014

* Generate new weight to account for household houseSize
g hhweight = pw*hhsize
g hhweight2 = pw2*hhsize
xtile wealthQuint2012 = wealthIndex2012 [pweight=hhweight] if year == 2012, nq(5)
xtile wealthQuint2014 = wealthIndex2014 [pweight=hhweight2] if year == 2014, nq(5)

g wealthQuints = wealthQuint2012
replace wealthQuints = wealthQuint2014 if year == 2014

* Export a cut of data to R for graphing in ggplot
preserve
	keep if ptrack == 2
	encode household_id, gen(HID)
		#delimit ; 
		local wealthVars ax bed bike blanket car cart clothing dungFuel dvd
			elecLight fireLight flushToilet indoorKitchen jewel metalRoof
			mitad mobile moto mudFloor mudHome noKitchen hasToilet
			ownHouse phone plough protWaterDry protWaterRainy pump radio 
			refrig sat sew shelf sickle sofa stoneHome stove thatchRoof
			tv watch weave well wasteFert wasteThrow roomsPC
			FCS dietDiv assetShk hazardShk healthShk priceShk rptShock goodcope badcope
			wealthSmooth2012 wealthSmooth2014 wealthQuint2012 wealthQuint2014 
			wealthIndexSmooth2012 wealthIndexSmooth2014 electricity
			crowding TLUtotal totMonFoodlack HID wlthSmooth religHoh wealthQuints;

 		#delimit cr 
	keep household_id year wealthPanel `wealthVars' femhead agehead region saq01 rural ftfzone

	export delimited "$pathexport/Panel.wealth.analysis.csv", replace
restore


************ TODO **************
/* Determine list of assets and variables over which we want to look at these
relationships */
* Determine list of assets overwhich we would like to show the relationship for
* long-run wealth and asset holdings.
cap drop xvar
g xvar = wealthSmooth2012

* Basic needs include: hasToilet mobile protWaterDry protWaterRainy thatchRoof electricity

twoway (lowess tv xvar) (lowess mobile xvar) /*
*/(lowess hasToilet xvar) (lowess protWaterDry xvar) /*
*/(lowess thatchRoof xvar)(lowess electricity xvar), by(saq01)

twoway(lowess TLUtotal xvar)(lowess landOwn xvar) if rural ==1, by(region)
twoway(lowess dietDiv xvar), by(region ftfzone)
twoway(lowess hfiasindex_rur xvar), by(region ftfzone)

twoway (lowess hazardShk wealthSmooth2012) (lowess priceShk wealthSmooth2012) /*
*/(lowess healthShk wealthSmooth2012) (lowess assetShk wealthSmooth2012), by( region)

twoway (lowess julyFoodShort xvar if ftfzone!=1) (lowess julyFoodShort xvar if ftfzone == 1), by(region)
twoway (lowess FCS wealthSmooth2012 if ftfzone!=1) (lowess FCS wealthSmooth2012 if ftfzone == 1) /*
*/ (lowess FCS wealthSmooth2014 if ftfzone != 1) (lowess FCS wealthSmooth2014 if ftfzone == 1), by(region)

* After filter out the outliers, try refitting pcf to get a better shape of wealth 

/* IDEA: Take the core variables that matter to people in the lab such as:
	Mobiles, sanitation, electricity, phone, kitchen, floor type, water details
	and plot them along the predicted wealth index spectrum by region;
	Highlight regions where the patterns are different; Combine this with a cluster
	GIS analysis on the predicted wealth index to show areas that are poor beyond
	chance alone; Plot is a basic smoothed lowess along wealth indices;
	THEN: show how these are changing across time & in FTFzones;

	Another idea: Look at how the shocks vary across the wealth spectrum, by 
	regions and FTF zones; Are they targeting well? Preliminiary investigation suggests
	that they are.

	In Addis, it appears that price volatility is the major shock reported, as expected
	asset poor houses are much more likely to report the shock, but even asset wealthy
	households also report the shock.
*/

* Check for outliers, drop those with extreme values ()

* Reported food shortages by regions; May be just as easy to plot by month, by region & by FTF zone
set more off
forvalues i = 2012(2)2014 {
	qui mean janFoodShort febFoodShort marFoodShort aprFoodShort mayFoodShort juneFoodShort julyFoodShort /*
	*/ augFoodShort septFoodShort octFoodShort novFoodShort decFoodShort if year == `i', over(region)
	matrix Shk`i'  = r(table)
	matrix list Shk`i'
	mat2txt, matrix(Shk`i') saving("$pathexport/foodShort_`i'") replace
}


/* Send a cut of data to Jamison with percentages calculated for shocks at the EA level. The reason 
	for this cut is that the cluster analysis package in ArcMap is not suitable for binary data. 
*/
preserve
egen eaCount = count(pw), by(ea_id)
collapse (mean) assetShk hazardShk healthShk priceShk rptShock latitude longitude  if year == 2012, by(ea_id)
export delimited "$pathout/shocks_percent_2012.csv", replace
restore

preserve
collapse (mean) assetShk hazardShk healthShk priceShk rptShock latitude longitude if year == 2014, by(ea_id2)
export delimited "$pathout/shocks_percent_2014.csv", replace
restore

* Reduce outliers in land ownership
winsor2 avgField, replace cuts(0 99)
replace avgField = avgField + 1
g avgFieldlog = ln(avgField)


* Create variable for total land owned -- use categories
g landArea = 0 if landOwn == 0
replace areaField = 0 if areaField == .
xtile landQtile = areaField if year == 2012 & ptrack == 2, nq(4)
xtile lqtmp = areaField if year == 2014 & ptrack == 2, nq(4)
replace landQtile = lqtmp if year == 2014 & ptrack == 2 
table landQtile year, c(mean areaField)

g landHectares = areaField/10000
la var landHectares "land owned in hectares"

* Fix any missing household sizes based on variable from raw data
replace hhsize = hh_saq09 if hhsize ==.

**********************
* Missingeness check *
**********************
/* NOTE: Make decisions on a few missing variables; 
	educAdultM  (created new variable replacing missing w/ 0's)
	agehead  	(101 missing)
	religHoh 	(130 missing)
	depRatio 	(302 missing - use ae instead)
	wealthIndex (171 missing)
	TLUtotal 	(1750 missing)
*/

* Create a censored variable for TLUtotal
mdesc TLUtotal
clonevar TLUtotal_cnsrd = TLUtotal
replace TLUtotal_cnsrd = 0 if TLUtotal_cnsrd == .
replace ftfzone = . if ftfzone == 99

* Save a cut of data before starting analysis
preserve 
keep if ptrack == 2
saveold "$pathexport/ETH_201508_analysis_panel.dta", replace 
restore


* Create a new education variable that chunks the highest male/female out into categories; No educ is the append_base
clonevar educAdultM_cat = educAdultM_cnsrd
recode educAdultM_cat (2 3 = 1) (5 4 = 2) (6 = 3)
clonevar educAdultF_cat = educAdultF_cnsrd
recode educAdultF_cat (2 3 = 1) (5 4 = 2) (6 = 3)
la def educLab 0 "No education" 1 "Primary" 2 "Secondary" 3 "Tertiary"
la val educAdultM_cat educLab
la val educAdultF_cat educLab


* SNPP and Oromia have the most shocks; Cluster standard erros at the regional level (saq01)
* Results vary if using only robust standard errors
global demog "agehead c.agehead#c.agehead i.femhead i.marriedHoh vulnHead i.religHoh"
global educ "literateHoh educAdultM_cnsrd educAdultF_cnsrd gendMix ae mlabor flabor hhsize"
global educ2 "i.literateHoh "
global educ2 "literateHoh i.educAdultM_cat i.educAdultF_cat gendMix depRatio mlabor flabor hhsize"
global ltassets " iddirMemb" 
global ltassets2 "TLUtotal_cnsrd wealthIndex landHectares ib(4).landQtile iddirMemb"
global ltassets3 "l2.TLUtotal_cnsrd l2.wealthIndex l2.landHectares ib(4)l2.landQtile l2.iddirMemb" 
global ltassets4 "l2.TLUtotal_cnsrd l2.wealthIndex cl2.wealthIndex#cl2.wealthIndex l2.landHectares ib(4)l2.landQtile l2.iddirMemb"
global geog "dist_road dist_popcenter dist_market dist_borderpost i.ftfzone"
global shocks "priceShk hazardShk"
global year1 "if year == 2012 & ptrack == 2, cluster(ea_id)"
global year2 "if year == 2014 & ptrack == 2, cluster(ea_id)"

* Oromia is the base
* Turn on Stata's baselevels so we can see base cases for all categoricals
set showbaselevels off, permanently
encode household_id, gen(HID)
xtset HID year
est clear

set more off
* Estimate 2014 shocks using Linear probabilty model and lagged values for assets that could be used for coping; 
/* Create a loop for the major shocks that does the following:
	1) Estimate five model specifications, 2 for 2012 and 3 for 2014
	2) Saves estimates and writes them in two forms to text files
	3) creates a variable flagging the sample used in specifications 2 and 5
*/

* Create an updated macro for github repository
global pathgit2 "C:/Users/Tim/Documents/GitHub/Ethiopia/Analysis"

est clear
local wrdlist price hazard health illness any q1HFIAS foodshortage
local i = 1
foreach x of varlist priceShk hazardShk healthShk illnessShk rptShock q1_HFIAS numMonthFoodShort  {
	* Grab word from word list above
	local a: word `i' of `wrdlist'
	display "`a'"
	
	* Run 5 regression specifications using global macros defined above
	qui eststo `x'_1, title("`a' 2012.1"): reg `x' $demog $educ2 $ltassets $geog ib(4).regionAll $year1
	qui eststo `x'_2, title("`a' 2012.2"): reg `x' $demog $educ2 $ltassets2 $geog ib(4).regionAll $year1
	capture g byte `x'_sample2012 = e(sample) == 1
	qui eststo `x'_3, title("`a' 2014.1"): reg `x' $demog $educ2 $ltassets $geog  ib(4).regionAll $year2 
	qui eststo `x'_4, title("`a' 2014.2"): reg `x' $demog $educ2 $ltassets2 $geog ib(4).regionAll $year2 
	qui eststo `x'_5, title("`a' 2014.3"): reg `x' $demog $educ2 $ltassets3 $geog ib(4).regionAll $year2 
	capture g byte `x'_sample2014 = e(sample) == 1
	
	* Print results to screen and to text files in both wide and long formats
	esttab `x'_*, se star(* 0.10 ** 0.05 *** 0.01) label
	esttab `x'_* using "$pathreg/`x'.csv", se star(* 0.10 ** 0.05 *** 0.001) label replace
	esttab `x'_* using "$pathgit2/`x'Wide.csv", wide plain se mlabels(none) label replace
	display in yellow "Executed regression for `x' variable."
	local i = `++i'
}
*end
estimates dir
esttab priceShk_2 priceShk_5 hazardShk_2 hazardShk_5 healthShk_2 healthShk_5, star(* 0.10 ** 0.05 *** 0.01) label not
esttab illnessShk_2 illnessShk_5 rptShock_2 rptShock_5 q1_HFIAS_2 q1_HFIAS_5 /*
*/numMonthFoodShort_2 numMonthFoodShort_5, star(* 0.10 ** 0.05 *** 0.01) label not

esttab *_5 using "$pathgit2/allWide.csv", wide plain se mlabels(none) label replace /*
*/ addnotes(Order is price, hazard, health, illness, any, q1HFIAS, Number of months with food shortage)


* Look at pooled probits for shocks across the two years
eststo pldPrice:  probit priceShk $demog $educ2 $ltassets2 $geog ib(4).regionAll i.year, cluster(ea_id)
eststo pldHazard: probit hazardShk $demog $educ2 $ltassets2 $geog ib(4).regionAll i.year, cluster(ea_id)
eststo pldHealth: probit healthShk $demog $educ2 $ltassets2 $geog ib(4).regionAll i.year, cluster(ea_id)
eststo pldIllness:probit illnessShk $demog $educ2 $ltassets2 $geog ib(4).regionAll i.year, cluster(ea_id)
eststo pldAny:	  probit rptShock $demog $educ2 $ltassets2 $geog ib(4).regionAll i.year, cluster(ea_id)
eststo pldHFIAS1: probit q1_HFIAS $demog $educ2 $ltassets2 $geog ib(4).regionAll i.year , cluster(ea_id)
*eststo fe: xtreg numMonthFoodShort $demog $educ2 $ltassets2 $geog ib(4).regionAll i.year, i(hhid) fe cluster(ea_id)
esttab pld*, se star(* 0.10 ** 0.05 *** 0.01) label
esttab pld* using "$pathgit2/PooledProbitsWide.csv", wide plain se mlabels(none) label replace

/* Synthesis: 
	* Having a Muslim HOH or Other is not good;
	* Secondary education for males in HHs relative to none is good
	* Tertiary education for females relative to none is good 
	* More males, more hazard shocks (more exposure to climate shocks)
	* Assets (TLUS & wealth index) help insulate from shocks; Livestock help w/ food security;
	* Lack of market accessibility increases vulnerability
	* Relative to Oromia, if you live in Afar you're more likely to have a health shock
	* Somalie, relative to Oromia, are much more likely to have price and hazard shocks
	* All shocks analyzed, decline from 2012 to 2014;
	* Gambelia is better than Oromia in nearly every shock category
	* Land constraints appear to be driving food security issues; 
*/

* Now model FCS and dietary diversity -- investigate the use of poisson or zero-truncated poisson for overdispersion in count data
* Can also look at adding in EA_id fixed-effects, but not sure how appropriate this is given the small sample sizes for each ea.
local wrdlist FCS1 FCS2 dietDiv1 dietDiv2
local i = 1
foreach x of varlist FCS fcsMin dietDiv dd {
	* Grab word from word list above
	local a: word `i' of `wrdlist'
	display "`a'"
	
	* Run 5 regression specifications using global macros defined above
	qui eststo `x'_1, title("`a' 2012.1"): reg `x' $demog $educ2 $ltassets $geog ib(4).regionAll $year1
	qui eststo `x'_2, title("`a' 2012.2"): reg `x' $demog $educ2 $ltassets2 $geog i.priceShk i.hazardShk ib(4).regionAll $year1
	capture g byte `x'_sample2012 = e(sample) == 1
	qui eststo `x'_3, title("`a' 2014.1"): reg `x' $demog $educ2 $ltassets $geog  ib(4).regionAll $year2 
	qui eststo `x'_4, title("`a' 2014.2"): reg `x' $demog $educ2 $ltassets2 $geog il2.priceShk il2.hazardShk ib(4).regionAll $year2
	qui eststo `x'_5, title("`a' 2014.3"): reg `x' $demog $educ2 $ltassets3 $geog il2.priceShk il2.hazardShk ib(4).regionAll $year2
	capture g byte `x'_sample2014 = e(sample) == 1
	
	* Print results to screen and to text files in both wide and long formats
	*esttab `x'_*, se star(* 0.10 ** 0.05 *** 0.01) label
	esttab `x'_* using "$pathreg/`x'.txt", se star(* 0.10 ** 0.05 *** 0.001) label replace
	*esttab `x'_* using "$pathgit2/`x'Wide.txt", wide plain se mlabels(none) label replace
	display in yellow "Executed regression for `x' variable."
	local i = `++i'
}
esttab FCS_5 fcsMin_5 dietDiv_5 dd_5 using "$pathgit2/allWide.csv", wide plain se mlabels(none) label replace /*
*/ addnotes(Order is FCS, FCS_Rversion, dietDiv, dietDiv_Rversion)

* 
esttab FCS_2 FCS_5 fcsMin_2 fcsMin_5 dietDiv_2 dietDiv_5 dd_2 dd_5, star(* 0.10 ** 0.05 *** 0.01) label not


* Look at dietary diversity outcomes
* Estimate poisson or zero-truncated poisson b/c dietary diversity cannot be 0
est clear
eststo p2012, title("Diet Diversity 2012"): tpoisson dietDiv $demog $educ2 $ltassets $geog ib(4).regionAll if ptrack == 2 & year == 2012, ll(0) cluster(saq01) 
eststo p20122, title("Diet Diversity 2012"): tpoisson dietDiv $demog $educ2 $ltassets2 $geog i.priceShk i.hazardShk ib(4).regionAll if ptrack == 2 & year == 2012, ll(0) cluster(saq01)  
eststo p20141, title("Diet Diversity 2014"): tpoisson dietDiv $demog $educ2 $ltassets $geog i.priceShk i.hazardShk  ib(4).regionAll if ptrack == 2 & year == 2014, ll(0) cluster(saq01)  
eststo p20142, title("Diet Diversity 2014"): tpoisson dietDiv $demog $educ2 $ltassets2 $geog il2.priceShk il2.hazardShk ib(4).regionAll if ptrack == 2 & year == 2014, ll(0) cluster(saq01)  
eststo p20143, title("Diet Diversity 2014"): tpoisson dietDiv $demog $educ2 $ltassets3 $geog il2.priceShk il2.hazardShk  ib(4).regionAll if ptrack == 2 & year == 2014, ll(0) cluster(saq01)  
esttab, se star(* 0.10 ** 0.05 *** 0.01) label
esttab using "$pathreg/dietDivZT.txt", se star(* 0.10 ** 0.05 *** 0.001) label replace 

*Keep a subset for exporting and running spatial filter models in R
keep HID year household_id saq01 $educ educAdultM educAdultF TLUtotal TLUtotal_cnsrd wealthIndex landHectares landQtile 		/*
*/ iddirMemb dist_road dist_popcenter dist_market dist_borderpost ftfzone priceShk illnessShk	/*
*/ hazardShk healthShk rptShock dietDiv dd FCS fcsMin agehead ageheadsq femhead 	/*
*/ marriedHoh vulnHead religHoh ptrack latitude longitude priceShk_sample2012 priceShk_sample2014 educAdultM_cat educAdultF_cat

* Create lagged variables (easier to do in Stata)
foreach x of varlist wealthIndex TLUtotal TLUtotal_cnsrd priceShk hazardShk iddirMemb landQtile landHectares{
	g `x'_lag = l2.`x'
}
*end


saveold "$pathgit/Data/ETH_201508_analysis_panel.dta", replace 


* Export two cuts of data for Jamison to run GWRs and Spat Filter models
preserve
keep if ptrack == 2 
order household_id saq01 HID ptrack latitude longitude rptShock priceShk hazardShk healthShk /*
*/ FCS dd fcsMin agehead ageheadsq femhead marriedHoh vulnHead religHoh	/*
*/  $educ educAdultM educAdultF ftfzone TLUtotal TLUtotal_cnsrd* wealthIndex landHectares landQtile iddirMemb

export delimited using "$pathexport/ETH_201508_2012_analysis.csv" if year == 2012 & priceShk_sample2012 == 1, replace nolabel
export delimited using "$pathexport/ETH_201508_2014_analysis.csv" if year == 2014 & priceShk_sample2014 == 1, replace nolabel
restore

