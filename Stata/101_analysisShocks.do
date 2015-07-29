clear
capture log close
log using "$pathout/101_analysisShocks.do", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off

* May have to change below depending on what happends in 100
use $pathout/ETH_201507_LSMS_ALL.dta, clear

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

* Calculate key statistics keeping only the point estimate, se, lower and upper bound and append the count
* Should these go on GitHub repository or in the output folder? 
/* NOTE: Below is only for panel; National and urb/rur are for all of survey */
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

histogram wealthIndex2014, by(region)
histogram wealthIndex2012, by(region)
histogram wealthPanel if year == 2012, by(region)
histogram wealthPanel if year == 2014, by(region)

* Look at the change in wealth from year-to-year
bys household_id (year): g wlthChg = wealthPanel[2]-wealthPanel[1] if ptrack==2
winsor2 wlthChg, replace cuts(1 99)
g wealthIndex = wealthIndex2012
replace wealthIndex = wealthIndex2014 if year == 2014
* Use a 30-tile grouping to show how shocks vary by wealth holdings; The wealth index 	
* is stack
xtile wealthSmooth2012 = wealthIndex2012 [pweight = pw] if year == 2012, nq(10)
xtile wealthSmooth2014 = wealthIndex2014 [pweight = pw2] if year == 2014, nq(10)

* Generate new weight to account for household houseSize
g hhweight = pw*hhsize
g hhweight2 = pw2*hhsize
xtile wealthQuint2012 = wealthIndex2012 [pweight=hhweight] if year == 2012, nq(5)
xtile wealthQuint2014 = wealthIndex2014 [pweight=hhweight2] if year == 2014, nq(5)








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
bob

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

* SNPP and Oromia have the most shocks
global demog "agehead ageheadsq femhead marriedHoh literateHoh educAdultM educAdultF gendMix depRatio mlabor flabor hhsize"
global ltassets "TLUtotal wealthIndex" 
global land "landOwn"
global geog "dist_road dist_popcenter dist_market dist_borderpost"
global intFE ""


* Oromia is the base
g byte AddisTag = (region == 14)
eststo p2012, title("Price shock 2012"):reg priceShk $demog $ltassets $geog ib(4).region i.rural ib(1).weeklyMkt if year ==2012, robust 
eststo p2014, title("Price shock 2014"):reg priceShk $demog $ltassets $geog ib(4).region i.rural ib(1).weeklyMkt if year ==2014, robust
coefplot p2012 p2014, xline(0, lwidth(thin) lcolor(gray)) drop(_cons)


eststo p2012, title("Price shock 2012"):reg hazardShk $demog $ltassets $geog ib(4).region ib(1).weeklyMkt if year ==2012 & ptrack == 2, robust 
eststo p2014, title("Price shock 2014"):reg hazardShk $demog $ltassets $geog ib(4).region ib(1).weeklyMkt if year ==2014 & ptrack == 2, robust 
coefplot p2012 p2014, xline(0, lwidth(thin) lcolor(gray)) drop(_cons)


eststo p2012, title("Price shock 2012"):reg healthShk $demog $ltassets $geog ib(4).region ib(1).weeklyMkt if year ==2012 & ptrack == 2, robust 
eststo p2014, title("Price shock 2014"):reg healthShk $demog $ltassets $geog ib(4).region ib(1).weeklyMkt if year ==2014 & ptrack == 2, robust 
coefplot p2012 p2014, xline(0, lwidth(thin) lcolor(gray)) drop(_cons)



* Look at dietary diversity outcomes
graph matrix $demog $ltassets if year == 2012