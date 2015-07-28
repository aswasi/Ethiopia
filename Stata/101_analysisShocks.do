clear
capture log close
log using "$pathout/101_analysisShocks.do", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off

* May have to change below depending on what happends in 100
use $pathout/ETH_201507_LSMS_ALL.dta, clear


/* TODO: 
	*Group religHoh into four buckets
	*Create statistics at community level for price shocks
	*Provide a cut of stats for Laura at regional level (With n and std err)
	*Create a new wealth index that ignores new urban areas (basically just for panel)
	*Fix the regional labels
*/

* Recode the regional variable



egen regPriceShk = mean(priceShkComm), by(saq01 year)
mean assetShk agShkComm hazardShk hazardShkComm healthShk healthShkComm priceShk priceShkComm if year == 2012, over(saq01)








preserve 
keep if year == 2012

* Calculate statistics for shocks using weights
svy: mean assetShk hazardShk healthShk priceShk rptShock, over(saq01)
matrix stat = r(table)

svy: mean assetShk hazardShk healthShk priceShk rptShock, over(rural)
matrix statR = r(table)

svy:mean assetShk hazardShk healthShk priceShk rptShock
matrix statEth = r(table)

mean assetShk hazardShk healthShk priceShk rptShock, over(ftfzone)
matrix statFtf  = r(table)
mat Shk = stat, statR, statEth, statFtf
matrix list Shk

mat2txt, matrix(Shk) saving("$pathexport/shock_stats2012") replace
restore


preserve 
svyset ea_id [pweight=pw2], strata(saq01) singleunit(centered) 

* Calculate statistics for shocks using weights
svy: mean assetShk hazardShk healthShk priceShk rptShock, over(region)
matrix stat = r(table)

svy: mean assetShk hazardShk healthShk priceShk rptShock, over(rural)
matrix statR = r(table)

svy:mean assetShk hazardShk healthShk priceShk rptShock
matrix statEth = r(table)

mean assetShk hazardShk healthShk priceShk rptShock, over(ftfzone)
matrix statFtf  = r(table)
mat Shk = stat, statR, statEth, statFtf
matrix list Shk

mat2txt, matrix(Shk) saving("$pathexport/shock_stats2014") replace
restore


* What's the correlation betweenhousehold shocks and community shocks?

egen priceShk_pct = mean(priceShk), by(ea_id)
table region priceShkComm , c(mean priceShk_pct)
table ea_id priceShkComm, c(mean priceShk_pct)

* Create quintile for different wealth indices & check how key variables fall along distribution
* (Plot results in R for prettifying)
g byte landOwn = (numParcels !=0 & numParcels!=.)

#delimit ; 
local wealthVars ax bed bike blanket car cart clothing dungFuel dvd
		elecLight fireLight flushToilet indoorKitchen jewel metalRoof
		mitad mobile moto mudFloor mudHome noKitchen noToilet
		ownHouse phone plough protWaterDry protWaterRainy pump radio 
		refrig sat sew shelf sickle sofa stoneHome stove thatchRoof
		tv watch weave well wasteFert wasteThrow;
 #delimit cr 

* First, try creating global wealth index over all households in just 2012
/* NOTE: including the 30 or so households that score high has a strong effect
on the predicted pca; Also, weighting also seems to have a strong effect; */

factor `wealthVars' [aweight = pw] if year == 2012 , pcf means
predict wealthIndex2012 if year == 2012
*replace wealthIndex2012 = . if wealthIndex2012>5
g byte pcaFilter = (wealthIndex2012 >6)

factor `wealthVars' [aweight = pw2] if year == 2014 , pcf means
predict wealthIndex2014 if year == 2014
replace wealthIndex2014 = . if wealthIndex2014>5
*g byte pcaFilter = (wealthIndex2014 >6)
winsor2 wealthIndex2012 wealthIndex2014, replace cuts(0 99)

histogram wealthIndex2014, by(region)
histogram wealthIndex2012, by(region)

g wealthIndex = wealthIndex2012
replace wealthIndex = wealthIndex2014 if year == 2014
* Use a 30-tile grouping to show how shocks vary by wealth holdings; The wealth index 	
* is stack
xtile wealthSmooth2012 = wealthIndex2012 [pweight = pw] if year == 2012, nq(30)
xtile wealthSmooth2014 = wealthIndex2014 [pweight = pw2] if year == 2014, nq(30)

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

twoway (lowess tv xvar) (lowess mobile xvar) /*
*/(lowess noToilet xvar) (lowess protWaterDry xvar) /*
*/(lowess moto xvar)(lowess electricity xvar) if region != 14, by(region)

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
