clear
capture log close
log using "$pathout/101_analysisShocks.do", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off

* May have to change below depending on what happends in 100
use $pathout/ETH_201507_LSMS_ALL.dta, clear

* Running summary stats on shocks using sampling weights for each year
forvalues i=2012(2)2014 {
	preserve
		keep if year == `i'
		if year == 2012 {
			svyset ea_id [pweight=pw], strata(saq01) singleunit(centered) 
			svydescribe
			svy:mean assetShk crimeShk employShk hazardShk healthShk priceShk, over(region)
			}
		else {
			svyset ea_id2 [pweight=pw2], strata(saq01) singleunit(centered) 
			svydescribe
			svy:mean assetShk crimeShk employShk hazardShk healthShk priceShk, over(region)
		} 
		*set tr on
		restore
}



* What's the correlation betweenhousehold shocks and community shocks?

egen priceShk_pct = mean(priceShk), by(ea_id)
table region priceShkComm , c(mean priceShk_pct)
table ea_id priceShkComm, c(mean priceShk_pct)

* Create quintile for different wealth indices & check how key variables fall along distribution
* (Plot results in R for prettifying)
g byte landOwn = (areaField >0 & areaField!=.)

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

factor `wealthVars' [aweight = pw] if year == 2012 , pcf
predict wealthIndex2012 if year == 2012
replace wealthIndex2012 = . if wealthIndex2012>5
g byte pcaFilter = (wealthIndex2012 >6)
histogram wealthIndex2012, by(region)

set tr on
factor `wealthVars' [aweight = pw2] if year == 2014 , pcf
predict wealthIndex2014 if year == 2014
replace wealthIndex2014 = . if wealthIndex2014>5
*g byte pcaFilter = (wealthIndex2014 >6)
histogram wealthIndex2014, by(region)

* Generate new weight to account for household houseSize
g hhweight = pw*hhsize
xtile wealthQuint2012=wealthIndex2012 [pweight=hhweight] if year == 2012, nq(5)

twoway (lowess tv wealthIndex2012) (lowess mobile wealthIndex2012) /*
*/(lowess noToilet wealthIndex2012) (lowess protWaterDry wealthIndex2012), by(region)

twoway (lowess tv wealthIndex2012) (lowess mobile wealthIndex2012) /*
*/(lowess noToilet wealthIndex2012) (lowess protWaterDry wealthIndex2012) /*
*/(lowess moto wealthIndex2012), by(region)

twoway(lowess FCS wealthIndex2014), by(region ftfzone)
twoway(lowess dietDiv wealthIndex2014), by(region ftfzone)
twoway(lowess hfiasindex_rur wealthIndex2014), by(region ftfzone)

twoway (lowess hazardShk wealthIndex2014) (lowess priceShk wealthIndex2014) /*
*/(lowess healthShk wealthIndex2014) (lowess assetShk wealthIndex2014), by( region)

twoway (lowess dietDiv wealthIndex2012 if ftfzone!=1) (lowess dietDiv wealthIndex2012 if ftfzone == 1), by(region)
twoway (lowess FCS wealthIndex2012 if ftfzone!=1) (lowess FCS wealthIndex2012 if ftfzone == 1) /*
*/ (lowess FCS wealthIndex2014 if ftfzone != 1) (lowess FCS wealthIndex2014 if ftfzone == 1), by(region)

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





* SNPP and Oromia have the most shocks
global demog "agehead ageheadsq femhead marriedHoh literateHoh educAdult gendMix depRatio mlabor flabor"
global ltassets "TLUtotal infraindex_rur durWealthindex_rur"
global land 
global geog "dist_road dist_popcenter dist_market dist_borderpost"

eststo p2012, title("Price shock 2012"):logit priceShk $demog  $ltassets $geog ib(7).region ib(1).weeklyMkt if year ==2012, robust or
eststo p2014, title("Price shock 2014"):logit priceShk $demog  $ltassets $geog ib(7).region ib(0).weeklyMkt if year ==2014, robust or
