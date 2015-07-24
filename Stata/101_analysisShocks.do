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









* SNPP and Oromia have the most shocks
global demog "agehead ageheadsq femhead marriedHoh literateHoh educAdult gendMix depRatio mlabor flabor"
global ltassets "TLUtotal infraindex_rur durWealthindex_rur"
global land 
global geog "dist_road dist_popcenter dist_market dist_borderpost"

eststo p2012, title("Price shock 2012"):logit priceShk $demog  $ltassets $geog ib(7).region ib(1).weeklyMkt if year ==2012, robust or
eststo p2014, title("Price shock 2014"):logit priceShk $demog  $ltassets $geog ib(7).region ib(0).weeklyMkt if year ==2014, robust or
