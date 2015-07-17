/*-------------------------------------------------------------------------------
# Name:		10_assets
# Purpose:	Process household assets and create wealth index
# Author:	Tim Essam, Ph.D.
# Created:	2015/07/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close 
log using "$pathlog/10_assets.txt", replace
di in yellow "`c(current_date)' `c(current_time)'"

* Load module on food aggregates for dietary diversity score
use "$wave1/sect10_hh_w1.dta", clear
set more off

* Look at assets and codes (35 codes possible)
tab hh_s10q00, mi

g byte stove = inlist(hh_s10q00, 1, 2, 3) == 1 & hh_s10q01 >0
g byte mitad = inlist(hh_s10q00, 20, 21) == 1 & hh_s10q01 >0
g byte ax = inlist(hh_s10q00, 31, 32) == 1 & hh_s10q01 >0
g byte plough = inlist(hh_s10q00, 33, 34) == 1 & hh_s10q01 >0
g byte cart = inlist(hh_s10q00, 16, 17) == 1 & hh_s10q01 >0

foreach x of varlist stove mitad ax plough cart {
		la var `x' "household owns at least 1 `x'"
}

#delimit ;
local vlist "blanket bed watch phone mobile radio tv dvd sat sofa 
		bike moto sew weave refrig car jewel clothing shelf biostove
		well mofera sickle pump"; 
#delimit cr
local nlist 4 5 6 7 8 9 10 11 12 13 14 15 18 19 22 23 24 25 26 27 28 29 30 35
local n: word count `vlist'

forvalues i = 1/`n' {
		# First grab words and corresponding value and store in a and b
		local a: word `i' of `vlist'
		local b: word `i' of `nlist'
		g byte `a' = inlist(hh_s10q00, `b') == 1 & 	hh_s10q01  >0
		di "`a' asset mapped to item code `b'"
		la var `a' "household owns at least 1 `a'"
}

* Collapse to hh levels, merge in housing info to create wealth index per 
ds(hh_s* saq* ea_id), not
keep `r(varlist)'

include "$pathdo/copylabels.do"
ds(household_id), not
	collapse (max) `r(varlist)', by(household_id) fast
* Reapply variable lables & value labels
include "$pathdo/attachlabels.do"

merge 1:1 household_id using "$pathout/housing_2012.dta", gen(_merge_tmp)

/* Create a wealth index based on ownership of similar assets:
	1) Television
	2) Refrigerator
	3) landline telephone
	4) cell 
	5) vehicle
	6) wash machine
	7) microwave
	8) indoor plumbing 
	) indoor bathroom
	10) computer
*/

* Looking for decent variation in inputs to the wealth index
sum stove-pump

factor ax plough sickle pump cart well  if rural == 1, pcf

local wealthVars  tv refrig phone mobile car radio shelf jewel bed mofera watch  
factor `wealthVars' if rural == 1, pcf
rotate, oblique promax(2)
predict durWealthindex_rur if rural == 1
alpha `wealthVars' if rural == 1
loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)


local wealthVars  tv refrig phone mobile car radio shelf jewel bed mofera watch 
factor `wealthVars' if rural != 1, pcf
rotate, oblique promax(2)
predict durWealthindex_urb if rural != 1
alpha `wealthVars' if rural != 1
loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

compress
sa "$pathout/assets_2012.dta", replace

* -----------------------------------------------------------------------------------*

****************************
* --- Process 2014 data ---*
****************************
* Load module on food aggregates for dietary diversity score
use "$wave2/sect10_hh_w2.dta", clear
set more off

* Look at assets and codes (35 codes possible)
tab hh_s10q00, mi nol

g byte stove = inlist(hh_s10q00, 1, 2, 3) == 1 & hh_s10q01 >0
g byte mitad = inlist(hh_s10q00, 20, 21) == 1 & hh_s10q01 >0
g byte ax = inlist(hh_s10q00, 31, 32) == 1 & hh_s10q01 >0
g byte plough = inlist(hh_s10q00, 33, 34) == 1 & hh_s10q01 >0
g byte cart = inlist(hh_s10q00, 16, 17) == 1 & hh_s10q01 >0

foreach x of varlist stove mitad ax plough cart {
		la var `x' "household owns at least 1 `x'"
}

#delimit ;
local vlist "blanket bed watch phone mobile radio tv dvd sat sofa 
		bike moto sew weave refrig car gold clothing shelf biostove
		well sickle pump silver"; 
#delimit cr
local nlist 4 5 6 7 8 9 10 11 12 13 14 15 18 19 22 23 24 25 26 27 28 30 35 36
local n: word count `vlist'

forvalues i = 1/`n' {
		# First grab words and corresponding value and store in a and b
		local a: word `i' of `vlist'
		local b: word `i' of `nlist'
		g byte `a' = inlist(hh_s10q00, `b') == 1 & 	hh_s10q01  >0
		di "`a' asset mapped to item code `b'"
		la var `a' "household owns at least 1 `a'"
}

g byte jewel = (gold == 1 | silver ==1)
la var jewel "household owns gold or silver"

* Collapse to hh levels, merge in housing info to create wealth index per 
ds(hh_s* saq* ea_id2 ea_id household_id), not
keep `r(varlist)'

include "$pathdo/copylabels.do"
ds(household_id2), not
	collapse (max) `r(varlist)', by(household_id2) fast
* Reapply variable lables & value labels
include "$pathdo/attachlabels.do"

ds(household_id2 rural pw2), not
sum `r(varlist)'

merge 1:1 household_id2 using "$pathout/housing_2014.dta", gen(_merge_tmp)

/* Create a wealth index based on ownership of similar assets:
	1) Television
	2) Refrigerator
	3) landline telephone
	4) cell 
	5) vehicle
	6) wash machine
	7) microwave
	8) indoor plumbing 
	) indoor bathroom
	10) computer
*/

local wealthVars  tv refrig phone mobile car radio shelf jewel bed watch dvd
factor `wealthVars' if rural == 1, pcf
rotate, oblique promax(2)
predict durWealthindex_rur if rural == 1
alpha `wealthVars' if rural == 1
loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)


local wealthVars  tv refrig phone mobile car radio shelf jewel bed watch dvd
factor `wealthVars' if rural != 1, pcf
rotate, oblique promax(2)
predict durWealthindex_urb if rural != 1
alpha `wealthVars' if rural != 1
loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

* Why rotate? http://pareonline.net/getvn.asp?v=20&n=2
* Shouldn't matter that much, but helps with interpreation.

compress
sa "$pathout/assets_2014.dta", replace

* Append the two years of data using custom function
pappend assets_2012 assets_2014 assets_all

* Check the resulting indices and how they compare by year
* What's up with the outliers?
histogram wealthindex_rur, by(year)
