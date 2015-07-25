/*-------------------------------------------------------------------------------
# Name:		05_HFIAS
# Purpose:	Process food security module and create HFIAS metrics
# Author:	Tim Essam, Ph.D.
# Created:	2015/07/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close 
log using "$pathlog/05_HFAIS.txt", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off
* load food security module and create HFIAS based on 
* http://www.fantaproject.org/monitoring-and-evaluation/household-food-insecurity-access-scale-hfias
u "$wave1/sect7_hh_w1.dta"

* Drop any households who did not answer question 1 (57 of them)
drop if hh_s7q01 == .
g byte q1_HFIAS = (hh_s7q01 == 1)

local i = 2
foreach x of varlist hh_s7q02_a hh_s7q02_b hh_s7q02_c hh_s7q02_d hh_s7q02_e hh_s7q02_f hh_s7q02_g hh_s7q02_h {
		g byte q`i'_HFIAS = `x'>0 & `x'!=.
		clonevar q`i'a_HFIAS = `x'
		local i = `++i'
}

g condHFIAS = q7_HFIAS
g byte domainHFIAS = (q2_HFIAS ==1 | q3_HFIAS ==1 | q4_HFIAS ==1)

egen modHFIAS_score = rsum2(q2a_HFIAS q3a_HFIAS q4a_HFIAS q5a_HFIAS q6a_HFIAS q7a_HFIAS q8a_HFIAS q9a_HFIAS)

* Run factor analysis on FS questions to reduce dimensionality (http://www.theanalysisfactor.com/rotations-factor-analysis/)
* Allow for factors to be correlated using oblique rotation
local fsVars q1_HFIAS q2a_HFIAS q3a_HFIAS q4a_HFIAS q5a_HFIAS q6a_HFIAS q7a_HFIAS q8a_HFIAS q9a_HFIAS
local i = 0
local locVar urb rur
forvalues i=0/1 {
	factor `fsVars' if rural == `i', pcf
	rotate, oblique promax(2)
	alpha `fsVars' if rural == `i'

	loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

	if `i' == 0 {
			predict hfiasindex_urb if rural == 0
	}
	else predict hfiasindex_rur if rural == 1
}

factor `fsVars', pcf 
predict hfiasIndex
la var hfiasIndex "HFIAS factor analysis score"

histogram hfiasindex_rur if hfiasindex_rur>-0.5, by(saq01) legend(rows(2))
/* NOTES: a large number of hh answered no to q1 which results in a 
	stacked index variable around -0.60; Food security is sort of a
	two stage process, first you are food secure and secondly you
	have a degree of food insecurity; Should account for this if we 
	decide to model the variable */

g byte foodShortage = (hh_s7q06 == 1)
replace foodShortage = . if  hh_s7q06 == .

local dlist Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
local vlist m a b c d e f g h i j k
local n: word count `vlist'

forvalues i = 1/`n'{
	local a: word `i' of `dlist'
	local b: word `i' of `vlist'
	g byte foodlack_`a' = regexm(hh_s7q07_`b', "X") == 1
	la var foodlack_`a' "Household face food shortage in `a'"
	tab foodlack_`a', mi
}

* Quickly summarize when shortages are occuring
graph bar (sum) foodlack*, by(saq01) legend(rows(2))

egen totMonFoodlack = rsum(foodlack_*)
la var totMonFoodlack "Total months lacking food"

* Clone the cause of food insecurity
clonevar foodinsec_issue1 = hh_s7q08_a
clonevar foodinsec_issue2 = hh_s7q08_b
clonevar foodinsec_issue3 = hh_s7q08_c

* Collapse to hh levels, merge in housing info to create wealth index per 
ds(hh_s* saq* ea_id), not
keep `r(varlist)'

include "$pathdo/copylabels.do"
ds(household_id), not
	collapse (max) `r(varlist)', by(household_id) fast
* Reapply variable lables & value labels
include "$pathdo/attachlabels.do"
g year = 2012
compress

sa "$pathout/hfias_2012.dta", replace
* -----------------------------------------------------------------------------------*

****************************
* --- Process 2014 data ---*
****************************


u "$wave2/sect7_hh_w2.dta", clear

* Drop any households who did not answer question 1 (2 of them)
recode hh_s7q01 (5 0 = .)
drop if hh_s7q01 == .
g byte q1_HFIAS = (hh_s7q01 == 1)

local i = 2
foreach x of varlist hh_s7q02_a hh_s7q02_b hh_s7q02_c hh_s7q02_d hh_s7q02_e hh_s7q02_f hh_s7q02_g hh_s7q02_h {
		g byte q`i'_HFIAS = `x'>0 & `x'!=.
		clonevar q`i'a_HFIAS = `x'
		local i = `++i'
}

g condHFIAS = q7_HFIAS
g byte domainHFIAS = (q2_HFIAS ==1 | q3_HFIAS ==1 | q4_HFIAS ==1)

egen modHFIAS_score = rsum2(q2a_HFIAS q3a_HFIAS q4a_HFIAS q5a_HFIAS q6a_HFIAS q7a_HFIAS q8a_HFIAS q9a_HFIAS)

* Run factor analysis on FS questions to reduce dimensionality (http://www.theanalysisfactor.com/rotations-factor-analysis/)
* Allow for factors to be correlated using oblique rotation
recode rural (2 3 = 0)

local fsVars q1_HFIAS q2a_HFIAS q3a_HFIAS q4a_HFIAS q5a_HFIAS q6a_HFIAS q7a_HFIAS q8a_HFIAS q9a_HFIAS
local i = 0
local locVar urb rur
forvalues i=0/1 {
	factor `fsVars' if rural == `i', pcf
	rotate, oblique promax(2)
	alpha `fsVars' if rural == `i'

	loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

	if `i' == 0 {
			predict hfiasindex_urb if rural == 0
	}
	else predict hfiasindex_rur if rural == 1
}

/* TODO: Calculate different scale for Addis as it's scores are waaay different'

factor `fsVars', pcf
alpha `fsVars'
predict hfiasIndex
la var hfiasIndex "HFIAS factor analysis score"


histogram hfiasindex_rur if hfiasindex_rur>-0.4, by(saq01) legend(rows(2))
/* NOTES: a large number of hh answered no to q1 which results in a 
	stacked index variable around -0.40; Food security is sort of a
	two stage process, first you are food secure and secondly you
	have a degree of food insecurity; Should account for this if we 
	decide to model the variable */


g byte foodShortage = (hh_s7q06 == 1)
replace foodShortage = . if  hh_s7q06 == .

local dlist Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
local vlist a b c d e f g h i j k l
local n: word count `vlist'

forvalues i = 1/`n'{
	local a: word `i' of `dlist'
	local b: word `i' of `vlist'
	g byte foodlack_`a' = regexm(hh_s7q07_`b', "X") == 1
	la var foodlack_`a' "Household face food shortage in `a'"
	tab foodlack_`a', mi
}

egen totMonFoodlack = rsum(foodlack_*)
la var totMonFoodlack "Total months lacking food"

* Quickly summarize when shortages are occuring
graph bar (sum) foodlack*, by(saq01) 

* Clone the cause of food insecurity
clonevar foodinsec_issue1 = hh_s7q08_a
clonevar foodinsec_issue2 = hh_s7q08_b
clonevar foodinsec_issue3 = hh_s7q08_c

tab foodinsec_issue1 saq01

* Collapse to hh levels, merge in housing info to create wealth index per 
ds(hh_s* saq* ea_id2 ea_id household_id), not
keep `r(varlist)'

include "$pathdo/copylabels.do"
ds(household_id2), not
	collapse (max) `r(varlist)', by(household_id2) fast
* Reapply variable lables & value labels
include "$pathdo/attachlabels.do"

g year = 2014
compress

sa "$pathout/hfias_2014.dta", replace
pappend hfias_2012 hfias_2014 hfias_all
