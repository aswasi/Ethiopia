/*-------------------------------------------------------------------------------
# Name:		04a_health_merge
# Purpose:	Merge individual level health data with education data
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/17
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close 
log using "$pathlog/health_merge_Ind.log", replace
set more off 

* Import and convert excel files to temp files for merging
global pathgit "C:/Users/t/Documents/GitHub/Ethiopia/data/"
cd $pathgit

tempfile ed12 ed14 ind12 ind14
use education2012.dta, clear
clonevar individual_id = indivID2012
g year = 2012
*save "`ed12'"
sa "$pathout/education2012.dta", replace

use indiv2012.dta, clear
clonevar individual_id = indivID2012
g year = 2012
*save "`'ind12'"
merge 1:1 individual_id using "$pathout/education2012.dta", gen(merge_tmp)
drop merge_tmp
sa "$pathout/indiv2012.dta", replace

use education2014_noID14.dta, clear
clonevar individual_id = indivID2012
g year = 2014
append using "$pathout/indiv2012.dta" 
sa "$pathout/cHealth_merge.dta", replace

use "$pathgit/ETH_201506_cHealth.dta"
drop if individual_id == ""
merge 1:1  individual_id year using "$pathout/cHealth_merge.dta", gen(chealth_mg)

keep if chealth_mg == 3
drop chealth_mg
ren ptrack ptrackChild
sa "$pathout/cHealth_all.dta", replace

* Merge in full dataset at household level
use "$pathout/ETH_201507_LSMS_Analysis.dta", clear
merge 1:m household_id year using "$pathout/cHealth_all.dta", gen(ind_to_hh) force
keep if ind_to_hh == 3

* Generate age categories by year
g ageCat = 0 if ageMonths>6 & ageMonths <=11 & year == 2012
replace ageCat = 1 if ageMonths>11 & ageMonths <=23 & year == 2012 
replace ageCat = 2 if ageMonths>23 & ageMonths <=59 & year == 2012 
replace ageCat = 3 if ageMonths>6 & ageMonths <=11 & year == 2014
replace ageCat = 4 if ageMonths>11 & ageMonths <=23 & year == 2014 
replace ageCat = 5 if ageMonths>23 & ageMonths <=59 & year == 2014

la def agecat 0 "6 - 11 mo. 2012" 1 "12 - 23 mo. 2012" 2 "24 - 59 mo. 2012" /*
*/ 3 "6 - 11 mo. 2014" 4 "12 - 23 mo. 2014" 5 "24 - 59 mo. 2014"
la val ageCat agecat
la var ageCat "Age categories for stunting analysis"

sa "$pathout/ETH_201508_Child_Analysis.dta", replace
