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
 cd $pathgit/data

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




use education2014.dta, clear
clonevar individual_id2 = indivID2014
clonevar individual_id = indivID2012

g 

merge 1:1 individual_id using "$pathout/education2012_str.dta", gen(merge_ind)





use "$pathout/ETH_201506_cHealth.dta"



indiv_education.csv
should be indiv + education merged
there's also the edu2012, 2014 and indiv2012, 2014 files
