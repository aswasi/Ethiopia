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
log using "$pathlog/health_merge_Ind", replace
set more off 

* Import and convert excel files to temp files for merging
 cd $pathgit\data
use indiv_education.dta, clear

import delimited "$pathgit\data\indiv_education.csv", clear





use "$pathout/ETH_201506_cHealth.dta"



indiv_education.csv
should be indiv + education merged
there's also the edu2012, 2014 and indiv2012, 2014 files
