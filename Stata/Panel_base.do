/*-------------------------------------------------------------------------------
# Name:		Panel_base
# Purpose:	Create household base file to which other files can be merged
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/22
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#---------------------------------------------------------------------------------*/

clear
capture log close

use "$wave1/sect_cover_hh_w1.dta"
keep household_id-saq08 hh_saq09 hh_saq13_a hh_saq13_b hh_saq13_c

* Check that id is unique identifier
isid household_id
g year = 2012
save "$pathout/base1.dta", replace

use "$wave2/sect_cover_hh_w2.dta", clear
keep household_id-hh_saq09 hh_saq13_a hh_saq13_b hh_saq13_c hh_saq12_a
g year = 2014

* Append two datesets together for merging later on
append using "$pathout\base1.dta", generate(append_base)

* Create a unique id for households that are in panel
bys household_id: gen ptrack = _N
sum ptrack, d 
la var ptrack "Status of household aross waves"

replace ptrack = 3 if ptrack == `r(max)'
la def pcount 1 "Only in 1st wave" 2 "Both waves" 3 "Only in 2nd wave"
la val ptrack pcount

* Create regions at which survey is representative, relabel code 20
clonevar region = saq01
recode region (2 5 6 12 13 15 = 20)
labmm SAQ01 20 "Other regions"
la val region SAQ01

* Fill in id_2 for households in first wave
replace household_id = household_id2 if household_id == ""
bys household_id (year): replace household_id2 = household_id2[2] if household_id2 =="" & ptrack == 2
replace household_id2 = household_id2
replace household_id2 = household_id if household_id2 == "" 

isid household_id year

save "$pathout/hh_base.dta", replace

