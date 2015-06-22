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
keep household_id-saq08 hh_saq09

* Check that id is unique identifier
isid household_id
g year = 2012
save "$pathout/base1.dta", replace

use "$wave2/sect_cover_hh_w2.dta", clear
keep household_id-hh_saq09
g year = 2014

* Append two datesets together for merging later on
append using "C:\Users\Tim\Documents\Ethiopia\Dataout\base1.dta", generate(append_base)

* Create a unique id for households that are in panel
bys household_id: gen ptrack = _N
sum ptrack, d 
replace ptrack = 1 if ptrack == `r(max)'

* Fill in id_2 for households in first wave
clonevar hid = household_id2
bys household_id (year): replace hid = hid[2] if hid =="" & ptrack == 2
replace hid = household_id if hid == ""
isid hid year

save "$pathout/hh_base.dta", replace

