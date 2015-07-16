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
keep household_id-saq08 saq01 hh_saq09 hh_saq13_a hh_saq13_b hh_saq13_c rural ea_id

* Check that id is unique identifier
isid household_id
g year = 2012
save "$pathout/base1.dta", replace

use "$wave2/sect_cover_hh_w2.dta", clear
keep household_id* saq01 hh_saq09 hh_saq13_a hh_saq13_b hh_saq13_c hh_saq12_a rural ea_id2 ea_id
g year = 2014

* Append two datesets together for merging later on
append using "$pathout\base1.dta", generate(append_base)

* Fix ea_id's so they will merge later on with comm variables
replace ea_id = ea_id2 if ea_id == ""
replace ea_id2 = ea_id if ea_id2 == ""

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
replace household_id2 = household_id if household_id2 == "" 

isid household_id year

save "$pathout/hh_base.dta", replace

* Repeat exercise for communities (https://www.youtube.com/watch?v=rH48caFgZcI - to get through more of this)

use "$wave1/sect1b_com_w1.dta", clear
keep ea_id rural sa1q01 sa1q02 sa1q03 sa1q04 sa1q05 sa1q06 sa1q07 
g year = 2012
save "$pathout/base_comm1.dta", replace

use "$wave2/sect1b_com_w2.dta", clear

/* NOTE: Two ea_id's are not unique and have to be modified
before the ea's can be merged/appended. Not sure why they are not
unique.
https://www.youtube.com/watch?v=rH48caFgZcI
*/
replace ea_id = "" if ea_id2 == "010501088800105"
replace ea_id = "" if ea_id2 == "130101088800303"
keep ea_id* sa1q01 sa1q02 sa1q03 sa1q04 sa1q05 sa1q06 sa1q07
g year = 2014

* Append two datesets together for merging later on
append using "$pathout\base_comm1.dta", generate(append_base)

* Create a unique id for communities that are in panel
bys ea_id: gen ptrackComm = _N
sum ptrackComm, d 
la var ptrackComm "Status of household aross waves"

replace ptrackComm = 3 if ptrack == `r(max)'
la def pcount 1 "Only in 1st wave" 2 "Both waves" 3 "Only in 2nd wave"
la val ptrackComm pcount

* Fill in id_2 for households in first wave
replace ea_id = ea_id2 if ea_id == ""
bys ea_id (year): replace ea_id2 = ea_id2[2] if ea_id2 =="" & ptrack == 2
replace ea_id2 = ea_id if ea_id2 == "" 

* Check that both ea_id variables + year give a unique id for the eas 
isid ea_id year
isid ea_id2 year

save "$pathout/comm_base.dta", replace
