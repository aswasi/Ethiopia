/*-------------------------------------------------------------------------------
# Name:		02_hhchar_2012
# Purpose:	Process household characteristics and education
# Author:	Tim Essam, Ph.D.
# Created:	10/31/2014; 02/19/2015.
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathlog/02_hhchar_2012", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off

use "$wave1/sect1_hh_w1.dta"

* Merge in Education information to base to work out of one dataset
merge 1:1 household_id individual_id using "$wave1/sect2_hh_w1.dta"

/* Demographic list to calculate
1. Head of Household Sex
2. Relationship Status
*/
clonevar hid = household_id
clonevar pid = individual_id

g byte hoh = hh_s1q02 == 1
la var hoh "Head of household"

g byte spouse = hh_s1q02 == 2
la var spouse "spouse of hoh"

g byte femhead = hh_s1q02 == 1 & hh_s1q03 == 2
la var femhead "Female head of household"

g ageSpouse = hh_s1q04_a if spouse == 1
la var ageSpouse "Age of the spouse"

g agehead = hh_s1q04_a if hoh == 1
g ageheadsq = agehead^2
la var agehead "Age of hoh"
la var ageheadsq "Squared age of the head (for non-linear life experience)"


* --- Relationship status 
g byte marriedHohm = (hh_s1q08 == 2 & hoh == 1)
g byte marriedHohp = (hh_s1q08 == 3 & hoh == 1)
g byte nonmarriedHoh = (inlist(hh_s1q08, 1, 4, 5, 6)==1) & hoh == 1
g byte widowFemhead = femhead == 1 & hh_s1q08 == 6
g byte nonmarriedFemhead = femhead == 1 & inlist(hh_s1q08, 1, 4, 5)

la var marriedHohm "Monogamous married hoh"
la var marriedHohp "Polygamous married hoh"
la var nonmarriedHoh "Non-married (never marry, divorce, separated, widowed)"
la var widowFemhead "Widowed female hoh"
la var nonmarriedFemhead "Non-married (never marry, divorce, separated, widowed) female hoh"

* --- Religion 
g religHoh = hh_s1q07 if hoh == 1
g religSpouse = hh_s1q07 if hh_s1q02 == 2

tempvar hohrelig sprelig
egen `hohrelig' = max(religHoh), by(hid)
egen `sprelig' = max(religSpouse), by(hid)
g byte mxdreligHH = `hohrelig' != `sprelig' & `hohrelig' != . & `sprelig' != .

la var religHoh "Religion of hoh"
la var religSpouse "Religion of spouse"
la var mxdreligHH "Mixed-religion household"

* --- Calculate household demographics (size, adult equivalent units, dep ratio, etc).

/* Household size - Household size refers to the number of usual members in a 
household. Usual members are defined as those who have lived in the household 
for at least 6 months in the past 12 months. However, it includes persons who 
may have spent less than 6 months during the last 12 months in the household 
but have joined the household with intention to live permanently or for an 
extended period of time.
* definition is from Uganda data, but will apply here for consistency */


TODO*

g byte hhmemb = 

la var hhmemb "Usual member of household"
egen hhsize = total(hhmemb), by(hid)
la var hhsize "household size"




* --- Education 
/* Literacy is defined as the ability to read with understanding and to 
 write meaningfully in any language. */
g byte literateHoh = hh_s2q02 == 1 & hoh == 1
g byte literateSpouse = 

la var literateHoh "Hoh is literate"
la var literateSpouse "Spouse is literate"



