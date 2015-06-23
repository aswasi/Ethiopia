/*-------------------------------------------------------------------------------
# Name:		06_timeuse
# Purpose:	Process household health and child nutrition information
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/17
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathlog/06_timeuse", replace
di in yellow "`c(current_date)' `c(current_time)'"

* Load time use and labour module to document household time allocation
use "$wave1/sect4_hh_w1.dta"
g year = 2012

* Household time allocation for water, firewood, and ag activities
* Extract last digit from individual_id to distinguish hoh from spouse
g hhpos = substr(individual_id,-1,.)
destring hhpos, replace

* Irregular values that are above 8 hours -- assuming these are really minutes and converting to appropriate unit
replace hh_s4q02 = (hh_s4q02/60) if hh_s4q02>8
egen totWaterTime = total(hh_s4q02), by(household_id)
g spouseWaterTime = (hh_s4q02) if hhpos == 2 
g headWaterTime = (hh_s4q02) if hhpos == 1

* Same problem as variable above
replace hh_s4q03 = (hh_s4q03/60) if hh_s4q03>8
egen totFirewdTime = total(hh_s4q03), by(household_id)
g spouseFireTime = hh_s4q03 if hhpos ==2
g headFireTime	= hh_s4q03 if hhpos == 1

* Total household time spend on agricultural activities
egen totAgTime = total(hh_s4q04), by(household_id)
egen totNonagTime = total(hh_s4q05), by(household_id)
egen workedFree = max(hh_s4q37), by(household_id)
egen workdHH = max(hh_s4q38), by(household_id)

la var totWaterTime "Total household time spent on water collection (min)"
la var spouseWaterTime "Total spouse time spent on water collection (min)"
la var headWaterTime "Total time hh head spent on water collection (min)"
la var totFirewdTime "Total household time spent on firewood collection (min)"
la var spouseFireTime "Total spouse time spent on firewood collection (min)"
la var headFireTime "Total hoh time spent on firewood collection (min)"
la var totAgTime "Total time spent on ag activities in past week"
la var totNonagTime "Total time spent on non-ag activities in past week"
la var workedFree "Household worked for other households free of charge"
la var workdHH "Total number of households for which hh worked free of charge"

* Retain newly created varaibles, discard remaining and collapse down to hh level
ds(hh_s* saq*), not 
keep `r(varlist)'

qui include "$pathdo/copylabels.do"
#delimit ;
	collapse (max) totWaterTime spouseWaterTime headWaterTime 
		totFirewdTime spouseFireTime headFireTime totAgTime 
		totNonagTime workedFree workdHH year, by(household_id);
#delimit cr
qui include "$pathdo/attachlabels.do"

local vlist totWaterTime spouseWaterTime headWaterTime totFirewdTime spouseFireTime headFireTime
foreach x of local vlist {
		replace `x' = `x' * 60
}

compress
sa "$pathout/timeuse_2012.dta", replace

* --- Repeat process for 2014 data
clear
* Load time use and labour module to document household time allocation
use "$wave2/sect4_hh_w2.dta"
g year = 2014

* Extract last digit from individual_id to distinguish hoh from spouse
g hhpos = substr(individual_id2,-1,.)
destring hhpos, replace

g wtTemp = (hh_s4q02_a*60) + (hh_s4q02_b)
g fwTemp = (hh_s4q03_a*60) + hh_s4q03_b

egen totWaterTime = total(wtTemp), by(household_id2)
g spouseWaterTime = (wtTemp) if hhpos == 2 
g headWaterTime = (wtTemp) if hhpos == 1

egen totFirewdTime = total(fwTemp), by(household_id2)
g spouseFireTime = fwTemp if hhpos ==2
g headFireTime	= fwTemp if hhpos == 1

* Total household time spend on agricultural activities
egen totAgTime = total(hh_s4q04), by(household_id2)
egen totNonagTime = total(hh_s4q05), by(household_id2)
egen workedFree = max(hh_s4q37), by(household_id2)
egen workdHH = max(hh_s4q38), by(household_id2)

la var totWaterTime "Total household time spent on water collection (min)"
la var spouseWaterTime "Total spouse time spent on water collection (min)"
la var headWaterTime "Total time hh head spent on water collection (min)"
la var totFirewdTime "Total household time spent on firewood collection (min)"
la var spouseFireTime "Total spouse time spent on firewood collection (min)"
la var headFireTime "Total hoh time spent on firewood collection (min)"
la var totAgTime "Total time spent on ag activities in past week"
la var totNonagTime "Total time spent on non-ag activities in past week"
la var workedFree "Household worked for other households free of charge"
la var workdHH "Total number of households for which hh worked free of charge"

* Retain newly created varaibles, discard remaining and collapse down to hh level
ds(hh_s* saq*), not 
keep `r(varlist)'

qui include "$pathdo/copylabels.do"
#delimit ;
	collapse (max) totWaterTime spouseWaterTime headWaterTime 
		totFirewdTime spouseFireTime headFireTime totAgTime 
		totNonagTime workedFree workdHH year pw2, by(household_id2);
#delimit cr
qui include "$pathdo/attachlabels.do"

compress
sa "$pathout/timeuse_2014.dta", replace

* Call the panel append function to append datasets together
pappend timeuse_2012 timeuse_2014 timeuse_all

capture log close
