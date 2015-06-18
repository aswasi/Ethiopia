/*-------------------------------------------------------------------------------
# Name:		04_health_2012
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
log using "$pathlog/04_health_2012", replace
di in yellow "`c(current_date)' `c(current_time)'"

* Merge in gender information
use "$wave1/sect1_hh_w1.dta", clear
merge 1:1 household_id individual_id using "$wave1/sect3_hh_w1.dta"

clonevar hid = household_id
clonevar pid = individual_id

* Household illness in past 2 months
g byte illness = (hh_s3q01 == 1)
egen totIllness = total(illness), by(hid)
la var totIllness "Total hh members ill in last two months"
clonevar sickdays = hh_s3q03

* Sick hh member sought treatment? What type?
g byte sickTreat = (hh_s3q04 == 1)
clonevar treatType = hh_s3q05
clonevar illnessType =  hh_s3q02

* At least one hh member had malaria
g byte malariaTmp = (illnessType == 1)
egen malariaHH = max(malariaTmp), by(hid)

* At least one hh member had diarrhea
g byte diarrheaTmp = (illnessType == 2)
egen diarrheaHH = max(diarrheaTmp), by(hid)

la var illness "illness in last 60 days"
la var sickTreat "hh member sought treatment for illness"
la var malariaHH "At least one hh member had malaria"
la var diarrheaHH "At least one hh member had diarrhea"

drop *Tmp

g byte childTag = (hh_s3q20==1)

* Generate child height var assuming 24 month cutoff used correctly
clonevar cheight = hh_s3q23
clonevar cweight = hh_s3q22
g ageMonths = hh_s1q04_b if childTag == 1
la var ageMonths "Age of child in months"
clonevar gender = hh_s1q03
*
* Calculate z-scores using zscore06 package
* a - ageMonths; s - gender, h - hieght, w - weight, o - oedema
zscore06, a(ageMonths) s(gender) h(cheight) w(cweight)

* Remove scores that are implausible
replace haz06=. if haz06<-6 | haz06>6
replace waz06=. if waz06<-6 | waz06>5
replace whz06=. if whz06<-5 | whz06>5
replace bmiz06=. if bmiz06<-5 | bmiz06>5

* Rename the variables to be more meaningful
ren haz06 stunting
ren waz06 underweight
ren whz06 wasting
ren bmiz06 BMI

la var stunting "Stunting: Length/height-for-age Z-score"
la var underweight "Underweight: Weight-for-age Z-score"
la var wasting "Wasting: Weight-for-length/height Z-score"

g byte stunted = stunting < -2 if stunting != .
g byte underwgt = underweight < -2 if underweight != . 
g byte wasted = wasting < -2 if wasting != . 
g byte BMIed = BMI <-2 if BMI ~= . 
la var stunted "Child is stunting"
la var underwgt "Child is underweight for age"
la var wasted "Child is wasting"

sum stunted underwgt wasted 

* Look at the outcomes by age category
twoway (lowess stunted ageMonths, mean adjust bwidth(0.75)) /*
*/ (lowess wasted ageMonths, mean adjust bwidth(0.75)) /*
*/ (lowess underwgt ageMonths, mean adjust bwidth(0.75)),  /*
*/ xlabel(0(6)60,  labsize(small)) title("Child Nutrition Outcomes: 2009 (unweighted)")

* Save child health information
preserve
ds(hh_s*)
keep `r(varlist)'
sa $pathout/childHealth_I_2012.dta", replace
restore

