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

prop stunted underwgt wasted 

* Look at the outcomes by age category
qui twoway (lowess stunted ageMonths, mean adjust bwidth(0.75)) /*
*/ (lowess wasted ageMonths, mean adjust bwidth(0.75)) /*
*/ (lowess underwgt ageMonths, mean adjust bwidth(0.75)), /*
*/ xlabel(0(6)60,  labsize(small)) title("Child Nutrition Outcomes: 2012 (unweighted)")

* Save child health information
preserve
ds(hh_s*), not 
keep `r(varlist)'
g year = 2012
sa "$pathout/childHealth_I_2012.dta", replace
restore

* Save household information for appending/merging with 2014 diarrheaTmp
g year = 2012
ds(hh_s*  _merge age*), not
keep `r(varlist)'

qui include "$pathdo/copylabels.do"
#delimit ;
	collapse (max) illness totIllness malariaHH diarrheaHH year
			 (mean) stunting underweight wasting BMI saq*
			 (sum) childTag, by(household_id);
#delimit cr
qui include "$pathdo/attachlabels.do"

compress
sa "$pathout/health_2012.dta", replace

* --------------------------------------------------------------- *
********************
* 2014 Health data *
********************

* Merge in gender information
use "$wave2/sect1_hh_w2.dta", clear
merge 1:1 household_id2 individual_id2 using "$wave2/sect3_hh_w2.dta"

clonevar hid2 = household_id2
clonevar pid2 = individual_id2

* Household illness in past 2 months
g byte illness = (hh_s3q01 == 1)
egen totIllness = total(illness), by(hid2)
la var totIllness "Total hh members ill in last two months"
clonevar sickdays = hh_s3q03

* Sick hh member sought treatment? What type?
g byte sickTreat = (hh_s3q04 == 1)
clonevar treatType = hh_s3q05
clonevar illnessType =  hh_s3q02_a

* At least one hh member had malaria
g byte malariaTmp = (illnessType == 1)
egen malariaHH = max(malariaTmp), by(hid2)

* At least one hh member had diarrhea
g byte diarrheaTmp = (illnessType == 2)
egen diarrheaHH = max(diarrheaTmp), by(hid2)

* At least one hh member had respiratory infection
g byte respInfectionTmp = (hh_s3q02_a_other == "UPPER RESPIRATORY TRACT INFECTION")
egen respInfection = max(respInfectionTmp), by(hid2)

la var illness "illness in last 60 days"
la var sickTreat "hh member sought treatment for illness"
la var malariaHH "At least one hh member had malaria"
la var diarrheaHH "At least one hh member had diarrhea"
la var respInfection "At least one hh member had respiratory infection"

drop *Tmp

* Add in information for child diarrhea & breastfeeding
g byte childTag = (hh_s3q12_a==1)
g byte childTag7 = (hh_s3q20 == 1)

* Did child under 7 have diarrhea in last two weeks?
g byte chDiarrhea = hh_s3q20a == 1

* Generate child height var assuming 24 month cutoff used correctly
clonevar cweight = hh_s3q22
clonevar cheight = hh_s3q23
g ageYrs = hh_s1q04_a if hh_s1q04_a <=5 & childTag == 1
clonevar ageMonthRange = hh_s1q04_b
g ageMonths = (ageYrs * 12) + hh_s1q04_b
replace ageMonths =. if ageMonths>60
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

prop stunted underwgt wasted if rural == 1

* Look at the outcomes by age category
qui twoway (lowess stunted ageMonths, mean adjust bwidth(0.75)) /*
*/ (lowess wasted ageMonths, mean adjust bwidth(0.75)) /*
*/ (lowess underwgt ageMonths, mean adjust bwidth(0.75)), /*
*/ xlabel(0(6)60,  labsize(small)) title("Child Nutrition Outcomes: 2014 (unweighted)")

* Save child health information
preserve
ds(hh_s*), not
keep `r(varlist)'
g year = 2014
sa "$pathout/childHealth_I_2014.dta", replace
restore

* Merge with 2012 data to create panel
/* NOTE: There are only 98 children under 5 in Addis Ababa - how can this be representative? 
Page 27 of WB ESS Survey Report shows stats for Addis -- what is upper and lower bound? */
preserve
	clear
	use "$pathout/childHealth_I_2014.dta"
	*recode rural (0 = 2)
	*la def rur 1 "Rural" 2 "Small Town"
	*la val rural rur
	append using "$pathout/childHealth_I_2012.dta", generate(_append)

	* Create regions at which survey is representative, relabel code 20
	clonevar region = saq01
	recode region (2 5 6 12 13 15 = 20)
	labmm SAQ01 20 "Other regions"
	la val region SAQ01

	* Create a panel tracking tag
	keep if childTag == 1
	bys individual_id household_id: gen ptrack = _N
	sum ptrack, d
	replace ptrack = 3 if ptrack == `r(max)'
	bys individual_id (year): gen ageDiff = ageMonths[2]-ageMonths
	replace ageDiff = . if year == 2014

	* Keep only data for children to be vislualized in R
	keep if childTag == 1
	export delimited using "$pathexport/ETH_201506_cHealth.csv", replace
	save "$pathout/ETH_201506_cHealth.dta", replace
restore

* Collapse down to hh level and max/ave key variables
ds(hh_s* pid2 hid2 individual_id individual_id2 ea_id _merge age*), not
keep `r(varlist)'
g year = 2014

qui include "$pathdo/copylabels.do"
#delimit ;
	collapse (max) illness totIllness malariaHH diarrheaHH respInfection chDiarrhea year
			 (mean) stunting underweight wasting BMI saq*
			 (sum) childTag, by(household_id2 household_id);
#delimit cr
qui include "$pathdo/attachlabels.do"

* Append to 2012 data to build health panel at household level
sa "$pathout/health_2014.dta", replace

pappend health_2012 health_2014 health_all
