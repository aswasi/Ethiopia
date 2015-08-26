/*-------------------------------------------------------------------------------
# Name:		103_FTFanalysis.do
# Purpose:	Run basic data validation and analysis of shocks for the panel
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/17
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

* Import github data with distance to zone as well as FTF matched to admin 3 areas  
clear
capture log close 
log using "$pathlog/FTFanalysis.log", replace

use "$pathexport/ETH_201508_analysis_panel.dta"
clonevar educAdultM_cat = educAdultM_cnsrd
recode educAdultM_cat (2 3 = 1) (5 4 = 2) (6= 3)
clonevar educAdultF_cat = educAdultF_cnsrd
recode educAdultF_cat (2 3 = 1) (5 4 = 2) (6 = 3)
la def educLab 0 "No education" 1 "Primary" 2 "Secondary" 3 "Tertiary"
la val educAdultM_cat educLab
la val educAdultF_cat educLab

* generate intensity of treatement variable assuming a nonlinear decay
clonevar ftfdist = dist_FTFzone
replace ftfdist = ftfdist/1000
la var ftfdist "distance from FTF zone in kilometers"
g treatDecayExp = exp(-ftfdist/5) if ftfdist != 0
g treatDecaySq  = 1/(ftfdist^1.5) if ftfdist != 0
replace treatDecaySq = 1 if treatDecaySq >1 | ftfdist == 0
replace treatDecayExp = 1 if treatDecayExp >1 | ftfdist ==0
la var treatDecayExp "intensity of treatment exponential"
la var treatDecaySq "intensity of treatment distance decay"
twoway(scatter  treatDecaySq ftfdist)(scatter  treatDecayExp ftfdist) 

** Create a variable to show how indicators dampen as distance increases
* Plot how indicators change across distances

tab disttile if year == 2012, sum(ftfdist)
tab disttile if year == 2014, sum(ftfdist)
xtile disttile = ftfdist, nq(30)
set more on
foreach x of varlist q1_HFIAS numMonthFoodShort illnessShk q8_HFIAS q9_HFIAS {
	twoway(lpoly `x' disttile if year == 2012)(lpoly `x' disttile if year == 2014)
	 more
	 }
*
tab disttile if year == 2012, sum(ftfdist)
tab disttile if year == 2014, sum(ftfdist)


* Create ftfzone treatment var that includes all hh within 10 KM of ftfzones
clonevar ftfzone_10km = ftfzone
replace ftfzone_10km = 1 if ftfdist <=10

* Create ftf treatment variables assuming everying is not in the program in 2011/12
g byte period = (year == 2014)
g byte treatment = (ftfzone == 1)
* Create interaction of two vars above, this is the term we care about for dnd impact eval.
g postTreatment = period * treatment
g postTreatmentDecay1 = period * treatDecayExp
g postTreatmentDecay2 = period * treatDecaySq


* We see substantial effects in treated households for Food security perception questions; 
global exogVars "hhsize educHoh literateHoh landOwn"
global dnd "postTreatmentDecay1 period treatDecayExp"
global clusterme "cluster(ea_id)"
global depVars "priceShk FCS dietDiv illnessShk illness q1_HFIAS q2_HFIAS q8_HFIAS q9_HFIAS numMonthFoodShort malaria boilWater chlorinateWater"

est clear
foreach x of varlist $depVars{
	diff `x', t(treatment) p(period) cov($exogVars) $clusterme
	eststo Spec`x': reg `x' $dnd $exogVars, $clusterme
	}
*end
esttab, se star(* 0.10 ** 0.05 *** 0.01) label
est dir

/* NOTES: Female headed household heterogeneity analysis show results as well; */

* Check baseline equivalence of key covariates
diff healthShk, t(treatment) p(period) cov(relig1 relig2 relig3 relig4 agehead femhead marriedHoh under5 hhlabor literateHoh literateSpouse educHoh/*
*/ mlabor flabor youth25to35 over35under65 educAdultM educAdultF protWaterAll mobile malaria diarrheaHH iddirMemb /*
*/ TLUtotal_cnsrd landOwn wealthIndex  dungFuel flushToilet electricity q1_HFIAS q2_HFIAS /*
*/ q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS roomsPC treatWater boilWater chlorinateWater dadbioHoh avgNumMealsAdults avgNumMealsKids) test


diff healthShk if femhead==1, t(ftfzone) p(period) cov(priceShk illnessShk agehead marriedHoh under5 hhlabor literateHoh literateSpouse/*
*/ mlabor flabor youth25to35 over35under65 educHoh educAdult protWaterAll mobile malaria diarrheaHH iddirMemb /*
*/ TLUtotal_cnsrd landOwn wealthIndex  dungFuel flushToilet electricity q1_HFIAS q2_HFIAS /*
*/ q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS roomsPC treatWater dadbioHoh avgNumMealsAdults avgNumMealsKids) test


*Export a cut of data to test out GWR dnd
keep if ptrack == 2
keep latitude longitude q1_HFIAS period treatment postTreatment hhid year
