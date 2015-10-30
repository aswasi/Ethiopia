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
/*clonevar educAdultM_cat = educAdultM_cnsrd
recode educAdultM_cat (2 3 = 1) (5 4 = 2) (6= 3)
clonevar educAdultF_cat = educAdultF_cnsrd
recode educAdultF_cat (2 3 = 1) (5 4 = 2) (6 = 3)
la def educLab 0 "No education" 1 "Primary" 2 "Secondary" 3 "Tertiary"
la val educAdultM_cat educLab
la val educAdultF_cat educLab
*/ 
* generate intensity of treatement variable assuming a nonlinear decay (exponential decay and inverse distance)
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
xtile disttile = ftfdist, nq(30)
tab disttile if year == 2012, sum(ftfdist)
tab disttile if year == 2014, sum(ftfdist)

set more on
foreach x of varlist q1_HFIAS numMonthFoodShort illnessShk q8_HFIAS q9_HFIAS {
	twoway(lpoly `x' disttile if year == 2012)(lpoly `x' disttile if year == 2014)
	 more
	 }
*
tab disttile if year == 2012, sum(ftfdist)
tab disttile if year == 2014, sum(ftfdist)

twoway(lpoly q1_HFIAS disttile if year == 2012)(lpoly q1_HFIAS disttile if year == 2014), /*
*/ytitle(Percent hh answering yes) xtitle(Distance to FTF zone (deciles)) legend(order(1 "2012" 2 "2014")) by(femhead) /*
*/ by(, title("Food security perceptions by Gender of Household Head", size(small))) 


* Create ftfzone treatment var that includes all hh within 10 KM of ftfzones
clonevar ftfzone_10km = ftfzone
replace ftfzone_10km = 1 if ftfdist <=10

* Create ftf treatment variables assuming everying is not in the program in 2011/12
g byte period = (year == 2014)
g byte treatment = (ftfzone_5km == 1)
* Create interaction of two vars above, this is the term we care about for dnd impact eval.
g postTreatment = period * treatment
g postTreatmentDecay1 = period * treatDecayExp
g postTreatmentDecay2 = period * treatDecaySq

* We see substantial effects in treated households for Food security perception questions; 
global exogVars "hhsize educHoh literateHoh agehead femhead"
global dnd "postTreatment period treatment"
global clusterme "cluster(ea_id)"
global depVars "priceShk FCS dietDiv illnessShk illness q1_HFIAS q2_HFIAS q8_HFIAS q9_HFIAS numMonthFoodShort"

* Here are some vars we've looked at in various models 
global demog "agehead c.agehead#c.agehead i.femhead i.marriedHoh vulnHead i.religHoh"
global educ "literateHoh educAdultM_cnsrd educAdultF_cnsrd gendMix ae mlabor flabor hhsize"
global educ2 "i.literateHoh "
global educ2 "literateHoh i.educAdultM_cat i.educAdultF_cat gendMix depRatio mlabor flabor hhsize"
global ltassets " iddirMemb" 
global ltassets2 "TLUtotal_cnsrd wealthIndex landHectares ib(4).landQtile iddirMemb"
global ltassets3 "l2.TLUtotal_cnsrd l2.wealthIndex l2.landHectares ib(4)l2.landQtile l2.iddirMemb" 
global ltassets4 "l2.TLUtotal_cnsrd l2.wealthIndex cl2.wealthIndex#cl2.wealthIndex l2.landHectares ib(4)l2.landQtile l2.iddirMemb"
global geog "dist_road dist_popcenter dist_market dist_borderpost i.ftfzone_5km"

global shocks "priceShk hazardShk"

est clear
foreach x of varlist $depVars{
	diff `x' if femhead == 1 & ptrack == 2, t(treatment) p(period) cov($exogVars) $clusterme
	eststo Spec`x': reg `x' $dnd $exogVars if femhead == 1, $clusterme
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
*/ TLUtotal_cnsrd landOwn wealthIndex dungFuel flushToilet electricity q1_HFIAS q2_HFIAS /*
*/ q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS roomsPC treatWater dadbioHoh avgNumMealsAdults avgNumMealsKids) test

** Propensity score matching combined w/ DnD **
* Doc: A Primer for Applying Propensity-Score Matching *

* Conduct a balancing test on potential predictive covariates
* Create some interactions to allow PSM logit to be as flexible as possible
g ageEduc = agehead * educHoh
g educSq = educHoh^2
g ageheadCube = agehead ^ 3
g educCube = educHoh ^ 3
g byte orthodox = (religHoh == 1)
g byte muslim = (religHoh == 4 )
g ageOrthodox = orthodox * agehead
g educOrthodox = orthodox * educHoh



global hhchar "agehead ageheadsq ageheadCube educHoh educSq educCube ageEduc orthodox muslim vulnHead marriedHohp educAdultM_cnsrd educAdultF_cnsrd"
global hhchar2 "marriedHoh hhsize depRatio gendMix under5 over64 hhlabor dadbioHoh mombioSpouse educSpouse literateHoh crowding"
global geog2   "dist_road distRoad distNrstDoc dist_popcenter dist_market dist_borderpost dist_admctr af_bio_1 af_bio_8 af_bio_12 af_bio_13 af_bio_16 srtm anntot_avg"

diff q1_HFIAS, t(treatment) p(period) cov($hhchar $hhchar2 $geog2) test

stepwise, pr(0.2): logit treatment $hhchar $hhchar2 $geog2 if year == 2012 & ptrack == 2
predict yhat2 
diff q1_HFIAS , t(treatment ) p(period) kernel id(hid) ktype(gaussian) pscore(yhat2) bs reps(50)


