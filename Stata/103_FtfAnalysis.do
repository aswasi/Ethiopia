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
clonevar ftfzone_2km = ftfzone
replace ftfzone_2km = 1 if ftfdist <= 2

* Create ftf treatment variables assuming everying is not in the program in 2011/12
g byte period = (year == 2014)
g byte treatment = (ftfzone_2km == 1)
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

/* Check baseline equivalence of key covariates
diff healthShk, t(treatment) p(period) cov(relig1 relig2 relig3 relig4 agehead femhead marriedHoh under5 hhlabor literateHoh literateSpouse educHoh/*
*/ mlabor flabor youth25to35 over35under65 educAdultM educAdultF protWaterAll mobile malaria diarrheaHH iddirMemb /*
*/ TLUtotal_cnsrd landOwn wealthIndex  dungFuel flushToilet electricity q1_HFIAS q2_HFIAS /*
*/ q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS roomsPC treatWater boilWater chlorinateWater dadbioHoh avgNumMealsAdults avgNumMealsKids) test
*/

/*
diff healthShk if femhead==1, t(ftfzone) p(period) cov(priceShk illnessShk agehead marriedHoh under5 hhlabor literateHoh literateSpouse/*
*/ mlabor flabor youth25to35 over35under65 educHoh educAdult protWaterAll mobile malaria diarrheaHH iddirMemb /*
*/ TLUtotal_cnsrd landOwn wealthIndex dungFuel flushToilet electricity q1_HFIAS q2_HFIAS /*
*/ q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS roomsPC treatWater dadbioHoh avgNumMealsAdults avgNumMealsKids) test
*/

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

g byte soilSalt = (sq5 == 1)
g byte soilToxicity = (sq6 == 1)
g byte soilNutrient = (sq2 == 1)

* Define a set of predictors for treatment
global hhchar "agehead ageheadsq ageheadCube educHoh educSq educCube ageEduc orthodox muslim vulnHead marriedHohp educAdultM_cnsrd educAdultF_cnsrd"
global hhchar2 "marriedHoh hhsize depRatio gendMix under5 over64 hhlabor dadbioHoh mombioSpouse literateHoh crowding"
global geog2   "dist_road distRoad dist_popcenter dist_market dist_borderpost dist_admctr af_bio_1 af_bio_8 af_bio_12 af_bio_13 af_bio_16 srtm anntot_avg"
global doug "soilSalt soilToxicity soilNutrient noKitchen indoorKitchen wetQ_avgstart h2011_wetQ metalRoof ag mudFloor male dungFuel electricity wetQ_avg healthFacComm sen_avg literateSpouse fsrad3_agpct h2011_tot houseSize h2011_eviarea"

diff q1_HFIAS, t(treatment) p(period) cov($hhchar $hhchar2 $geog2) test

stepwise, pr(0.2): reg treatment $hhchar $hhchar2 $geog2 $doug if year == 2012 & ptrack == 2
predict yhat2 if e(sample) 
diff q1_HFIAS , t(treatment) p(period) kernel id(hid) ktype(gaussian) pscore(yhat2) 

global shockVars "priceShk hazardShk illnessShk illness malariaHH"
global nutritionVars "fcsMin dietDiv numMonthFoodShort q1_HFIAS q2_HFIAS q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS"
global agVars "inorgFert orgFert fertPct urea ureaPct manure manurePct landHectares"

/*foreach y of varlist $nutritionVars {
	reg `y' postTreatment period treatment if (ftfdist > 5 | treatment),  $clusterme
	diff `y' if  (ftfdist > 5 | treatment), t(treatment) p(period) $clusterme
	*matrix beta = e(b)
	*matrix cov = e(V)
	*disp as result "`y' ", _cont _column(30)
	*disp as error beta[1,1]/(cov[1,1]^.5)
}
*/

* Check for missingness in covariates
mdesc $hhchar $hhchar2 $geog2 $doug if year == 2012

* Tim's version of treatment using a 5km buffer on the ftfzone
foreach y of varlist illnessShk numMonthFoodShort q1_HFIAS q2_HFIAS q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS {
	reg `y' postTreatment period treatment, $clusterme
	diff `y', t(treatment) p(period) $clusterme
}

g byte insample = e(sample)

* Run a logit to predict propesity score, check confusion matrix for accuracy
forvalues i = 0.1(0.01)0.70 {
	qui stepwise, pr(`i'): logit treatment $hhchar $hhchar2 $geog2 $doug if year == 2012 & ptrack == 2, $clusterme or
	qui estat classification
	display in yellow "`r(P_p1)'% of treatment household correctly predicted using `i' value for stepwise"
}
*end

* Loop across distance threshold values for ftf buffer & core variables

q1_HFIAS q2_HFIAS  q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS
est clear
drop treatmentbuff postTreatmentbuff 
foreach y of varlist illnessShk numMonthFoodShort q1_HFIAS q2_HFIAS  q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS {
	forvalues i=0(1)10 {
		g byte treatmentbuff = (ftfdist <= `i')
		g postTreatmentbuff = period * treatmentbuff
		tab treatmentbuff
		di in red "buffer size is `i' kilometers"
		
		* First estimate diff in diff
		eststo `y'`i', title("`y' `i'km buffer"):reg `y' postTreatmentbuff period treatmentbuff, $clusterme

		* Create a propensity score
		qui stepwise, pr(0.2): reg treatmentbuff $hhchar $hhchar2 $geog2 $doug if year == 2012 & ptrack == 2
		predict p_treat if e(sample)
		diff `y', t(treatmentbuff) p(period) pscore(p_treat) $clusterme
		*matrix beta = e(b)
		*matrix cov = e(V)
		*disp as result "`y' ", _cont _column(30)
		*disp as error beta[1,3]/(cov[3,3]^.5)*/
		drop treatmentbuff postTreatmentbuff p_treat
		}
	}
*end

esttab numMonthFoodShort*, star(* 0.10 ** 0.05 *** 0.01) label
esttab nc_numMonthFoodShort*, star(* 0.10 ** 0.05 *** 0.01) label


