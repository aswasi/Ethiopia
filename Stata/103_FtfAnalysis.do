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


* Feed the future analysis 
clear
capture log close 
log using "$pathlog/FTFanalysis.log", replace

use "$pathexport/ETH_201508_analysis_panel.dta"
clonevar educAdultM_cat = educAdultM_cnsrd
recode educAdultM_cat (2 3 = 1) (5 4 = 2) (6 5= 3)
clonevar educAdultF_cat = educAdultF_cnsrd
recode educAdultF_cat (2 3 = 1) (5 4 = 2) (6 5 = 3)
la def educLab 0 "No education" 1 "Primary" 2 "Secondary" 3 "Tertiary"
la val educAdultM_cat educLab
la val educAdultF_cat educLab

* Create ftf treatment variables assuming everying is not in the program in 2011/12
g byte period = (year == 2014)
g byte treatment = (ftfzone == 1)
* Create interaction of two vars above, this is the term we care about for dnd impact eval.
g postTreatment = period * treatment

* We see substantial effects in treated households for Food security perception questions; 
global exogVars "hhsize educHoh femhead literateHoh landOwn"
global dnd "postTreatment period treatment"
global clusterme "robust"
global depVars "illnessShk illness q1_HFIAS q2_HFIAS q3_HFIAS q8_HFIAS q9_HFIAS numMonthFoodShort diarrheaHH"

est clear
foreach x of varlist $depVars{
	diff `x', t(treatment) p(period) cov($exogVars) $clusterme
	eststo Spec`x': reg `x' $dnd $exogVars if femhead==1, $clusterme
	}
*end
esttab, se star(* 0.10 ** 0.05 *** 0.01) label
est dir

/* NOTES: Female headed household heterogeneity analysis show results as well; */

* Check baseline equivalence of key covariates
diff healthShk, t(treatment) p(period) cov(agehead femhead marriedHoh under5 hhlabor literateHoh literateSpouse/*
*/ mlabor flabor youth25to35 over35under65 educHoh protWaterAll mobile malaria diarrheaHH iddirMemb /*
*/ TLUtotal_cnsrd landOwn wealthIndex  dungFuel flushToilet electricity q1_HFIAS q2_HFIAS /*
*/ q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS roomsPC treatWater dadbioHoh avgNumMealsAdults avgNumMealsKids) test


