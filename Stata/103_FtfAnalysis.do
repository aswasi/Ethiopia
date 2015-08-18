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
g postTreatment = period * treatment

* We see substantial effects in treated households for Food security perception questions; 
foreach x of varlist mobile hazardShk healthShk FCS dietDiv  /*
*/ q1_HFIAS q2_HFIAS q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS numMonthFoodShort treatWater avgNumMealsAdults avgNumMealsKids {
	diff `x', t(treatment) p(period)
}


* Test diff-n-diff for health shocks
reg healthShk postTreatment period treatment, cluster(ea_id)

diff healthShk, t(treatment) p(period) cov(agehead femhead marriedHoh under5 hhlabor literateHoh literateSpouse/*
*/ mlabor flabor youth25to35 over35under65 educHoh protWaterAll mobile malaria diarrheaHH iddirMemb /*
*/ TLUtotal_cnsrd landOwn wealthIndex  dungFuel flushToilet electricity q1_HFIAS q2_HFIAS /*
*/ q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS roomsPC treatWater dadbioHoh avgNumMealsAdults avgNumMealsKids) test



replace ftf_treatment = 1 if year == 2014 & ftfzone ==1


* Run baseline equivalence tests for major indicators; Use unequal vairances 
estpost ttest assetShk hazardShk healthShk priceShk if year == 2012, by(ftfzone) une
estpost ttest fcsMin FCS dd dietDiv if year == 2012, by(ftfzone) une

global demog "agehead c.agehead#c.agehead i.femhead i.marriedHoh vulnHead i.religHoh"
global educ "literateHoh educAdultM_cnsrd educAdultF_cnsrd gendMix ae mlabor flabor hhsize"
global educ2 "i.literateHoh "
global educ2 "literateHoh educAdultM educAdultF gendMix depRatio mlabor flabor hhsize"
global ltassets " iddirMemb" 
global ltassets2 "TLUtotal_cnsrd wealthIndex landHectares ib(4).landQtile iddirMemb"
global ltassets3 "l2.TLUtotal_cnsrd l2.wealthIndex l2.landHectares ib(4)l2.landQtile l2.iddirMemb" 

estpost ttest agehead femhead marriedHoh vulnHead  if year == 2012, by(ftfzone) une
estpost ttest literateHoh educAdultM_cnsrd educAdultF_cnsrd gendMix ae mlabor flabor hhsize if year == 2012, by(ftfzone) une
estpost ttest iddirMemb wealthIndex landHectares TLUtotal_cnsrd if year == 2012, by(ftfzone) une


xtreg priceShk year ftf_treatment, i(hid) cluster(saq01) 
