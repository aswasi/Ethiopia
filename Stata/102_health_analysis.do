/*-------------------------------------------------------------------------------
# Name:		04a_health_merge
# Purpose:	Merge individual level health data with education data
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/17
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close 
log using "$pathlog/health_merge_Ind.log", replace
set more off 

* Import and convert excel files to temp files for merging
global pathgit "C:/Users/Tim/Documents/GitHub/Ethiopia/Data/"
cd $pathgit

tempfile ed12 ed14 ind12 ind14
use education2012.dta, clear
clonevar individual_id = indivID2012
g year = 2012
*save "`ed12'"
sa "$pathout/education2012.dta", replace

use indiv2012.dta, clear
clonevar individual_id = indivID2012
g year = 2012
*save "`'ind12'"
merge 1:1 individual_id using "$pathout/education2012.dta", gen(merge_tmp)
drop merge_tmp
sa "$pathout/indiv2012.dta", replace

use education2014_noID14.dta, clear
clonevar individual_id = indivID2012
g year = 2014
append using "$pathout/indiv2012.dta" 
sa "$pathout/cHealth_merge.dta", replace

use "$pathout/ETH_201506_cHealth.dta"
drop if individual_id == ""
merge 1:1  individual_id year using "$pathout/cHealth_merge.dta", gen(chealth_mg)

keep if chealth_mg == 3
drop chealth_mg
ren ptrack ptrackChild
sa "$pathout/cHealth_all.dta", replace

* Merge in full dataset at household level
use "$pathgit/Data/ETH_201508_analysis_panel.dta", clear
merge 1:m household_id year using "$pathout/cHealth_all.dta", gen(ind_to_hh) force
keep if ind_to_hh == 3

* Generate age categories by year
g ageCat = 0 if ageMonths>6 & ageMonths <=11 & year == 2012
replace ageCat = 1 if ageMonths>11 & ageMonths <=23 & year == 2012 
replace ageCat = 2 if ageMonths>23 & ageMonths <=59 & year == 2012 
replace ageCat = 3 if ageMonths>6 & ageMonths <=11 & year == 2014
replace ageCat = 4 if ageMonths>11 & ageMonths <=23 & year == 2014 
replace ageCat = 5 if ageMonths>23 & ageMonths <=59 & year == 2014

la def agecat 0 "6 - 11 mo. 2012" 1 "12 - 23 mo. 2012" 2 "24 - 59 mo. 2012" /*
*/ 3 "6 - 11 mo. 2014" 4 "12 - 23 mo. 2014" 5 "24 - 59 mo. 2014"
la val ageCat agecat
la var ageCat "Age categories for stunting analysis"

sa "$pathout/ETH_201508_Child_Analysis.dta", replace

clear
use "$pathgit/ETH_201508_Child_Analysis.dta", replace
* Start analysis of stunting, wasting and underweight
encode household_id, gen(hhid)
encode individual_id, gen(indiv_id)
xtset indiv_id year

* Break out education into categories
clonevar educAdultM_cat = educAdultM
recode educAdultM_cat (2 3 = 1) (5 4 = 2) (6 = 3)
clonevar educAdultF_cat = educAdultF
recode educAdultF_cat (2 3 = 1) (5 4 = 2) (6 = 3)
la def educLab 0 "No education" 1 "Primary" 2 "Secondary" 3 "Tertiary"
la val educAdultM_cat educLab
la val educAdultF_cat educLab

global pathgitH "C:/Users/t/Documents/GitHub/Ethiopia/Data"
*merge m:m latitude longitude using "$pathgitH/ETH_ftf_distance.dta", gen(dist_treat)

* Create ftf treatment variables assuming everying is not in the program in 2011/12
g byte period = (year == 2014)
g byte treatment = (ftfzone_5km == 1)
* Create interaction of two vars above, this is the term we care about for dnd impact eval.
g postTreatment = period * treatment
g byte treatment2 = (ftfzone == 1)

* Create a censored variable for TLUtotal
mdesc TLUtotal
clonevar TLUtotal_cnsrd = TLUtotal
replace TLUtotal_cnsrd = 0 if TLUtotal_cnsrd == .
replace ftfzone = . if ftfzone == 99

diff stunted, t(treatment ) p(period) cluster(ea_id)
reg stunted postTreatment period treatment ageMonths c.ageMonths#c.ageMonths i.gender FCS, cluster(ea_id)
diff wasted, t(treatment ) p(period) cluster(ea_id)
diff underwgt, t(treatment ) p(period) cluster(ea_id)

global demog "agehead c.agehead#c.agehead i.femhead i.marriedHoh vulnHead i.religHoh dadbioHoh mombioSpouse femCount20_34 femCount35_59"
global cdemog "ageMonths c.ageMonths#c.ageMonths i.gender FCS"
global educ "literateHoh educAdultM educAdultF gendMix mlabor flabor hhsize"
global educ2 "i.literateHoh "
global educ2 "literateHoh i.educAdultM_cat i.educAdultF_cat gendMix depRatio mlabor flabor hhsize"
global ltassets " iddirMemb" 
global TLUs "TLUcattle TLUchx TLUsheep TLUasses TLUcamel"
global ltassets2 "wealthIndex landHectares ib(4).landQtile iddirMemb"
global ltassets3 "l2.wealthIndex landHectares ib(4)l2.landQtile l2.iddirMemb" 
global ltassets4 "l2.TLUtotal_cnsrd l2.wealthIndex cl2.wealthIndex#cl2.wealthIndex landHectares ib(4)l2.landQtile l2.iddirMemb"
global geog "dist_road dist_popcenter dist_market dist_borderpost i.ftfzone_5km dist_FTFzone"
global shocks "priceShk hazardShk healthShk healthShkComm hospWthMedPers"


est clear
foreach x of varlist stunted wasted underwgt  {
	
	* Run 5 regression specifications using global macros defined above
	qui eststo `x'_1, title("`x' 2012.1"): probit `x' $cdemog $demog $educ2 $TLUs $ltassets2 $geog ib(4).regionAll $shocks if year == 2012, cluster(hhid)
	qui eststo `x'_2, title("`x' 2014.1"): probit `x' $cdemog $demog $educ2 $TLUs $ltassets2 $geog ib(4).regionAll $shocks if year == 2014, cluster(hhid)
	qui eststo `x'_3, title("`x' 2014.2"): probit `x' $cdemog $demog $educ2 $TLUs $ltassets3 $geog ib(4).regionAll $shocks if year == 2014, cluster(hhid)

	*qui eststo `x'_3, title("`x' 2014.1"): reg `x' $demog $educ2 $ltassets $geog  ib(4).regionAll $year2 
	*qui eststo `x'_4, title("`x' 2014.2"): reg `x' $demog $educ2 $ltassets2 $geog ib(4).regionAll $year2 
	*qui eststo `x'_5, title("`x' 2014.3"): reg `x' $demog $educ2 $ltassets3 $geog ib(4).regionAll $year2 
	*capture g byte `x'_sample2014 = e(sample) == 1
	
	* Print results to screen and to text files in both wide and long formats
	esttab `x'_*, se star(* 0.10 ** 0.05 *** 0.01) label
	qui esttab `x'_* using "$pathreg/`x'.txt", se star(* 0.10 ** 0.05 *** 0.001) label replace
	qui esttab `x'_* using "$pathreg/`x'Wide.txt", wide plain se mlabels(none) label replace
	*display in yellow "Executed regression for `x' variable."
}
*end
estimates dir
esttab *_*, se star(* 0.10 ** 0.05 *** 0.01) label

probit stunted $demog $educ2 TLUtotal_cnsrd $ltassets2 $geog ib(4).regionAll $shocks i.year if ageCat == 2 | ageCat == 5, cluster(ea_id)




lookforit TLU
probit stunted $demog $ltassets $geog wealthIndex TLUcattle TLUchx TLUsheep TLUasses literateHoh educHoh hhsize dadbioHoh mombioSpouse femCount20_34 femCount35_59 i.year ib(4).regionAll, cluster(ea_id)
probit stunted $demog $ltassets $geog wealthIndex TLUcattle TLUchx TLUsheep TLUasses TLUcamel  literateHoh educHoh hhsize dadbioHoh mombioSpouse femCount20_34 femCount35_59 i.year ib(4).regionAll, cluster(ea_id)

tab ageCat
tab ageCat, nol
probit stunted $demog $ltassets $geog wealthIndex TLUcattle TLUchx TLUsheep TLUasses TLUcamel  literateHoh educHoh hhsize dadbioHoh mombioSpouse femCount20_34 femCount35_59 i.year ib(4).regionAll if inlist(ageCat , 0, 3), cluster(hhid)
probit stunted $demog $ltassets $geog wealthIndex TLUcattle TLUchx TLUsheep TLUasses TLUcamel  literateHoh educHoh hhsize dadbioHoh mombioSpouse femCount20_34 femCount35_59 i.year ib(4).regionAll if inlist(ageCat , 1, 4), cluster(hhid)
