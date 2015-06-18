/*-------------------------------------------------------------------------------
# Name:		02_hhchar_2012
# Purpose:	Process household characteristics and education
# Author:	Tim Essam, Ph.D.
# Created:	2015/06/17
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

* Preliminaries - clone id vars and define regular household members
clonevar hid = household_id
clonevar pid = individual_id

g byte hhmemb = inlist(hh_s1q05, 0, 1, 2, 3, 4, 5, 6, .) == 1
la var hhmemb "Usual member of household"

*Keeping only subset which consist of regular household members
keep if hhmemb == 1

/* Demographic list to calculate
1. Head of Household Sex
2. Relationship Status
*/


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
g byte marriedHoh = inlist(hh_s1q08, 2, 3) & hoh == 1
g byte marriedHohm = (hh_s1q08 == 2 & hoh == 1)
g byte marriedHohp = (hh_s1q08 == 3 & hoh == 1)
g byte nonmarriedHoh = (inlist(hh_s1q08, 1, 4, 5, 6)==1) & hoh == 1
g byte widowFemhead = femhead == 1 & hh_s1q08 == 6
g byte nonmarriedFemhead = femhead == 1 & inlist(hh_s1q08, 1, 4, 5)

la var marriedHoh "Married hoh (any type)"
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
egen hhsize = total(hhmemb), by(hid)
la var hhsize "household size"

* Create gender ratio for households
g byte male = hh_s1q03 == 1 
g byte female = hh_s1q03 == 2 
la var male "male hh members"
la var female "female hh members"

egen msize = total(male), by(hid)
la var msize "number of males in hh"

egen fsize = total(female), by(hid)
la var fsize "number of females in hh"

* Create a gender ratio variable
g gendMix = msize/fsize
*recode gendMix (. = 0) if fsize==0
la var gendMix "Ratio of males to females (1 = 1:1 mix)"

* Calculate age demographics (Youth)
* Make cuts at 0-5; 6-9; 10-14; 15-17; 18-24; 25-30; 31-35;
* Youth standard definition is 15-24; 

* Some obs have both age in years and months with a flag for obs over 5 yoa
g byte und5tmp = (hh_s2q01!="X" & hh_s1q04_b!=.)

* Create categorical for age group chunks
/* NOTE: These codes will be used throughout for demographic factors so determine
   the cuts needed before initiating the cut; Will save time downstream.
*/
egen youthtmp = cut(hh_s1q04_a), at(0, 5, 10, 12, 15, 18, 25, 31, 36, 60, 65, 100) icodes
replace youthtmp = 0 if und5tmp
table youthtmp, c(min hh_s1q04_a max hh_s1q04_a)
table und5tmp youthtmp

egen youthtmp2 = cut(hh_s1q04_a), at(0, 10, 20, 100) icodes
replace youthtmp2 = 0 if und5tmp


* Create binary variables for demographic categories
g byte under5tmp = inlist(youthtmp, 0) 
g byte under15tmp = inlist(youthtmp, 0, 1, 2, 3) 
g byte under24tmp = inlist(youthtmp, 0, 1, 2, 3, 4, 5)
g byte youth15to24tmp = inlist(youthtmp, 4, 5) 
g byte youth18to30tmp = inlist(youthtmp, 5, 6)
g byte youth25to35tmp = inlist(youthtmp, 6, 7) 
g byte over35under65tmp = inlist(youthtmp, 8, 9)
g byte over64tmp = inlist(youthtmp, 10) 

* Create total, male and female totals at the household level of each demographic
local demo under5 under15 under24 youth15to24 youth18to30 youth25to35 over35under65 over64
foreach x of local demo {
	egen `x'  = total(`x'tmp), by(hid)
	egen `x'm = total(`x'tmp) if male == 1, by(hid)
	egen `x'f = total(`x'tmp) if female == 1, by(hid)
	
	* Replace missing values with zeros
	replace `x' = 0 if `x' == .
	replace `x'm = 0 if `x'm == .
	replace `x'm = 0 if `x'm == .
	
	la var `x' "total hh members `x'"
	la var `x'm "total male hh members `x'"
	la var `x'f "total female hh members `x'"
}

/* Create intl. HH dependency ratio 
# HH Dependecy Ratio = [(# people 0-14 + those 65+) / # people aged 15-64 ] * 100 # 
The dependency ratio is defined as the ratio of the number of members in the age groups 
of 14 years and above 65 years to the number of members of working age (15-64 years). 
The ratio is normally expressed as a percentage (data below are multiplied by 100 for pcts.*/
g byte numDepRatio = inlist(youthtmp, 4, 5, 6, 7, 8, 9) != 1
g byte demonDepRatio = inlist(youthtmp, 4, 5, 6, 7, 8, 9) == 1
egen totNumDepRatio = total(numDepRatio), by(hid)
egen totDenomDepRatio = total(demonDepRatio), by(hid)

* Check that numbers add to hhsize
assert hhsize == totNumDepRatio+totDenomDepRatio if hhmemb==1
g depRatio = (totNumDepRatio/totDenomDepRatio)*100 if totDenomDepRatio!=.
*recode depRatio (. = 0) if totDenomDepRatio==0
la var depRatio "Dependency Ratio"

* Calculate household labor shares (ages 12 - 60)
/* Household Labor Shares */
g byte hhLabort = inlist(youthtmp, 3, 4, 5, 6, 7, 8)==1
egen hhlabor = total(hhLabort), by(hid)
la var hhlabor "hh labor age>11 & < 60"

g byte mlabort = inlist(youthtmp, 3, 4, 5, 6, 7, 8)==1 & male == 1
egen mlabor = total(mlabort), by(hid)
la var mlabor "hh male labor age>11 & <60"

g byte flabort = inlist(youthtmp, 3, 4, 5, 6, 7, 8)==1 & female == 1
egen flabor = total(flabort), by(hid)
la var flabor "hh female labor age>11 & <60"
drop hhLabort mlabort flabort

* Male/Female labor share in hh
g mlaborShare = mlabor/hhlabor
recode mlaborShare (. = 0) if hhlabor == 0
la var mlaborShare "share of working age males in hh"

g flaborShare = flabor/hhlabor
recode flaborShare (. = 0) if hhlabor == 0
la var flaborShare "share of working age females in hh"

* % of hh females aged 20-34 & 35 - 59
g byte fem20_34tmp = (hh_s1q04_a>=20 &hh_s1q04_a<35) & (female == 1)
g byte fem35_59tmp = (hh_s1q04_a>=35 &hh_s1q04_a<60) & (female == 1)
egen femCount20_34 = total(fem20_34tmp), by(hid)
egen femCount35_59 = total(fem35_59tmp), by(hid)
g femRatio20_34 = femCount20_34/hhsize
g femRatio35_59 = femCount35_59/hhsize

la var femRatio20_34 "Share of females in hh 20-34"
la var femRatio35_59 "Share of females in hh 35-59"
la var femCount20_34 "Number of females in hh 20-34"
la var femCount35_59 "Number of females in hh 35-59"

* --- Generate adult equivalents in household
g male10 	= 1
g fem10_19 	= 0.84
g fem20		= 0.72
g child10	= 0.60

g ae = .
replace ae = male10 if inlist(youthtmp2, 1, 2) == 1 & male == 1
replace ae = fem10_19 if inlist(youthtmp2, 1)==1 & female == 1
replace ae = fem20 if inlist(youthtmp2, 2) & female == 1 
replace ae = child10 if inlist(youthtmp2, 0)

la var ae "Adult equivalents in household"
egen adultEquiv = total(ae), by(hid)
la var adultEquiv "Total adult equivalent units"

drop male10 fem10_19 fem20 child10

* --- Biological Mother or Father present in hh?
g byte dadbioHoh = hh_s1q12 == 1 & hh_s1q13==1
g byte mombioSpouse = hh_s1q16 == 1 & hh_s1q17==2
g byte mombioHoh = hh_s1q16 == 1 & hh_s1q17==1

la var dadbioHoh "Biological dad present as hoh"
la var mombioSpouse "Biological mom present in hh as spouse"
la var mombioHoh "Biological mom present as hoh"

* --- Geographic migration codes - where husband/wife from same district?
tempvar hohloc spouseloc
g `hohloc' = hh_s1q11 if hoh == 1
g `spouseloc' = hh_s1q11 if spouse == 1

egen hohRegion = max(`hohloc'), by(hid)
egen spouseRegion = max(`spouseloc'), by(hid)
g byte mixedRegion = (hohRegion != spouseRegion) if marriedHoh==1 & hohRegion!=. & spouseRegion!=.
la var hohRegion "Region of birth hoh"
la var spouseRegion "Region of birth spouse"
la var mixedRegion "Household members from different regions"

* --- Education 
/* Literacy is defined as the ability to read with understanding and to 
 write meaningfully in any language. */
g byte literateHoh = hh_s2q02 == 1 & hoh == 1

* Assuming that if hh is not married, then litSpouse takes value of 0
g byte literateSpouse = hh_s2q02 == 1 & spouse == 1

la var literateHoh "Hoh is literate"
la var literateSpouse "Spouse is literate"

/* Education levels -
From: http://www.moe.gov.et/English/Resources/Documents/eab05.pdf
	No Education = 0, 98
	Pre-Primary - 0
	Lower primary - 1, 2, 3, 4, 93, 94, 96
	Primary - 5, 6, 7, 8
	Lower secondary - 9, 10, 21, 22
	Secondary - 11, 12, 25, 26, 27, 28, 29, 30, 13
	Tertiary - 14, 15, 16, 17, 18, 19, 31, 32, 33	
*/
g educ = . 
la var educ "Education levels"

* No education (This includes:"Don't Know" and "2" Responses))
replace educ = 0 if (inlist(hh_s2q05, 98) |  hh_s2q03 == 2)
* Pre-primary
replace educ = 1 if inlist(hh_s2q05, 0)
* Lower Primary
replace educ = 2 if inlist(hh_s2q05, 1, 2, 3, 4, 93, 94, 96)
* Upper Primary
replace educ = 3 if inlist(hh_s2q05, 5, 6, 7, 8)
* Lower Secondary
replace educ = 4 if inlist(hh_s2q05, 9, 10, 21, 22)
* Secondary 
replace educ = 5 if inlist(hh_s2q05, 11, 12, 13, 23, 24, 25, 26, 27, 28, 29, 30)
* Secondary 
replace educ = 6 if inlist(hh_s2q05, 14, 15, 16, 17, 18, 19, 31, 32, 33, 34)

lab def ed 0 "No education" 1 "Pre-primary" 2 "Lower Primary" 3 "Primary" /*
*/ 4 "Lower Secondary" 5 "Secondary" /*
*/ 6 "Tertiary"
la values educ ed

g educHoh = educ if hoh == 1
g educSpouse = educ if spouse == 1

* Create var capturing max education for those above 20
egen educAdult = max(educ) if youthtmp2 == 2, by(hid)
egen educAdultM = max(educ) if youthtmp2 == 2  & male == 1, by(hid)
egen educAdultF = max(educ) if youthtmp2 == 2  & female == 1, by(hid)

local edlist educAdult educAdultM educAdultF educHoh educSpouse
foreach x of local edlist {
	la values `x' ed
	}
*end

la var educAdult "Highest adult education in household"
la var educAdultM "Highest male adult education in household"
la var educAdultF "Highest female adult education in household"
la var educHoh "Education of Hoh"
la var educSpouse "Education of spouse"

* School Expenses
egen schoolFees = total(hh_s2q16), by(hid)
egen schoolSup = total(hh_s2q17), by(hid)
g schoolExp = schoolFees + schoolSup

la var schoolFees "Total hh school fees"
la var schoolSup "Total hh school supply expense"
la var schoolExp "Total hh school expenses (Fees + Supplies)"

* Retain only key variables for collapsing
drop *tmp
qui ds(hh_s* household_id individual_id), not
keep `r(varlist)'
compress

save "$pathout/hhchar_ind_2012.dta", replace

* Drop variables w/ strings as they will make collapse command bomb
drop pid youthtmp2 educ `hohloc' `spouseloc' `hohrelig' `sprelig' hhmemb ea_id

g year = 2012
la var year "Survey year"

* Collapse everything down to household level
qui include "$pathdo/copylabels.do"
qui ds(hid), not
collapse (max) `r(varlist)', by(hid) 
qui include "$pathdo/attachlabels.do"

* Fix variables for which values are missing due to no female members of hh
local dgph gendMix under5f under15f under24f youth15to24f youth18to30f youth25to35f over35under65f over64f mixedRegion
foreach x of local dgph {
	replace `x' = 0 if `x' == .
	}
*end

compress

* Attach value lables to relevant variables
foreach x of varlist educHoh educSpouse educAdult educAdultM educAdultF {
	la val `x' ed
	}
*end

foreach x of varlist religHoh religSpouse {
	la val `x' HH_S1Q07
	}
*end


sa "$pathout/hhchar_2012.dta", replace



