/*-------------------------------------------------------------------------------
# Name:		12_comminfo
# Purpose:	Process commnity level information about events and access to services
# Author:	Tim Essam, Ph.D.
# Created:	2015/07/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close 
log using "$pathlog/10_assets.txt", replace
di in yellow "`c(current_date)' `c(current_time)'"



// -------- Part 1.b ---------- *
u "$wave1/sect1b_com_w1.dta"

* Community perceptions on presentation
clonevar childCloth = cs1bq01
clonevar childShoes = cs1bq02
clonevar adultCloth = cs1bq03
clonevar adultShoes = cs1bq04
clonevar homeSweep = cs1bq05

g byte mudHomeComm = inlist(cs1bq06, 1, 10) == 1
g byte thatchRoofComm = inlist(cs1bq07, 3, 4, 5) == 1
clonevar weredaOffice = cs1bq08
clonevar noticeBoard = cs1bq09
clonevar suggestBox = cs1bq11

la var mudHomeComm "Mud home freq at comm level"
la var thatchRoofComm "Thatch roof freq at comm level"

foreach x of varlist childCloth childShoes adultCloth adultShoes weredaOffice noticeBoard suggestBox {
		recode `x' (2 = 0)
		tab `x', mi
}

ds(cs* sa1*), not
keep `r(varlist)'

sa "$pathout/commInfo1b_2012.dta", replace

// -------- Part 2 ----------
u "$wave1/sect2_com_w1.dta", clear

egen yrsLiveComm = mean(cs2q06), by(ea_id)
ds(cs* sa1*), not
keep `r(varlist)'

sa "$pathout/commInfo2_2012.dta", replace

* -------- Part 3 ----------
u "$wave1/sect3_com_w1.dta", clear

g byte outmigrateComm = (cs3q01 == 2)
g byte inmigrateComm = (cs3q01 == 1)

la var outmigrateComm "Community has more people moving out"
la var inmigrateComm "Community has more people moving in"

clonevar commPop = cs3q02
clonevar dominantRelig = cs3q04_a
clonevar polygamousPct = cs3q06

g byte farmUseComm = (cs3q07 ==2)
la var farmUseComm "land is most commonly used for farming in community"

ds(cs* sa1*), not
keep `r(varlist)'

sa "$pathout/commInfo3_2012.dta", replace

* -------- Part 4 ----------
u "$wave1/sect4_com_w1.dta", clear

clonevar roadType = cs4q01
clonevar distRoad = cs4q02_1
g byte commAccessVehic = (cs4q03 == 1)
clonevar nrstBusStation = cs4q06_1
clonevar nrstUrbCenter = cs4q12_b1
clonevar weeklyMkt = cs4q14
recode weeklyMkt (2 = 0)
g byte phoneAccess = (cs4q16 == 1)
g byte govSchlElec = (cs4q22 == 1)
g byte medShop = (cs4q29 == 1)
g byte healthFacElec = (cs4q35 == 1)
g byte hospWthMedPers = (cs4q36 == 1 & cs4q37 == 1)
clonevar distNrstDoc = cs4q39_1

g byte freeBedNetTrt = (cs4q41 == 1 & cs4q42_1 == 0)
g byte mfiPresent = cs4q47 == 1
g byte waterService = cs4q49 == 1

la var commAccessVehic "Commercial vehicle access to community"
la var phoneAccess "Public pay phone aviable for use"
la var govSchlElec "Nearest government run school has electricity"
la var medShop "Place to purchase common medecines"
la var healthFacElec "Nearest health post is electrified"
la var hospWthMedPers "Hospital in community with doctor available"
la var freeBedNetTrt "Groups working to provide free bed nets"
la var mfiPresent "Micro finance inst. in community"
la var waterService "Water service in community"

ds(cs* sa1*), not
keep `r(varlist)'

sa "$pathout/commInfo4_2012.dta", replace

* -------- Part 5 ----------
u "$wave1/sect5_com_w1.dta", clear

g byte seasonalMig = (cs5q02 == 1)
g byte inMigration = (cs5q06 == 1)
g byte microEntrpz = (cs5q09 == 1)

la var seasonalMig "Community has seasonal migration out"
la var inMigration "Community has in-migration from other areas"
la var microEntrpz "Coop/microenterprise present for opporutities"

ds(cs* sa1*), not
keep `r(varlist)'

sa "$pathout/commInfo5_2012.dta", replace

* -------- Part 6 ----------
u "$wave1/sect6_com_w1.dta", clear

clonevar majorCrops = cs6q02_a
clonevar plantMonth = cs6q03_a
clonevar harvMonth = cs6q04_a
clonevar rainAssment = cs6q05
clonevar rainStart = cs6q06
clonevar rainEnd = cs6q07

g byte agExtAgent = (cs6q08 == 1)
g byte irrigation = (cs6q10 == 1)

clonevar fertSupplier = cs6q12
clonevar pestSupplier = cs6q13
clonevar hybSeedSupplier = cs6q14
clonevar cropYldPercep = cs6q18_a
clonevar cropIncPercep = cs6q18_b

ds(cs* sa1*), not
keep `r(varlist)'

la var agExtAgent "Ag extension agent lives in community"
la var irrigation "Irrigation scheme present in community"

sa "$pathout/commInfo6_2012.dta", replace

* -------- Part 7----------
u "$wave1/sect7_com_w1.dta", clear

/*NOTEL Years are recorded in Ethiopian Calendar years 
 Tthiopian calendar is 2007 == September 11, AD 2014 of 
 the Gregorian calendar.
2003 = 2011
2004 = 2012
*/

* Major shocks affect the community - how do these compare w/ hh shocks?
* Market price volatility is #1 major event, new school or dev project is 
* no. 1 good event *
 tab cs7q01_b cs7q00, mi

levelsof cs7q02, local(levels)
foreach x of local levels {
		tab cs7q01_b cs7q00 if cs7q02==`x'
		display in yellow "ETH year `x'"
}
