/*-------------------------------------------------------------------------------
# Name:		01_GeographicInfo
# Purpose:	Merge lat long info for households and export to R for jittering/viz
# Author:	Tim Essam, Ph.D.
# Created:	10/31/2014; 02/19/2015.
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathlog/01_GeographicInfo", replace

* Load and extract relevant data
use "$wave1/Pub_ETH_HouseholdGeovariables_Y1.dta"
clonevar latitude = LAT_DD_MOD 
clonevar longitude = LON_DD_MOD
keep household_id ea_id longitude latitude
g wave1 = 2012

tempfile wave1
save "wave1", replace

* Load 2nd wave and merge two together
use "$wave2/Pub_ETH_HouseholdGeovars_Y2.dta"
clonevar latitude = lat_dd_mod
clonevar longitude = lon_dd_mod

keep household_id household_id2 household_id household_id2 ea_id ea_id2 latitude longitude

* Create a sequence of strings to replace
tempvar id2 miss
g byte `miss' = (household_id == "")
bys `miss': gen `id2' = _n
tostring `id2', replace
replace household_id = `id2' if household_id == ""
isid household_id
gen wave2 = 2014

* Merge together to map in R or as geojson for github page
merge 1:1 household_id using "wave1", gen(geo_merge)

g svy_status = .
replace svy_status = 1 if wave2 == . & wave1 == 2012
replace svy_status = 2 if wave2 == 2014 & wave1 == .
replace svy_status = 3 if wave2 !=. & wave1 !=.

la def status 1 "Wave 1 only" 2 "Wave 2 only" 3 "Both waves"
la val svy_status status

drop `id2' `miss'

* Save to pathout folder
sa "$pathout/geovars.dta", replace
