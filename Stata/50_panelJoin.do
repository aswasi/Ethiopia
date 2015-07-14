/*-------------------------------------------------------------------------------
# Name:		50_panelJoin
# Purpose:	Join panel data for Ethiopia 2012 & 2014 LSMS
# Author:	Tim Essam, Ph.D.
# Created:	2015/07/06
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------*/

clear
capture log close
set more off
log using "$pathlog/panelJoin", replace

u "$pathout/hh_base.dta"
merge 1:1 household_id2 year using "$pathout/shocks_all.dta", gen(merge_shocks) force

*Fix latitude and longitude so they are consistent across waves
g double latitude = lat_dd_mod
g double longitude = lon_dd_mod
replace latitude = LAT_DD_MOD if latitude == . & LAT_DD_MOD != .
replace longitude = LON_DD_MOD if longitude == . & LON_DD_MOD != .

* Download Feed the Future spatial join data cluster
preserve
local rqr_file ETH_FTF_LSMS_join 
foreach x of local rqr_file {
	capture findfile `x'.csv, path($pathout)
		if _rc == 601  {
			noi disp in red "File not found, downloading from Github repository."
			copy https://raw.githubusercontent.com/tessam30/Ethiopia/master/Data/ETH_FTF_LSMS_join.csv $pathout/`x'.csv, replace
			import delimited "$pathout/`x'.csv", clear
			sort latitude longitude
			g lat2 = round(latitude, 0.000000001)
			g long2 = round(longitude, 0.000000001)
			g id = _n
			isid id
			save "$pathout/`x'.dta", replace
		}
		else di in yellow "File located, loading into working space and converting to Stata .dta file"
		import delimited "$pathout/`x'.csv", clear
		sort latitude longitude
		g lat2 = round(latitude, 0.000000001)
		g long2 = round(longitude, 0.000000001)
		g id = _n
		isid id
		save "$pathout/`x'.dta", replace
}
restore

g lat2 = round(latitude, 0.000000001)
g long2 = round(longitude, 0.000000001)

* Merge in the Feed the future data using the rounded lat/lon as points of joining;
* Had to do this b/c ArcMap truncated household ids; 
merge m:1 lat2 long2 using "$pathout/ETH_FTF_LSMS_join.dta", gen(ftfMerge)


fs *all.dta
* Merge all data sets together
local mlist hhchar timeuse health dietdiv assets housing hfias tlu lvstkprod
foreach x of local mlist {
	merge 1:1 household_id2 year using "$pathout/`x'_all.dta", gen(merge_`x') force
	compress
	di in yellow "Merging `x' to the househlold base dataset."
	}

* By region, check the ftf versus non-ftf households
foreach x of varlist dietDiv FCS hfiasindex_rur TLUtotal wealthindex_rur infraindex_rur priceShk hazardShk {
	disp in yellow "Cross-tabulating `x' with region, year and FTF zones"
	bys region: table year ftfzone , c(mean `x')
	set more on
}

bys region: table year ftfzone , c(mean priceShk)

* --------------------------------------------- *
/* TODO: Validate/verify major variables of use



























* Export a cut of data for joining with FTF zones in ArcMap
preserve
keep household_id household_id2 ea_id ea_id2 rural saq01 ptrack region year latitude longitude
drop if year == .
export delimited "$pathexport/ETH_201507_LSMS.csv", replace
restore

* Break up into years and save cuts for WVU folks 
export delimited "$pathexport/ETH_2012_lsms.csv" if year == 2012, replace
export delimited "$pathexport/ETH_2014_lsms.csv" if year == 2014, replace
export delimited "$pathexport/ETH_all_lsms.csv", replace

















/* EXTRA CODE: Create a master id for the panel to enable reclink command to be used
drop if year == .
drop if household_id == ""
egen idMaster = group(household_id year)
drop _merge

reclink lat2 long2 using "$pathout/ETH_FTF_LSMS_join.dta", idm(idMaster) idu(id) gen(fuzzyMatch)
br lat* lon* if longitude == 36.0818398582
*/

