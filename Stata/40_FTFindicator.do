/*-------------------------------------------------------------------------------
# Name:			41_FTFindicator
# Purpose:		Process Foreign Assistance data for visualization in AGOL
# Author:		Tim Essam, Ph.D.
# Created:		2015/07
# Copyright:	USAID GeoCenter
# License:		<Tim Essam Consulting/OakStream Systems, LLC> MIT
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathlog/FTFIndicator.txt", replace
cd "$pathout"

import delimited "$pathgit/Data/Nearest_FTF_zone.csv", clear
destring near_dist, replace ignore(",")
destring in_fid, replace ignore(",")
ren near_dist dist_FTFzone 
la var dist_FTFzone "distance to nearest FTF ZOI"

tempfile temp1 ftftag
save "`temp1'"

import delimited "$pathgit/Data/LSMS_FTF_Admin3_within_ascii.txt", clear
ren fid in_fid
drop if xcoord == "NULL"
merge 1:1 in_fid using "`temp1'"

* Probably easiest to collapse down to lat/lon level and merge based on this; The unique ids 
* are getting stripped out by ArcGIS; First join in the distance to FTF zone information;
keep latitude longitude ptrack year *name* join_count dist_FTFzone ea_id 
recode join_count (4 5 6 7 8 9 10 12 = 1)
g byte ftfzone = (join_count == 1)
la var ftfzone "Household is within FTF zone"
keep if ptrack == "Both waves"

* Collapse down to lat lon year 
collapse join_count  dist_FTFzone ftfzone , by(latitude longitude *name* year)
clonevar lat2 = latitude
clonevar lon2 = longitude
replace lat2 = round(lat2, 0.0001)
replace lon2 = round(lon2, 0.0001)
save "`ftftag'"

u "$pathout/hh_base.dta", clear
keep if ptrack == 2

*Fix latitude and longitude so they are consistent across waves; Merging on these values
clonevar lat2 = LAT_DD_MOD
clonevar lon2 = LON_DD_MOD
replace lat2 = round(lat2, 0.0001)
replace lon2 = round(lon2, 0.0001)

merge m:m lat2 lon2 using "`ftftag'"
keep if ptrack == 2
tab _merge

* Clean up some of the vars
keep household_id household_id2 ea_id ea_id2 rural /*
	*/ saq01 ptrack gps_change gps_change_tot final_GPS_delta /*
	*/ final_GPS_deltaTot lat2 lon2 latitude longitude name name_0 /*
	*/ name_1 name_2 name_3 nl_name_3 varname_3 join_count  /*
	*/ dist_FTFzone ftfzone year

/*
local required_file ETH_201507_LSMS_FTF_ZOI
foreach x of local required_file { 
	 capture findfile `x'.csv, path($pathout)
		if _rc==601 {
			noi disp in red "File not found. Downloading `x'.txt file to Dataout folder"
			copy https://raw.githubusercontent.com/tessam30/Ethiopia/master/Data/FTF_LSMS_join.txt $pathout/ETH_201507_LSMS_FTF_ZOI.csv, replace   	
			cd "$pathout"
			import delimited "$pathout\ETH_201507_LSMS_FTF_ZOI.csv", clear
			* Create an exit conditions based on whether or not file is found.
		}
		else di in yellow "Data downloaded, continue with do file"
		*cd "$pathin"
		import delimited "$pathout\ETH_201507_LSMS_FTF_ZOI.csv", clear
		}
*end

* Create an ftf zone variable changing
clonevar ftfZone = join_count
recode ftfZone (2 3 4 = 1)
la var ftfZone "Feed the future zone"

ren household_ household_id
ren household1 household_id2

* Retain only enough information for merging with panel data
* keep fid join_count target_fid household_id household_id2 ea_id ea_id2 rural saq01 year ptrack region latitude longitude

*B/c the id's are no longer unique (due to ArcMap rounding errors), collapsing data down to lat/lon
* for merging w/ ea_ids; 

collapse ftfZone, by(latitude longitude)
drop if latitude == 0
*/

export delimited "$pathgit/Data/ETH_FTF_LSMS_join.csv", replace
sa "$pathout/ETH_FTF_LSMS_join.dta", replace
