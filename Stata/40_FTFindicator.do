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

import delimited "$pathgit/Data/LSMS_FTF_Admin3_within_ascii.txt", clear

* Recode the FTF zones after checking to ensure household_id + year gives unique combo
keep if ptrack == "Both waves"


 format hhid %16.0g
tostring household_, gen(household_id) force
sort household_id year


merge m:m household_id year using "$pathout/hh_base.dta"

bys household_id (year): gen idfind2 = _n
drop if idfind2 >2












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

export delimited "$pathgit/Data/ETH_FTF_LSMS_join.csv", replace
