/*-------------------------------------------------------------------------------
# Name:		100_dataMerge
# Purpose:	Merge all files used to process data; call programs as well
# Author:	Tim Essam, Ph.D.
# Created:	2015/07
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------*/

clear
capture log close 
log using "$pathlog/100_dataMerge.txt", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off

* Call all programs first and set up local macros
local plist cnumlist pappend pappendEA 00_SetupFolderGlobals
foreach x of local plist {
	qui include "$pathdo2/`x'.do"
}

* Call do files in order, running panel base first
include "$pathdo2/Panel_base.do"

local dolist GeographicInfo hhchar_2012 hhchar_2014 health HFIAS timeuse dietdiv shocks housing assets livestock comminfo credit
local z = 1
foreach x of local dolist {
		if `z' < 10 {
			*set tr on
			include "$pathdo2/0`z'_`x'.do"
			di in yellow "`z'_`x'"
			*local i = `++i'
		}
		else qui include "$pathdo2/`z'_`x'.do"
		local z = `++z'
} 

* Run additional files to pull feed the future information from github repo
include "$pathdo2/40_FTFindicator.do"
include "$pathdo2/50_panelJoin.do"


* Create overall wealth index from pp. 298
* http://documents.wfp.org/stellent/groups/public/documents/manual_guide_proced/wfp203197.pdf
* First, review existing indices to see their distributions (pull in outliers)
use $pathout/ETH_201507_LSMS_ALL.dta, clear

bys year: sum *index*
 winsor2 wealthindex_rur, replace cuts(1 99)

clonevar rural2 = rural
recode rural2 (2 3 = 0)


local fsVars  ax plough radio refrig tv moto mobile blanket bed sofa jewel crowding mudFloor noToilet electricity protWaterAll
local i = 0
local locVar urb rur
forvalues i=0/1 {
	factor `fsVars' if rural2 == `i', pcf
	rotate, oblique promax(2)
	alpha `fsVars' if rural2 == `i'

	loadingplot, mlabs(small) mlabc(maroon) mc(maroon) /*
	*/ xline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ yline(0, lwidth(med) lpattern(tight_dot) lcolor(gs10)) /*
	*/ title(Household infrastructure index loadings)

	if `i' == 0 {
			predict wealth_urb if rural2 == 0
	}
	else predict wealth_rur if rural2 == 1
}

histogram wealth_rur if wealth_rur>-0.4, by(saq01) legend(rows(2))
