/*-------------------------------------------------------------------------------
# Name:		100_dataMerge
# Purpose:	Merge all files used to process data; call programs as well
# Author:	Tim Essam, Ph.D.
# Created:	2015/07
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------*/


* Run all the previous .do files to process data
global pathdo2 "C:/Users/Tim/Documents/GitHub/Ethiopia/Stata"

include "$pathdo2/00_SetupFolderGlobals.do"
*include "$pathdo2/100_dataMerge.do"

* Load the derived data product; Show it's format
use $pathout/ETH_201507_LSMS_ALL.dta, clear

* Working w/ only 2014 data so let's filter on that
keep if year == 2014

* Create variables for FCS graphics
foreach x of varlist FCS staplesFCS legumesFCS milkFCS  meatFCS vegFCS fruitFCS  sweetFCS oilFCS {
	* Create average value for each category
	egen mean_`x' = mean(`x')
	egen distmean_`x' = mean(`x'), by(regionAll)
	g dev_`x' = mean_`x' - distmean_`x'
	}
*end

sum mean_*

* Check for any missing values in our collapse variable
tab regionAll, mi
drop if regionAll == .

* Collapse down to regional level
order FCS mean_staplesFCS mean_legumesFCS mean_milkFCS mean_meatFCS mean_oilFCS mean_vegFCS mean_sweetFCS mean_fruitFCS

local fl foodlack_*

qui include "$pathdo/copylabels.do"
	collapse dev_staplesFCS dev_legumesFCS dev_milkFCS dev_meatFCS dev_oilFCS /*
	*/ dev_vegFCS dev_sweetFCS dev_fruitFCS dev_FCS FCS (count) nobs = FCS /*
	*/ (mean)`fl' , by(regionAll)
qui include "$pathdo/attachlabels.do"

* Sort the data based on FCS
sort FCS

* Export the data into an excel file
export excel using "C:\Users\Tim\Documents\GitHub\Ethiopia\Stata\ETH_2014_FCS_visualization.xls", /*
*/ firstrow(variables) replace
