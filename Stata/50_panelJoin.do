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
fs *all.dta

* Merge all data sets together
local mlist hhchar timeuse health dietdiv assets housing shocks hfias
foreach x of local mlist {
	merge 1:1 household_id2 year using "$pathout/`x'_all.dta", gen(merge_`x') force
	compress
	di in yellow "Merging `x' to the househlold base dataset."
	}

* Break up into years and save cuts for WVU folks 
export delimited "$pathexport/ETH_2012_lsms.csv" if year == 2012, replace
export delimited "$pathexport/ETH_2014_lsms.csv" if year == 2014, replace
export delimited "$pathexport/ETH_all_lsms.csv", replace
