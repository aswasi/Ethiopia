/*-------------------------------------------------------------------------------
# Name:		10_assets
# Purpose:	Process household assets and create wealth index
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

* Load module on food aggregates for dietary diversity score
use "$wave1/sect10_hh_w1.dta", clear

* Look at assets and codes (35 codes possible)
tab hh_s10q00, mi

g byte stove = inlist(hh_s10q00, 1, 2, 3) == 1 & hh_s10q01 >0
g byte mitad = inlist(hh_s10q00, 20, 21) == 1 & hh_s10q01 >0
g byte ax = inlist(hh_s10q00, 31, 32) == 1 & hh_s10q01 >0
g byte plough = inlist(hh_s10q00, 33, 34) == 1 & hh_s10q01 >0
g byte cart = inlist(hh_s10q00, 16, 17) == 1 & hh_s10q01 >0

#delimit ;
local vlist blanket bed watch phone mobile radio tv dvd sat sofa 
		bike moto sew weave refrig car jewel clothing shelf stove
		well mofera sickle pump;

forvalues i=4(1)35 {
		while `i' != 
}
#delimit cr
