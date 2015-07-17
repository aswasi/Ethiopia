/*-------------------------------------------------------------------------------
# Name:		13_credit
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
log using "$pathlog/10_credit.txt", replace
di in yellow "`c(current_date)' `c(current_time)'"
set more off

u "$wave1/sect14a_hh_w1.dta"

* Focus on first loan reported (about 93% of reported loans)
g byte multLoans = (hh_s14q00 >1 & hh_s14q00!=.)
drop if inlist(hh_s14q00, 2, 3, 4) ==1

clonevar borrowed = hh_s14q01
recode borrowed (2 = 0)

clonevar loanSource = hh_s14q02
clonevar loanReason = hh_s14q04
clonevar loanMonth = hh_s14q05_a
clonevar loanYear = hh_s14q05_b 	
clonevar loanHHmemb = hh_s14q03_a
clonevar loanRepaid = hh_s14q06
clonevar loanDueMonth = hh_s14q07_a
clonevar loanDueYear = hh_s14q07_b

la var borrowed "Household borrowed money"
la var multLoans "Household had multiple loans"
g year = 2012

ds(hh_s* saq* pw rural), not
keep `r(varlist)'

sa "$pathout/credit_2012.dta", replace

* 2014 Credit info *

u "$wave2/sect14b_hh_w2.dta", clear 
g borrowed = 1 if hh_s14q00 == 1
g byte multLoans = (hh_s14q00 >1 & hh_s14q00!=.)
keep if borrowed == 1
clonevar loanSource = hh_s14q02_b
clonevar loanHHmemb = hh_s14q03_a
clonevar loanReason = hh_s14q04
clonevar loanMonth = hh_s14q05_a
clonevar loanYear = hh_s14q05_b 	
clonevar loanRepaid = hh_s14q06
clonevar loanDueMonth = hh_s14q07_a
clonevar loanDueYear = hh_s14q07_b

la var borrowed "Household borrowed money"
la var multLoans "Household had multiple loans"
g year = 2014

ds(hh_s* saq* pw rural ea_id ea_id2), not
keep `r(varlist)'

sa "$pathout/credit_2014.dta", replace

pappend credit_2012 credit_2014 credit_all
