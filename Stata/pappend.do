
* Create a simple function to append data from the panel.
* Use: Function is called anytime datasets are merged together

capture program drop pappend
program define pappend
set more off

	/* Three inputs required 
	* 1 - 2009 data file name
	* 2 - 2010 data file name
	* 3 - 2011 data file name to which you are appending */

	clear
	use "$pathout/hh_base.dta", clear
	merge 1:1 household_id year using "$pathout/`1'.dta", gen(_2012) update replace
	merge 1:1 household_id2 year using "$pathout/`2'.dta", gen(_2014) update 

	drop _2012 _2014

	sa "$pathout/`3'.dta", replace
end

