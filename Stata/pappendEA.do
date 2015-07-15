
* Create a simple function to append data from the panel.
* Use: Function is called anytime datasets are merged together at the ea level.

capture program drop pappendEA
program define pappendEA
set more off

	/* Three inputs required 
	* 1 - 2012 data file name
	* 2 - 2014 data file name
	* 3 - data file name to which you are saving */

	clear
	use "$pathout/comm_base.dta", clear
	merge 1:1 ea_id year using "$pathout/`1'.dta", gen(_2012) update replace
	merge 1:1 ea_id2 year using "$pathout/`2'.dta", gen(_2014) update 

	drop _2012 _2014

	sa "$pathout/`3'.dta", replace
end
