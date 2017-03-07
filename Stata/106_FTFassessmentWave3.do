



* Import github data with distance to zone as well as FTF matched to admin 3 areas  
clear
capture log close 
log using "$pathlog/FTFanalysis_wave3.log", replace


* add in a year variable to consumption data
	cd $wave3
	use "cons_agg_w3.dta", replace
	gen year = 2016
	* Flag households with duplicate id counts
	bys household_id: gen id_count = _n
	bys household_id: gen id_dups  = _N
	* Dropping 3 households with dulpicate IDs
	drop if id_count == 2
save "$pathout/cons_agg_2016.dta", replace

* Process food perceptions
u "$wave3/sect7_hh_w3.dta", replace
	bys household_id: gen id_count = _n
	bys household_id: gen id_dups = _N
	* Dropping 3 households with dulpicate IDs
	drop if id_count == 2

* Drop any households who did not answer question 1 (57 of them)
	drop if hh_s7q01 == .
	g byte q1_HFIAS = (hh_s7q01 == 1)

	local i = 2
	foreach x of varlist hh_s7q02_a hh_s7q02_b hh_s7q02_c hh_s7q02_d hh_s7q02_e hh_s7q02_f hh_s7q02_g hh_s7q02_h {
			g byte q`i'_HFIAS = `x'>0 & `x'!=.
			clonevar q`i'a_HFIAS = `x'
			local i = `++i'
	}

	g condHFIAS = q7_HFIAS
	g byte domainHFIAS = (q2_HFIAS ==1 | q3_HFIAS ==1 | q4_HFIAS ==1)

	egen modHFIAS_score = rsum2(q2a_HFIAS q3a_HFIAS q4a_HFIAS q5a_HFIAS q6a_HFIAS q7a_HFIAS q8a_HFIAS q9a_HFIAS)

	g byte foodShortage = (hh_s7q06 == 1)
	replace foodShortage = . if  hh_s7q06 == .
	
	local dlist Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
	local vlist l a b c d e f g h i j k
	local n: word count `vlist'

	forvalues i = 1/`n'{
		local a: word `i' of `dlist'
		local b: word `i' of `vlist'
		g byte foodlack_`a' = regexm(hh_s7q07_`b', "X") == 1
		la var foodlack_`a' "Household face food shortage in `a'"
		tab foodlack_`a', mi
	}
	*end

	* Quickly summarize when shortages are occuring
	graph bar (mean) foodlack*, by(saq01) legend(rows(2))
	* Total household reporting shortages
	graph bar (sum) foodlack*, by(saq01) legend(rows(2))
	
	egen totMonFoodlack = rsum(foodlack_*)
	la var totMonFoodlack "Total months lacking food"
	
	* Clone the cause of food insecurity
	clonevar foodinsec_issue1 = hh_s7q08_a
	clonevar foodinsec_issue2 = hh_s7q08_b
	clonevar foodinsec_issue3 = hh_s7q08_c

	* Collapse to hh levels, merge in housing info to create wealth index per 
	ds(hh_s* saq* ea_id), not
	keep `r(varlist)'

	include "$pathdo/copylabels.do"
	ds(household_id* ea_id), not
		collapse (max) `r(varlist)', by(household_id) fast
	* Reapply variable lables & value labels
	include "$pathdo/attachlabels.do"
	g year = 2016
	compress
	
	sa "$pathout/hfias_2016.dta", replace
	
	use "$pathout/hfias_2016.dta", replace
	
	* Merge in the consumption aggregates to get us a 2016 base dataset for FTF analysis
	merge 1:1 household_id year using "$pathout/cons_agg_2016.dta", gen(_hfias_2016)
	sa "$pathout/ftf_analysis2016.dta", replace
	
	* Append in the analysis data from previous analysis
	append using "$pathexport/ETH_201508_analysis_panel.dta"
	sort household_id year
	
	* Flag households that are in the full panel
	bys household_id: gen full_panel = _N
	bys household_id: gen full_count = _n
	
	* Carry forward FTF status -- assuming household did not move!
	bysort household_id (year): carryforward ftfzone, gen(ftfzone_2016)
	
	* Not looking good for FTF regarding food security perceptions!!!
	foreach x of varlist q1_HFIAS q2_HFIAS q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS totMonFoodlack{
		display in yellow "`x'"
		table year ftfzone_2016 if full_panel == 3, c(mean `x') f(%9.3fc)
		}
	*end
	
	*Collapse results and export to be plotted in ggplot2
	preserve
	collapse (mean) q1_HFIAS q2_HFIAS q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS totMonFoodlack (count) freq = q1_HFIAS if full_panel == 3, by(ftfzone_2016 year)
	drop if ftfzone_2016 == .
	export delimited "$pathexport/ftfAnalysis_2016.csv", replace
	restore
	

* Merge in consumption aggreates to see if consumption varies in/out of FTF zones
merge 1:1 household_id year using "$pathout/consumption_all_idfull.dta", gen(_merge_cons)

merge 1:1 household_id year using "$pathout/cons_agg_2016.dta", gen(_cons_2016)
