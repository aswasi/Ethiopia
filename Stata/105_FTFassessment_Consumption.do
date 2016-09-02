* Import github data with distance to zone as well as FTF matched to admin 3 areas  
clear
capture log close 
log using "$pathlog/FTFanalysis.log", replace

* Append consumption data together and remove hh mising household_id as these
* will on be in the 2nd wave; 

use "$pathexport/ETH_201508_analysis_panel.dta"

* Merge in consumption aggreates to see if consumption varies in/out of FTF zones
merge 1:1 household_id year using "$pathout/consumption_all_idfull.dta", gen(_merge_cons)


* generate intensity of treatement variable assuming a nonlinear decay (exponential decay and inverse distance)
clonevar ftfdist = dist_FTFzone
replace ftfdist = ftfdist/1000
g byte period = (year == 2014)

* Conduct a balancing test on potential predictive covariates
* Create some interactions to allow PSM logit to be as flexible as possible
g ageEduc = agehead * educHoh
g educSq = educHoh^2
g ageheadCube = agehead ^ 3
g educCube = educHoh ^ 3
g byte orthodox = (religHoh == 1)
g byte muslim = (religHoh == 4 )
g ageOrthodox = orthodox * agehead
g educOrthodox = orthodox * educHoh

g byte soilSalt = (sq5 == 1)
g byte soilToxicity = (sq6 == 1)
g byte soilNutrient = (sq2 == 1)

* Define a set of predictors for treatment
global hhchar "agehead ageheadsq ageheadCube educHoh educSq educCube ageEduc orthodox muslim vulnHead marriedHohp educAdultM_cnsrd educAdultF_cnsrd"
global hhchar2 "marriedHoh depRatio gendMix under5 over64 hhlabor dadbioHoh mombioSpouse literateHoh crowding"
global geog2   "dist_road distRoad dist_popcenter dist_market dist_borderpost dist_admctr af_bio_1 af_bio_8 af_bio_12 af_bio_13 af_bio_16 srtm anntot_avg"
global doug "soilSalt soilToxicity soilNutrient noKitchen indoorKitchen wetQ_avgstart h2011_wetQ metalRoof ag mudFloor male dungFuel electricity wetQ_avg healthFacComm sen_avg literateSpouse fsrad3_agpct h2011_tot houseSize h2011_eviarea"

sort household_id pw
bysort household_id: gen weight = pw[1]

* Loop across distance threshold values for ftf buffer & core variables
est clear
*q2_HFIAS  q3_HFIAS q4_HFIAS q5_HFIAS q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS 
*0(1)10
*drop treatmentbuff postTreatmentbuff 
*q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS
foreach y of varlist real_totcons_aeq  {
	forvalues i=0(1)10 {
		g byte treatmentbuff = (ftfdist <= `i')
		g postTreatmentbuff = period * treatmentbuff
		tab treatmentbuff
		di in red "buffer size is `i' kilometers"
		
		* First estimate diff in diff
		qui eststo reg_`y'`i', title("`y' `i'km buffer"):reg `y' postTreatmentbuff period treatmentbuff, $clusterme
		qui eststo reg2_`y'`i', title("`y' `i'km buffer"):reg `y' postTreatmentbuff period treatmentbuff if (ftfdist > (`i'+5) | treatmentbuff), $clusterme		
		qui eststo wgt_`y'`i', title("`y' `i'km buffer"):reg `y' postTreatmentbuff period treatmentbuff [pweight = weight], $clusterme
		qui eststo wgt2_`y'`i', title("`y' `i'km buffer"):reg `y' postTreatmentbuff period treatmentbuff [pweight = weight] if (ftfdist > (`i'+5) | treatmentbuff), $clusterme

		* Create a propensity score
		qui stepwise, pr(0.2): logit treatmentbuff $hhchar $hhchar2 $geog2 $doug if year == 2012 & ptrack == 2 
		predict p_treat if e(sample)
		eststo psm_`y'`i': diff `y', t(treatmentbuff) p(period) pscore(p_treat) kernel id(household_id) ktype(gaussian) bs reps(100) support
		*eststo psm2_`y'`i': diff `y' if (ftfdist > (`i'+5) | treatmentbuff), t(treatmentbuff) p(period) pscore(p_treat) kernel id(household_id) ktype(gaussian) bs reps(100) support
		*matrix beta = e(b)
		*matrix cov = e(V)
		*disp as result "`y' ", _cont _column(30)
		*disp as error beta[1,3]/(cov[3,3]^.5)*/
		drop treatmentbuff postTreatmentbuff p_treat _est*
		}
	}
*end


*q6_HFIAS q7_HFIAS q8_HFIAS q9_HFIAS 
foreach y of varlist real_totcons_aeq  {
	di in yellow "`y' dependent variable"
	esttab reg_`y'*, star(* 0.10 * 0.05 ** 0.01 *** 0.001) b(3) compress
	esttab reg2_`y'*, star(* 0.10 * 0.05 ** 0.01 *** 0.001) b(3) compress
	esttab wgt_`y'*, star(* 0.10 * 0.05 ** 0.01 *** 0.001) b(3) compress
	esttab wgt2_`y'*, star(* 0.10 * 0.05 ** 0.01 *** 0.001) b(3) compress
	esttab psm2_`y'*, star(* 0.10 * 0.05 ** 0.01 *** 0.001)  b(3) compress
	*esttab reg_`y'* using "$pathreg/reg`y'.txt", label replace 
	*esttab psm_`y'* using "$pathreg/psm`y'.txt", label replace 	
}
*end

g byte treatmentbuff = (ftfdist <= 5)
g postTreatmentbuff = period * treatmentbuff
stepwise, pr(0.2): logit treatmentbuff $hhchar $hhchar2 $geog2 $doug if year == 2012 & ptrack == 2
predict p_treat if e(sample)
diff numMonthFoodShort, t(treatmentbuff) p(period) pscore(p_treat) kernel id(household_id) ktype(gaussian) bs reps(100) support
twoway (kdensity p_treat if treatmentbuff) (kdensity p_treat if !treatmentbuff)
twoway(kdensity p_treat if treatmentbuff & _support == 1) (kdensity p_treat if !treatmentbuff & _support == 1)


g byte test = (ftfdist <= 5)
stepwise, pr(0.2): logit test $hhchar $hhchar2 $geog2 $doug if year == 2012 & ptrack == 2
predict p_score if e(sample)
diff numMonthFoodShort, t(test) p(period) pscore(p_score) kernel id(household_id) ktype(gaussian) bs reps(100) support

* generate new graph showing the distribution of p-scores after matching
twoway (kdensity p_score if test)(kdensity p_score if !test), /*
*/ legend(label(1 "Treatment") label(2 "Control")) ytitle("Estimated Propensity Score") /*
*/ title("Unmatched Distribution of Estimated P-scores by Treat Status") scheme(burd8)

twoway (kdensity p_score if test [aweight = _weights]) /*
*/ (kdensity p_score if !test [aweight = _weights]), /*
*/ legend(label(1 "Treatment") label(2 "Control")) /* 
*/ ytitle("Estimated Propensity Score") title("Matched Distribution of Estimated P-scores by Treat Status") /*
*/ scheme(burd8)


* Other shocks
