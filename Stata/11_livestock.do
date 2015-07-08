/*-------------------------------------------------------------------------------
# Name:		11_livestock
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

u "$wave1/sect8a_ls_w1.dta"

* Verify that q14 + q15 = q13 (total livestock of each type)
* Fix the 15 instances where the condition does not hold
g totlstktmp =  ls_s8aq14a + ls_s8aq15a
*assert ls_s8aq13a >= totlstk if totlstktmp!=.

clonevar totlstk = ls_s8aq13a
replace totlstk = totlstktmp if ls_s8aq14a > ls_s8aq13a & totlstktmp!=.

local lvsk cattle sheep goat horse donkey mule camel hen1 hen2 cocks cockerels pullet chick
local i = 1
foreach x of local lvsk {
		egen `x'Tot = total(ls_s8aq13a) if ls_s8aq00 == `i', by(household_id)
		qui replace `x'Tot = 0 if `x'Tot == .
		la var `x'Tot "Total `x's owned by hh"
		display in yellow "`i' corresponds to `x'"		
		local i = `++i' 
}

/*Create TLU (based on values from http://www.fao.org/wairdocs/ilri/x5443e/x5443e04.htm)
Notes: Sheep includes sheep and goats
Horse includes all draught animals (donkey, horse, bullock)
chxTLU includes all small animals (chicken, fowl, etc).*/
g camelVal 	= 1.00
g cattleVal = 0.70
g sheepVal 	= 0.10
g horsesVal = 0.80
g mulesVal 	= 0.70
g assesVal 	= 0.50
g chxVal 	= 0.01

* Create TLU group values
g TLUcamel = (camelTot) * camelVal
g TLUcattle = (cattleTot) * cattleVal
g TLUsheep = (sheepTot + goatTot) * sheepVal
g TLUhorses = (horseTot) * horsesVal
g TLUmules = (muleTot) * mulesVal
g TLUasses = (donkeyTot) * assesVal
g TLUchx = (hen1Tot + hen2Tot + cocksTot + cockerelsTot + pulletTot + chickTot) * chxVal

local tlulist camel cattle sheep horses mules asses chx
foreach x of local tlulist {
		la var TLU`x' "Total `x' owned in TLUs"
} 

* Generate overall TLUs
egen TLUtotal = rsum(TLUcamel TLUcattle TLUsheep TLUhorses TLUmules TLUasses TLUchx)
la var TLUtotal "Total tropical livestock units"

* Two households have rather TLU holdings; Leaving for now but may need to revisit once analysis starts
sum TLUtotal

* Generate information on beehives and aparian activities; Very small numbers so stopping
egen bhiveTrad = total(ls_s8aq26) if ls_s8aq00 == 14, by(household_id)
egen bhiveIntmd = total(ls_s8aq27) if ls_s8aq00 == 14, by(household_id)
egen bhiveMod = total(ls_s8aq28) if ls_s8aq00 == 14, by(household_id)
egen bhiveTot = rsum(bhiveTrad bhiveIntmd bhiveMod)

egen bhTradProd = total(ls_s8aq29a_1)  if ls_s8aq00 == 14, by(household_id)
egen bhTradHarv = total(ls_s8aq29b) if ls_s8aq00 == 14, by(household_id)
egen bhIntmdProd = total(ls_s8aq29c_1) if ls_s8aq00 == 14, by(household_id)
egen bhIntmdHarv = total(ls_s8aq29d) if ls_s8aq00 == 14, by(household_id)
egen bhModProd = total(ls_s8aq29e_1) if ls_s8aq00 == 14, by(household_id)
egen bhModHarv = total(ls_s8aq29f) if ls_s8aq00 == 14, by(household_id)

g byte grazeOutVill = (ls_s8aq58 == 1) 
la var "Grazed livestock outside of village"


la var bhiveTrad "Total traditional beehives"
la var bhiveIntmd "Total intermediate beehives"
la var bhiveMod "Total modern beehives"
la var bhiveTot "Total beehives"
la var bhTradProd "Total production (kg) from traditional"
la var bhTradHarv "Total harvests fom traditional"
la var bhIntmdProd "Total production (kg) from intermediate"
la var bhIntmdHarv "Total harvests from intermediate"
la var bhModProd "Total production (kg) from modern"
la var bhModHarv "Total harvests (kg) from modern"

