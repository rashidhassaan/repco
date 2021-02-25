//Please see the readme for further documentation on this file.

capture log close
clear

//Windows path
cd "C:\Users\Nicolas\Dropbox\Depression Census\Datasets\Mississippi\Stata Data"
log using "..\Log Files\MS_preliminary_regs.txt", t replace
use MS_data_all_years_regs.dta, replace
cd ..\

//Mac path
//log using "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Manuscripts/Mississippi/MS_preliminary_regs.txt", t replace
//use "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Datasets/Mississippi/MS_data_all_years_regs.dta", replace
//cd "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Manuscripts/Mississippi/"

//compare firms in 1929 between the two regions
//Table 1 
ttest log_total_output_value if censusyear==1929, by(st_louis_fed) unequal
ttest log_wage_earners_total if censusyear==1929, by(st_louis_fed) unequal
ttest log_hours_per_wage_earner if censusyear==1929, by(st_louis_fed) unequal
ttest log_average_wage_a if censusyear==1929, by(st_louis_fed) unequal
ttest log_output_price_1 if  num_products==1 & censusyear==1929, by(st_louis_fed) unequal
ttest log_output_quantity_1 if  num_products==1 & censusyear==1929, by(st_louis_fed) unequal

//these firms shouldn't even be in here in the first place
drop if total_output_value<5000

gen p_wage=0
eststo clear

levelsof censusyear, local(years)
levelsof industrycode, local(industries)

foreach j of local industries{
	foreach i of local years{
	capture xtile p_wage_`j'_`i' = average_wage_a if censusyear==`i' & industrycode=="`j'", nquantiles(100)
	capture replace p_wage_`j'_`i' = 0 if p_wage_`j'_`i'==.
	capture replace p_wage = p_wage+p_wage_`j'_`i'
	capture drop p_wage_`j'_`i'
	}
}

gen log_wage_earners_mar = log(wage_earners_mar)
gen log_wage_earners_oct = log(wage_earners_oct)
gen cut = 1
//baseline regressions for plants
//1st set of regression for table 2
//regresions for table 3
//regressions for Table 4
foreach i in "log_total_output_value" "log_wage_earners_total" "log_average_wage_a" "log_hours_per_wage_earner" "log_wage_earners_oct" "log_wage_earners_mar"{
di "**************************************************************************************************"
di "Regressions for `i'"

if("`i'" == "log_hours_per_wage_earner"){
//minimal level of reasonableness for hours variable
replace cut = 0 if hours_per_wage_earner>84
}

if("`i'" == "log_average_wage_a"){
//play with how much of tails to trim 
local wage_cut = 1
replace cut = 0 if p_wage>=101-`wage_cut' | p_wage<=`wage_cut'
}

tab cut
//within estimator
qui eststo: xi: xtreg `i' st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929|censusyear==1931) & cut, fe vce(robust) 
//balanced panel, no firm fixed effects
qui eststo: xi: reg `i' st_louis_fed_1931 st_louis_fed i.year_1931*i.industrycode if balanced_1931 & cut, cluster(firmid)
//unbalanced panel
qui eststo: xi: reg `i' st_louis_fed_1931 st_louis_fed i.year_1931*i.industrycode if (censusyear==1929 | censusyear==1931) & cut, cluster(firmid) 
esttab using ../../Manuscripts/Mississippi/table_basic_`i'.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear
}

//regressions for Table 5 at plant-level
foreach i in "log_total_output_value" "log_wage_earners_total" "log_average_wage_a" "log_hours_per_wage_earner" "log_wage_earners_oct" "log_wage_earners_mar"{
di "**************************************************************************************************"
di "Regressions for `i'"

if("`i'" == "log_hours_per_wage_earner"){
//minimal level of reasonableness for hours variable
replace cut = 0 if hours_per_wage_earner>84
}

if("`i'" == "log_average_wage_a"){
//play with how much of tails to trim 
local wage_cut = 1
replace cut = 0 if p_wage>=101-`wage_cut' | p_wage<=`wage_cut'
}
//longer run regressions
//industry x year fixed effects within estimator
qui eststo: xi: xtreg `i' st_louis_fed_* i.censusyear*i.industrycode if cut, fe vce(robust)
//weird multi-collinearity problem when use county x year fixed effects
di "End of regressions for variable `i'"
di "**************************************************************************************************"
replace cut = 1
}
esttab using ../../Manuscripts/Mississippi/regs_long_run_plant.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_*) 
eststo clear

//useful for comparing to price and quantity results that follow
//regressions for single product firms
qui eststo: xi: xtreg log_output_value st_louis_fed st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929 | censusyear==1931) & num_products==1, fe vce(robust)
qui eststo: xi: xtreg log_wage_earners_total st_louis_fed st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929 | censusyear==1931) & num_products==1, fe cluster(firmid)
qui eststo: xi: xtreg log_hours_per_wage_earner st_louis_fed st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929 | censusyear==1931) & num_products==1, fe cluster(firmid)
esttab using ../../Manuscripts/Mississippi/robustness_singleProd.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear

//need to do some price tail trimming By industry
gen p_price=0

levelsof censusyear, local(years)
levelsof industrycode, local(industries)

foreach j of local industries{
	foreach i of local years{
	capture xtile p_price_`j'_`i' = output_price_1 if censusyear==`i' & industrycode=="`j'", nquantiles(100)
	capture replace p_price_`j'_`i' = 0 if p_price_`j'_`i'==.
	capture replace p_price = p_price+p_price_`j'_`i'
	capture drop p_price_`j'_`i'
	}
}

//regressions for 2nd and 3rd sets of regressions in table 2
foreach i in "log_output_quantity_1" "log_output_price_1" {
di "**************************************************************************************************"
di "Regressions for variable `i'"

if("`i'" == "log_output_price_1"){
//play with how much of tails to trim 
local price_cut = 1
replace cut = 0 if p_price>=101-`price_cut' | p_price<=`price_cut'
}
qui eststo: xi: xtreg `i' i.year_1931*i.industrycode st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929|censusyear==1931) & num_products==1 & cut, fe vce(robust)
//balanced panel, no firm effects
qui eststo: xi: reg `i' st_louis_fed_1931 st_louis_fed i.year_1931*i.industrycode if balanced_1931 & num_products==1 & cut, cluster(firmid)
//unbalanced panel
qui eststo: xi: reg `i'  st_louis_fed_1931 st_louis_fed i.year_1931*i.industrycode i.county if (censusyear==1929 | censusyear==1931)  & num_products==1 & cut, cluster(firmid)
esttab using ../../Manuscripts/Mississippi/regs_`i'.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear
}

//regressions for variables noted in table 5
foreach i in "log_output_quantity_1" "log_output_price_1" {
di "**************************************************************************************************"
di "Regressions for variable `i'"
//longer run regressions
//industry x year fixed effects within estimator
qui eststo: xi: xtreg `i' st_louis_fed_* i.censusyear*i.industrycode if  num_products==1 & cut, fe vce(robust)
di "End of regressions for variable `i'"
di "**************************************************************************************************"
}
esttab using ../../Manuscripts/Mississippi/regs_output_price_LR.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_*) 
eststo clear

//firm-level results for restricted set of counties
replace count = upper(county)
//regressions for table 6 at plant-level
gen border_county = 1 if county=="WASHINGTON" | county=="HUMPHREYS" | county=="HOLMES" | county=="ATTALA" | county=="WINSTON" | county=="NOXUBEE" | county=="ISSAQUENA" | county=="SHARKEY" | county=="YAZOO" | county=="MADISON" | county=="LEAKE" | county=="NESHOBA" | county=="KEMPER" 
foreach i in "log_total_output_value" "log_wage_earners_total" "log_average_wage_a" "log_hours_per_wage_earner"{
di "**************************************************************************************************"
di "Regressions for `i'"
//within estimator
di "Border counties"
qui eststo: xi: xtreg `i' st_louis_fed st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929|censusyear==1931) & border_county==1, fe vce(robust) 
}
esttab using ../../Manuscripts/Mississippi/border_plant.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931)
eststo clear

foreach i in "log_total_output_value" "log_wage_earners_total" "log_average_wage_a" "log_hours_per_wage_earner"{
di "Drop delta counties"
qui eststo: xi: xtreg `i' st_louis_fed st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929|censusyear==1931) & delta_indic==0, fe vce(robust) 
}
esttab using ../../Manuscripts/Mississippi/delta_plant.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931) 
eststo clear

//regressions where we ignore firms that are in coastal counties
gen coastal_county = ( county=="Hancock" | county=="Harrison" | county=="Jackson" )
foreach i in "log_total_output_value" "log_wage_earners_total" "log_average_wage_a" "log_hours_per_wage_earner"{
di "**************************************************************************************************"
di "Regressions for `i'"
//within estimator
qui: eststo: xi: xtreg `i' st_louis_fed st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929|censusyear==1931) & coastal_county==0, fe vce(robust) 
}
esttab using ../../Manuscripts/Mississippi/coastal_plant.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931) 
eststo clear

//distribution of effects across size quartiles

levelsof industrycode, local(industry_list)

gen q_total_output_value_1929 = 0
gen q_total_output_value_1931 = 0
gen q_total_output_value_1933 = 0
gen q_total_output_value_1935 = 0

foreach i of local industry_list{
capture xtile q_total_output_value_1929_`i' = total_output_value if industrycode=="`i'" & censusyear==1929, nquantiles(4)
capture xtile q_total_output_value_1931_`i' = total_output_value if industrycode=="`i'" & censusyear==1931, nquantiles(4)
capture xtile q_total_output_value_1933_`i' = total_output_value if industrycode=="`i'" & censusyear==1933, nquantiles(4)
capture xtile q_total_output_value_1935_`i' = total_output_value if industrycode=="`i'" & censusyear==1935, nquantiles(4)

capture replace q_total_output_value_1929_`i' = 0 if q_total_output_value_1929_`i'==.
capture replace q_total_output_value_1931_`i' = 0 if q_total_output_value_1931_`i'==.
capture replace q_total_output_value_1933_`i' = 0 if q_total_output_value_1933_`i'==.
capture replace q_total_output_value_1935_`i' = 0 if q_total_output_value_1935_`i'==.

capture replace q_total_output_value_1929 = q_total_output_value_1929 + q_total_output_value_1929_`i'
capture replace q_total_output_value_1931 = q_total_output_value_1931 + q_total_output_value_1931_`i'
capture replace q_total_output_value_1933 = q_total_output_value_1933 + q_total_output_value_1933_`i'
capture replace q_total_output_value_1935 = q_total_output_value_1935 + q_total_output_value_1935_`i'
}

//effect by size where size is relative to own industry effects
//note that this is based on size of firms in 1929
gen bottom_quartile_ind = (censusyear==1929)*(q_total_output_value_1929==1)
sort firmid censusyear
replace bottom_quartile_ind = 1 if bottom_quartile_ind[_n-1] & censusyear==1931
replace bottom_quartile_ind = . if censusyear>1931

gen second_quartile_ind =  (censusyear==1929)*(q_total_output_value_1929==2)
sort firmid censusyear
replace second_quartile_ind = 1 if second_quartile_ind[_n-1] & censusyear==1931
replace second_quartile_ind = . if censusyear>1931

gen third_quartile_ind =  (censusyear==1929)*(q_total_output_value_1929==3)
sort firmid censusyear
replace third_quartile_ind = 1 if third_quartile_ind[_n-1] & censusyear==1931
replace third_quartile_ind = . if censusyear>1931

gen fourth_quartile_ind = (censusyear==1929)*(q_total_output_value_1929==4)
sort firmid censusyear
replace fourth_quartile_ind = 1 if fourth_quartile_ind[_n-1] & censusyear==1931
replace fourth_quartile_ind = . if censusyear>1931

//regressions for table 7
qui eststo: xi: xtreg log_total_output_value i.year_1931*i.industrycode st_louis_fed_1931 if bottom_quartile_ind==1, fe vce(cluster firmid)
qui eststo: xi: xtreg log_total_output_value year_1931 i.year_1931*i.industrycode st_louis_fed_1931 if second_quartile_ind==1, fe vce(cluster firmid)
qui eststo: xi: xtreg log_total_output_value year_1931 i.year_1931*i.industrycode st_louis_fed_1931 if third_quartile_ind==1, fe vce(cluster firmid)
qui eststo: xi: xtreg log_total_output_value year_1931 i.year_1931*i.industrycode st_louis_fed_1931 if fourth_quartile_ind==1 , fe vce(cluster firmid)
esttab using ../../Manuscripts/Mississippi/size_effects.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931) 

//now do some regressions at county level
//first generate revenue weighted average log price and wage
gen counter = 1
bysort censusyear county: egen total_rev = sum(total_output_value)
gen frac_revenue = total_output_value /total_rev
local wage_cut = 2
gen init_ave_wage = frac_revenue*log_average_wage_a if p_wage<=101-`wage_cut' & p_wage>=`wage_cut'
local price_cut
gen init_ave_price = log_output_price_1*frac_revenue if p_price<=101-`wage_cut' & p_price>=`wage_cut'

collapse (sum) total_output_value wage_earners_total wage_earners_mar wage_earners_oct init_ave_wage init_ave_price counter (mean) st_louis_fed (firstnm) delta_indic, by(county censusyear)
drop if county=="??" | county==""
gen st_louis_fed_1931 = (st_louis_fed==1)*(censusyear==1931)
gen year_1931 = censusyear==1931
gen st_louis_fed_1933 = (st_louis_fed==1)*(censusyear==1933)
gen year_1933 = censusyear==1933
gen st_louis_fed_1935 = (st_louis_fed==1)*(censusyear==1935)
gen year_1935 = censusyear==1935

foreach i of varlist total_output_value wage_earners_total wage_earners_mar wage_earners_oct {
gen log_`i' = log(`i')
}

//short and long run regressions
//weight by number of firms in each county

//regressions for tables 2, 3, 4,, 5 at the county-level
foreach i of varlist log_total_output_value log_wage_earners_total log_wage_earners_mar log_wage_earners_oct init_ave_wage init_ave_price{
di "Regressions for variable `i'"
qui eststo: xi: reg `i' i.year_1931 st_louis_fed_1931 st_louis_fed i.county [aweight=counter] if (censusyear==1929|censusyear==1931), cluster(county)
}
esttab using ../../Manuscripts/Mississippi/county_SR.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear

foreach i of varlist log_total_output_value log_wage_earners_total log_wage_earners_mar log_wage_earners_oct init_ave_wage init_ave_price{
di "Regressions for variable `i'"
qui eststo: xi: reg `i' year_* st_louis_fed_* i.county [aweight=counter], cluster(county)
}
esttab using ../../Manuscripts/Mississippi/county_LR.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_*) 
eststo clear

//run some border county regressions
gen border_county = 1 if county=="WASHINGTON" | county=="HUMPHREYS" | county=="HOLMES" | county=="ATTALA" | county=="WINSTON" | county=="NOXUBEE" | county=="ISSAQUENA" | county=="SHARKEY" | county=="YAZOO" | county=="MADISON" | county=="LEAKE" | county=="NESHOBA" | county=="KEMPER" 
gen coastal_county = ( county=="Hancock" | county=="Harrison" | county=="Jackson" )

//regressions for results at county-level in Table 6.
foreach i of varlist log_total_output_value log_wage_earners_total {
di "Regressions only for border counties"
qui eststo: xi: reg `i' i.year_1931 st_louis_fed st_louis_fed_1931 i.county [aweight=counter] if (censusyear==1929|censusyear==1931) & border_count==1, cluster(county)
}
esttab using ../../Manuscripts/Mississippi/robustness_border_county.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear

//should create something like an indicator for second ring of counties from the border of the districts
foreach i of varlist log_total_output_value log_wage_earners_total {
di "Regressions excluding coastal counties"
qui eststo: xi: reg `i' i.year_1931 st_louis_fed st_louis_fed_1931 i.county [aweight=counter] if (censusyear==1929|censusyear==1931) & coastal_county==0, cluster(county)
}
esttab using ../../Manuscripts/Mississippi/robustness_noCoast_county.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear

foreach i of varlist log_total_output_value log_wage_earners_total {
di "Regressions excluding coastal counties"
qui eststo: xi: reg `i' i.year_1931 st_louis_fed st_louis_fed_1931 i.county [aweight=counter] if (censusyear==1929|censusyear==1931) & delta_indic==0, cluster(county)
}
esttab using ../../Manuscripts/Mississippi/robustness_delta_county.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear

log close
