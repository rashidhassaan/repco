//other tail trimming choices

capture log close
clear

//Windows path
cd "C:\Users\Nicolas\Dropbox\Depression Census\Datasets\Mississippi\Stata Data"
log using "..\Log Files\MS_other_trims.txt", t replace
use MS_data_all_years_regs.dta, replace
cd ..\
//Mac path
//log using "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Manuscripts/Mississippi/MS_preliminary_regs.txt", t replace
//use "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Datasets/Mississippi/MS_data_all_years_regs.dta", replace
//cd "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Manuscripts/Mississippi/"

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
//baseline regressions
//Table 4, panel A in online appendix
foreach i in "log_average_wage_a" {
forvalues cut_val = 0(5)5{
di "**************************************************************************************************"
di "Regressions for `i'"

if("`i'" == "log_hours_per_wage_earner"){
//minimal level of reasonableness for hours variable
replace cut = 0 if hours_per_wage_earner>84
}

if("`i'" == "log_average_wage_a"){
//play with how much of tails to trim 
local wage_cut = `cut_val'
replace cut = 0 if p_wage>=101-`wage_cut' | p_wage<=`wage_cut'
}

tab cut
//within estimator
qui eststo: xi: xtreg `i' st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929|censusyear==1931) & cut, fe vce(robust) 
//balanced panel, no firm fixed effects
qui eststo: xi: reg `i' st_louis_fed_1931 st_louis_fed i.year_1931*i.industrycode if balanced_1931 & cut, cluster(firmid)
//unbalanced panel
qui eststo: xi: reg `i' st_louis_fed_1931 st_louis_fed i.year_1931*i.industrycode if (censusyear==1929 | censusyear==1931) & cut, cluster(firmid) 
esttab using ../../Manuscripts/Mississippi/table_basic_wage_`cut_val'.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear
}
}

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

//Table 4, panel B in online appendix
foreach i in "log_output_price_1" {
forvalues cut_val = 0(5)5{

di "**************************************************************************************************"
di "Regressions for variable `i'"

if("`i'" == "log_output_price_1"){
//play with how much of tails to trim 
local price_cut = `cut_val'
replace cut = 0 if p_price>=101-`price_cut' | p_price<=`price_cut'
}
qui eststo: xi: xtreg `i' i.year_1931*i.industrycode st_louis_fed_1931 i.year_1931*i.industrycode if (censusyear==1929|censusyear==1931) & num_products==1 & cut, fe vce(robust)
//balanced panel, no firm effects
qui eststo: xi: reg `i' st_louis_fed_1931 st_louis_fed i.year_1931*i.industrycode if balanced_1931 & num_products==1 & cut, cluster(firmid)
//unbalanced panel
qui eststo: xi: reg `i'  st_louis_fed_1931 st_louis_fed i.year_1931*i.industrycode i.county if (censusyear==1929 | censusyear==1931)  & num_products==1 & cut, cluster(firmid)
esttab using ../../Manuscripts/Mississippi/regs_output_price_`cut_val'.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear
}
}

log close
