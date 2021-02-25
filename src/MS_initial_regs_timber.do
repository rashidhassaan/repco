//regressions using timber data

capture log close
clear

//Windows path
cd "C:\Users\Nicolas\Dropbox\Depression Census\Datasets\Mississippi\Stata Data"
log using "..\Log Files\MS_preliminary_regs.txt", t replace
use MS_data_all_years_regs_timber.dta, replace
cd ..\
//Mac path
//log using "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Manuscripts/Mississippi/MS_preliminary_regs.txt", t replace
//use "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Datasets/Mississippi/MS_data_all_years_regs.dta", replace
//cd "/Users/nicolaslehmannziebarth/Dropbox/Depression Census/Manuscripts/Mississippi/"

//compare firms in 1929 between the two regions
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
//baseline regressions
//1st set of regressions in online appendix, table 2
//table 3
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
esttab using ../../Manuscripts/Mississippi/table_basic_`i'_timber.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear
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


//2nd and 3rd sets of regressions in online appendi, table 2
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
esttab using ../../Manuscripts/Mississippi/regs_`i'_timber.tex, star(* 0.10 ** 0.05 *** 0.01) ar2 se label replace tex keep(st_louis_fed_1931 st_louis_fed) 
eststo clear
}


//entry and exit probits
//this really isn't a DD because don't have pre-treatment year. 1929 really is the treatment phase.
//note, however, that to a certain extent persistent differences across regions in exit rates must be matched by differences in entry rates
xi: probit exit st_louis_fed log_total_output_value i.industrycode if censusyear==1929,  vce(robust)
mfx, varlist(st_louis_fed)
xi: probit exit st_louis_fed i.industrycode if censusyear==1929,  vce(robust)
mfx, varlist(st_louis_fed)

sort firmid censusyear
//generate an age variable. Everyone gets same age in 1929
by firmid: gen age = _n
gen st_louis_fed_1929 = (censusyear==1929)*(st_louis_fed==1)

//table 1 in online appendix
xi: probit exit log_total_output_value i.age st_louis_fed_1929 st_louis_fed_1931 st_louis_fed_1933 i.year_1931*i.industrycode year_1933 i.industrycode if censusyear<1935, vce(robust)
mfx, varlist(st_louis_fed_1929 st_louis_fed_1931 st_louis_fed_1933)
xi: probit enter st_louis_fed_1931 st_louis_fed_1933 st_louis_fed_1935 i.year_1931*i.industrycode year_1933 i.industrycode if censusyear>1929, vce(robust)
mfx, varlist(st_louis_fed_1931 st_louis_fed_1933 st_louis_fed_1935)


log close
