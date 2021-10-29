********************************************************************************
*					Placebo Yatra (Random Effects Regression)
********************************************************************************

use "C:\Users\aarus\Google Drive\Communal Violence\clean_data\data_up.dta", clear

/* Tidying */
*drop if round == "nss62"
drop yatra_distance
drop if state_name == "bihar"

gen cohortn1 = (birth_year <= 1975)
gen cohortn2 = (birth_year >= 1976 & birth_year <= 1983)
gen cohortn3 = (birth_year >= 1976 & birth_year <= 1991)

*Correct weights
egen pop_group = group(dist_id muslim cohort)

bysort pop_group : egen pop = count(pop_group)
*Variables for Regression
gen log_distance = log(p_yatra_distance)

forval i = 2/6 {
	gen distance_cohort`i'_muslim = p_yatra_distance * cohort_`i' * muslim
	gen cohort`i'_muslim = cohort_`i' * muslim
	gen distance_cohort`i' = p_yatra_distance * cohort_`i'
}

gen distance_muslim = p_yatra_distance * muslim

/* Regression tables */

eststo clear
	
	forval i = 2/3 {

		xtset dist_id

		eststo: qui xtreg pri p_yatra_distance muslim distance_muslim if cohortn`i' == 1, re
	}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_p_newcohorts.tex", replace se

/* construct two year cohorts */

gen cohortb = 1 if birth_year >= 1976 & birth_year <= 1977
replace cohortb = 2 if birth_year >= 1978 & birth_year <= 1979
replace cohortb = 3 if birth_year >= 1980 & birth_year <= 1981
replace cohortb = 4 if birth_year >= 1982 & birth_year <= 1983
replace cohortb = 5 if birth_year >= 1984 & birth_year <= 1985
replace cohortb = 6 if birth_year >= 1986 & birth_year <= 1987
replace cohortb = 7 if birth_year >= 1988 & birth_year <= 1989
replace cohortb = 8 if birth_year >= 1990 & birth_year <= 1991

/* Random Effects regression */
xtset dist_id

/* Non-Hindus for each cohort as base group: Plotting in R */

/* Illiteracy and Primary Education */
foreach var in edu_level_0 pri {

	eststo clear
	
	forval i = 1/8 {

		preserve

		keep if cohortb == `i'
		xtset dist_id

		eststo: qui xtreg `var' p_yatra_distance muslim distance_muslim, re

		restore
	}

	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_`var'_p.csv", replace se nostar
	
	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_`var'_p.tex", replace se
}

/* Secondary Education */

drop if edu_level < 4
	
eststo clear
	
forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: qui xtreg sec p_yatra_distance muslim distance_muslim, re

	restore
}
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_sec_p.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_sec_p.tex", replace se


/*College Education */

keep if edu_level >= 5

eststo clear
	
forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: qui xtreg coll p_yatra_distance muslim distance_muslim, re
	
	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_col_p.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_coll_p.tex", replace se 