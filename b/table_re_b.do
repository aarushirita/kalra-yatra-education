********************************************************************************
*					Random Effects Regression (1976-90)
********************************************************************************

use "C:\Users\aarus\Google Drive\Communal Violence\clean_data\data_sry.dta", clear

/* Tidying */
drop if round == "nss62"
drop if social_group == "0"
drop if social_group == "1"
drop if social_group == "2"

/* Variables for Regression */
gen distance_muslim = yatra_distance * muslim

/* Non-Hindus for each cohort as base group: Plotting in R */

/* construct two year cohorts */

gen cohortb = 1 if birth_year >= 1976 & birth_year <= 1977
replace cohortb = 2 if birth_year >= 1978 & birth_year <= 1979
replace cohortb = 3 if birth_year >= 1980 & birth_year <= 1981
replace cohortb = 4 if birth_year >= 1982 & birth_year <= 1983
replace cohortb = 5 if birth_year >= 1984 & birth_year <= 1985
replace cohortb = 6 if birth_year >= 1986 & birth_year <= 1987
replace cohortb = 7 if birth_year >= 1988 & birth_year <= 1989
replace cohortb = 8 if birth_year >= 1990 & birth_year <= 1991


/* Illiteracy Rates */ 
eststo clear
	
forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: qui xtreg edu_level_0 yatra_distance muslim distance_muslim, re

	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_edu_level_0y.csv", replace se nostar
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_edu_level_0y.tex", replace se

/* Primary Education */
eststo clear
	
forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: qui xtreg pri yatra_distance muslim distance_muslim, re

	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_priy.csv", replace se nostar
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_priy.tex", replace se

/* Secondary Education */

drop if edu_level < 3
	
eststo clear

forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: qui xtreg sec yatra_distance muslim distance_muslim, re

	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_secy.csv", replace se nostar
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_secy.tex", replace se

/*College Education */

keep if edu_level >= 5

eststo clear

forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: qui xtreg coll yatra_distance muslim distance_muslim, re

	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_colly.csv", replace se nostar
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_colly.tex", replace se