********************************************************************************
*					Random Effects Regression (IV)
********************************************************************************

use "C:\Users\aarus\Google Drive\Communal Violence\clean_data\data_sry_score.dta", clear

/* Tidying */
drop if round == "nss62"

/* Variables for Regression */
gen distance_muslim = yatra_distance * muslim
gen disindex_muslim = dis_index * muslim

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

		eststo: qui xtivreg `var' muslim yatra_distance (disindex_muslim = distance_muslim muslim yatra_distance), re

		restore
	}

	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_iv_`var'.csv", replace se nostar
	
	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_iv_`var'.tex", replace se
}


/* Secondary Education */

drop if edu_level < 4
	
eststo clear
	
forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: qui xtivreg sec muslim yatra_distance (disindex_muslim = distance_muslim), re

	restore
}
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_iv_sec.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_iv_sec.tex", replace se


/*College Education */

keep if edu_level >= 5

eststo clear
	
forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: qui xtivreg coll muslim yatra_distance (disindex_muslim = distance_muslim), re
	
	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_iv_coll.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_iv_coll.tex", replace se 
