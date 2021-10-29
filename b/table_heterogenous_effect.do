********************************************************************************
*					Robustness Checks (Reduced Form)
********************************************************************************

use "C:\Users\aarus\Google Drive\Communal Violence\clean_data\data_sry_score.dta", clear

/* Tidying */
drop if round == "nss62"

/* Correct weights */
egen pop_group = group(dist_id muslim cohort)

bysort pop_group : egen pop = count(pop_group)

gen pop_inv = (1 / pop)

/* Variables for Regression */

gen distance_muslim = yatra_distance * muslim

gen distance_muslim_prop = yatra_distance * muslim * muslim_prop

gen distance_prop = yatra_distance * muslim_prop

gen muslim_mus_prop = muslim * muslim_prop

/* Non-Hindus for each cohort as base group: Plotting in R */

/* Illiteracy and Primary Education */
foreach var in edu_level_0 pri {

	eststo clear
	
	forval i = 1/9 {

		preserve

		keep if cohort5 == `i'
		xtset dist_id

		eststo: qui xtreg `var' yatra_distance muslim muslim_prop distance_muslim distance_prop muslim_mus_prop distance_muslim_prop, re

		restore
	}

	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_`var'_prop.csv", replace se nostar
	
	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_`var'_prop.tex", replace se
}


/* Secondary Education */

drop if edu_level < 4
	
eststo clear
	
forval i = 1/9 {

	preserve

	keep if cohort5 == `i'
	xtset dist_id

	eststo: qui xtreg sec yatra_distance muslim muslim_prop distance_muslim distance_prop muslim_mus_prop distance_muslim_prop, re

	restore
}
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_sec_prop.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_sec_prop.tex", replace se


/*College Education */

keep if edu_level >= 5

eststo clear
	
forval i = 1/9 {

	preserve

	keep if cohort5 == `i'
	xtset dist_id

	eststo: qui xtreg coll yatra_distance muslim muslim_prop distance_muslim distance_prop muslim_mus_prop distance_muslim_prop, re
	
	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_coll_prop.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_coll_prop.tex", replace se 
