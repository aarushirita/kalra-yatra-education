********************************************************************************
*				Random Effects Regression for SCs (Robustness Checks)
********************************************************************************

use "C:\Users\aarus\Google Drive\Communal Violence\clean_data\data_sry_sc.dta", clear

/* Tidying */
drop if round == "nss62"

/* Variables for Regression */

gen log_distance = log(yatra_distance)

forval i = 1/5 {
	gen distance_cohort`i'_sc = yatra_distance * cohort_`i' * sc
	gen cohort`i'_muslim = cohort_`i' * muslim
	gen distance_cohort`i' = yatra_distance * cohort_`i'
}

gen distance_sc = yatra_distance * sc

/* Non-SCs for each cohort as base group: Plotting in R */

/* Illiteracy and Primary Education */
foreach var in edu_level_0 pri {

	eststo clear
	
	forval i = 1/9 {

		preserve

		keep if cohort5 == `i'
		xtset dist_id

		eststo: qui xtreg `var' yatra_distance sc distance_sc, re

		restore
	}

	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_`var'_sc.csv", replace se nostar
	
	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_`var'_sc.tex", replace se
}


/* Secondary Education */

drop if edu_level < 4
	
eststo clear
	
forval i = 1/9 {

	preserve

	keep if cohort5 == `i'
	xtset dist_id

	eststo: qui xtreg sec yatra_distance sc distance_sc, re

	restore
}
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_sec_sc.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_sec_sc.tex", replace se


/*College Education */

keep if edu_level >= 5

eststo clear
	
forval i = 1/9 {

	preserve

	keep if cohort5 == `i'
	xtset dist_id

	eststo: qui xtreg coll yatra_distance sc distance_sc, re
	
	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_coll_sc.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_coll_sc.tex", replace se 
