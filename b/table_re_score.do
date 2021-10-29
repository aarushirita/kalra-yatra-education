********************************************************************************
*			 Random Effects Regression with Propensity Scores
********************************************************************************

use "C:\Users\aarus\Google Drive\Communal Violence\clean_data\data_sry_score.dta", clear

/* Tidying */
drop if round == "nss62"

/* Creating variables for regression */
gen log_distance = log(yatra_distance)
*gen rec_score = (1/score)

*creating triple differences
forval i = 2/6 {
	gen distance_cohort`i'_muslim = yatra_distance * cohort_`i' * muslim
	gen cohort`i'_muslim = cohort_`i' * muslim
	gen distance_cohort`i' = yatra_distance * cohort_`i'
}

gen distance_muslim = yatra_distance * muslim

/* construct two year cohorts */

gen cohortb = 1 if birth_year >= 1976 & birth_year <= 1977
replace cohortb = 2 if birth_year >= 1978 & birth_year <= 1979
replace cohortb = 3 if birth_year >= 1980 & birth_year <= 1981
replace cohortb = 4 if birth_year >= 1982 & birth_year <= 1983
replace cohortb = 5 if birth_year >= 1984 & birth_year <= 1985
replace cohortb = 6 if birth_year >= 1986 & birth_year <= 1987
replace cohortb = 7 if birth_year >= 1988 & birth_year <= 1989
replace cohortb = 8 if birth_year >= 1990 & birth_year <= 1991

/* weighting random effects regressions with Stabilized IPW*/
eststo clear

/* Illiteracy and Primary Education */
foreach var in edu_level_0 pri {

	eststo clear
	
	forval i = 1/8 {

		preserve

		keep if cohortb == `i'
		xtset dist_id

		eststo: xtregre2 `var' yatra_distance muslim distance_muslim [aweight = ipw_s]

		restore
		
		esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_score_`var'_`i'.tex", replace se
	}

	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_score_`var'.csv", replace se nostar
	
}

/* Secondary Education */

drop if edu_level < 4
	
eststo clear
	
forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: xtregre2 sec yatra_distance muslim distance_muslim [aweight = ipw_s]

	restore
	
	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_score_sec_`i'.tex", replace se
}
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_score_sec.csv", replace se nostar


/*College Education */

keep if edu_level >= 5

eststo clear
	
forval i = 1/8 {

	preserve

	keep if cohortb == `i'
	xtset dist_id

	eststo: xtregre2 coll yatra_distance muslim distance_muslim [aweight = ipw_s]
	
	restore
	
	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_score_coll_`i'.tex", replace se

}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_re_score_coll.csv", replace se nostar