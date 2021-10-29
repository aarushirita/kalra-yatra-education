********************************************************************************
*						OLS Segregation and Education
********************************************************************************

use "C:\Users\aarus\Google Drive\Communal Violence\clean_data\data_sry.dta", clear

*Relevant Social Groups
drop if social_group == "0"
drop if social_group == "1"
drop if social_group == "2"

drop if round == "nss62"

*Correct weights
egen pop_group = group(dist_id muslim cohort)

bysort pop_group : egen pop = count(pop_group)

*Variables for OLS between education and dissimilarity
forval i = 2/5 {
	gen disindex_cohort`i'_muslim = dis_index * cohort_`i' * muslim
	gen cohort`i'_muslim = cohort_`i' * muslim
	gen disindex_cohort`i' = dis_index * cohort_`i'
}

gen disindex_muslim = dis_index * muslim

********************************************************************************
*		Table 5: Linear Probability Model (Structural Relationship)
********************************************************************************
xtset dist_id 

/* Non-Hindus for each cohort as base group: Plotting in R */

/* Illiteracy and Primary Education */
foreach var in edu_level_0 pri {

	eststo clear
	
	forval i = 1/9 {

		preserve

		keep if cohort5 == `i'
		xtset dist_id

		eststo: xtreg `var' dis_index muslim disindex_muslim pc11_pca_tot_p_u muslim_prop, re

		restore
	}

	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_lpm_`var'.csv", replace se nostar
	
	esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_re_`var'.tex", replace se
}


/* Secondary Education */

drop if edu_level < 3

eststo clear
	
forval i = 1/9 {

	preserve

	keep if cohort5 == `i'
	xtset dist_id

	eststo: xtreg sec dis_index muslim disindex_muslim, re

	restore
}
	
esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_lpm_sec.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_lpm_sec.tex", replace se


/*College Education */

drop if edu_level < 5

eststo clear
	
forval i = 1/9 {

	preserve

	keep if cohort5 == `i'
	xtset dist_id

	eststo: xtreg coll dis_index muslim disindex_muslim, re
	
	restore
}

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_lpm_coll.csv", replace se nostar

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\tab_lpm_col.tex", replace se