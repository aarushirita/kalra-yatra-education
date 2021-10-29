********************************************************************************
*						OLS Segregation and Yatra
********************************************************************************

use "C:\Users\aarus\Google Drive\Communal Violence\clean_data\data_sry.dta", clear

/* Keep distinct district level observations only */
keep dist_id dis_index yatra_distance state_name pc11_pca_tot_p_u muslim_prop
duplicates drop

gen log_yatra = log(yatra_distance)

eststo clear
/* Without Controls */
eststo: reg dis_index yatra_distance, robust 

/* With Population Controls */
eststo: reg dis_index yatra_distance pc11_pca_tot_p_u, robust

/* Controlling for Proportion of Muslim Enterprises */
eststo: reg dis_index yatra_distance pc11_pca_tot_p_u muslim_prop, robust

/* Regressing on log(Yatra) */
eststo: reg dis_index log_yatra, robust

esttab using "C:\Users\aarus\Google Drive\communal_violence\a\table_disindex_yatra.tex", replace se scalar(F)

