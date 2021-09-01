

cd "~/Dropbox (Harvard University)/Drug_Age_Trends/submission_files/"

********
** Clean CPI Data
********
	*CPI Index from St. Louis Federal Reserve Econiomic Data
  import delimited "raw_data/CPIAUCSL.csv", clear

  rename cpiaucsl_nbd20160101 cpi_index
  label var cpi_index "CPI Index, 2015 == 100"

  
  gen date_yr = substr(date, 1, 4)
  destring date_yr, gen(YEAR)
	
  * FRED reports their values at the start of the period (annoying), so the 2015 value is 2015-01-01. As an approximate, we use this value for the end of the previous year's value.  (one day difference)
  replace YEAR = YEAR - 1
  
  gen cpi_deflator = 100 / cpi_index
  label var cpi_deflator "2015 CPI / Current Year CPI"
 
  ** Potentially More coding Fixes Here
  ** Weird date formatting, the date is in the format YYYY-01-01
	** sent email to FRED asking how to interpet this...
  
  keep cpi_index YEAR cpi_deflator
  
  save "analysis_data/cpi_cleaned.dta", replace
  
  
********
** Read In MEPS Data
********
use "analysis_data/duperid_ccc_1996_to_2015.dta", clear
  * Observations are unique on the DUPERSID - CCC_value - YEAR Level

{/* Notes about MEPS dataset

 The complete MEPS dataset
 Observations are unique on the DUPERSID - CCC_value - YEAR Level
	Note: This is a collapsed version of the "raw" MEPS data, which appears on the DUPERSID-medicine_event-YEAR level

 CCC_value contains the CCCs we use.

*/
}


********
** Inflation Adjusting
********

  merge m:1 YEAR using "analysis_data/cpi_cleaned.dta"

  keep if _merge == 3
  drop _merge

  foreach var of  varlist RX*YX{
  	
	*di("`var'") // used this line to check that the regular expression in the loop was working
	
	replace `var' = `var' * cpi_deflator
	
  }
 
  
********
** Generate Age Buckets and Expenditures
********

  gen age = YEAR - DOBYY
  label variable age "Age at Time of Survey (Years)"
		* The DOBYY variable has 2 hard cutoffs. 
		* Thorugh the 2000 survey, the minimum DOBYY was 90 years ago
		* From 2001 onwards, the minimum DOBYY was 85 years ago
  
    
  *"cut" gives the "left" side of the range.  i.e. a 31 year old is coded as 30 and a 29 year old is coded as "20", etc.
  egen age_buckets = cut(age), ///
	at(0, 25, 45, 65, 100)
		
  label variable age_buckets "Lower Value of Age Bucket"
  
  
  bysort age_buckets YEAR: egen bucket_expenditures = sum(RXXPYYX)
  *bysort bucket_expenditures: gen unique_bucket_expenditures = _N
  separate bucket_expenditures, by(age_buckets) g("bucket_expenditures_")

********
** FIGURE 2A
** Histogram of PER CAPITA EXPENDITURES (are older people spending more per person)
********

  bysort age_buckets YEAR: egen bucket_pop = nvals(DUPERSID)
  gen cost_per_capita = bucket_expenditures / bucket_pop
  separate cost_per_capita, by(age_buckets) g("cost_per_capita_")
  
  
  label var cost_per_capita_0 "0 to 24"
  label var cost_per_capita_25 "25 to 44"
  label var cost_per_capita_45 "45 to 64"
  label var cost_per_capita_65 "65+"
  
  preserve 
    duplicates drop cost_per_capita_* YEAR, force
	
    graph twoway (line cost_per_capita_* YEAR, ///
	              lpattern( dot shortdash longdash_dot solid) ///
				  lcolor("black" "black" "black" "black") ///
				  lwidth(medthick)), ///
	  legend(`legend_opts') ///
	  graphregion(fcolor(white)) ///
	  xlabel(1996(2)2015) title("Per Capita Expenditures, Weighted")
	  
    graph export "per_cap_expenditures_by_bucket.pdf" , replace
  restore  
  

********	
** Share of a Given CCC
********

 preserve 
  bysort CCC_value YEAR: egen CCC_expenditure = sum(RXXPYYX)
  bysort CCC_value YEAR: egen CCC_perscripts = count(RXXPYYX)
  bysort CCC_value age_bucket YEAR: egen CCC_age_expenditure = sum(RXXPYYX)
  gen age_bucket_share_of_CCC = CCC_age_expenditure / CCC_expenditure
  
 
  keep age_buckets YEAR bucket* cost* CCC*
  duplicates drop
    *Data now unique at the YEAR - CCC - age_bucket level

  rename YEAR year
  rename age_buckets age_bucket
********
** Drug Data
******** 
  merge m:1 CCC_value year using "analysis_data/CCC_lv_drugdev_status.dta"
  
  
  keep if _merge == 3  
  
********
** FIGURE 4
** New Drugs in Phase 1 Trials by Age Buckets
******** 
  
 
  bysort CCC_value year: egen total_pop_tmp = sum(bucket_pop)
  bysort year: egen total_pop = max(total_pop_tmp)
  bysort year: gen elderly_pop_share_tmp = bucket_pop / total_pop if age_bucket == 65
  bysort year: egen over_65_pop_share = max(elderly_pop_share_tmp)
  drop *tmp
  
  
  gen annual_bucket_share_65_tmp = CCC_age_expenditure / CCC_expenditure if age_bucket == 65
  
  
  bysort CCC_value year: egen annual_bucket_share_65 = max(annual_bucket_share_65_tmp)
  drop annual_bucket_share_65_tmp

  local age 65
  local final_var new_phase1
  
    gen high_age_CCC = (annual_bucket_share_`age' > over_`age'_pop_share)
	
	collapse (sum) `final_var' ,by(high_age_CCC year) 

  separate `final_var', by(high_age_CCC) g("`final_var'_")

  gen ln_`final_var'_0 = ln(`final_var'_0 + 1)
  gen ln_`final_var'_1 = ln(`final_var'_1 + 1)
	  
  label var `final_var'_0 "Non-Elderly"
  label var `final_var'_1 "Elderly"

  label var ln_`final_var'_0 "65+ Expense Share < 65+ Population Share"
  label var ln_`final_var'_1 "65+ Expense Share > 65+ Population Share"	  
	  
	  
  drop if year > 2013
    
  local title "Drug Innovaiton: Flow of Drugs into Phase 1 Trials"
	  
  graph twoway (line ln_`final_var'_* year, ///
	            lwidth(medthick) ///
			    lpattern(shortdash solid) ///
				lcolor("black" "black")) , ///
		 legend(col(1)) ///
		 xlabel(1996(2)2013)
		 
		 
		 
  graph export "innov_`age'_pop_share_`final_var'_ln.pdf", replace
	  
 restore
 

 save "analysis_data/MEPS_analysis_clean.dta", replace	 

 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
