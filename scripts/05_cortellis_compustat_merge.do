*Cortellis - Compustat Merge

cd "~/Dropbox (Harvard University)/Drug_Age_Trends/submission_files"


  *has both individual and bucket-level data
  use "analysis_data/MEPS_analysis_clean.dta", clear

  keep CCC_value age_buckets YEAR RXXPYYX
  collapse RXXPYYX, by(CCC_value age_buckets YEAR)
  
 
  
  reshape wide RXXPYYX, i(CCC_value YEAR) j(age_buckets) 
  
  egen total_expenditure = rowtotal(RX*)
 
  gen share_0 = RXXPYYX0 / total_expenditure
  gen share_25 = RXXPYYX25 / total_expenditure
  gen share_45 = RXXPYYX45 / total_expenditure
  gen share_65 = RXXPYYX65 / total_expenditure
  
  foreach i of varlist share_*{
  	replace `i' = 0 if `i' == .
  }
    *Data now unique at the YEAR - CCC - age_bucket level

  rename YEAR year
  
  drop RX* total*
  
  drop if CCC_value == "-1"
  drop if CCC_value == "-8"
  drop if CCC_value == "-9"
  
********
** Drug Data
******** 
  merge 1:m CCC_value year using "analysis_data/drug_lv_drugdev_status.dta"

  
  keep if _merge == 3
  drop _merge
  
* drug-level data

*use "analysis_data/drug_lv_drugdev_status.dta", clear
  *drug_id
  *CCC_value
  
  gen dev_quarter = quarterly(date, "YQ")
  format dev_quarter %tq
  drop date
  destring drugid, replace
  destring icd9_code, g(Indications)

*merge in coretellis info with the drug id, to connect to on quarterly basis
*
merge 1:m drugid dev_quarter Indications using "prop_data/company_and_orig_ikct.dta"
  *drugid
  *dev_quarter
  *companyid
  
  keep if _merge == 3
  drop _merge
  tostring companyid, replace
  
merge m:1 drugid Indications using "prop_data/disc_qtr_date_drugind.dta"  
  drop if _merge == 2
  drop _merge
 

  *checking that drug-date was assigned a value in the cortellis data
gen status_test =  pretrial + trials + launched + failed

gen proj_status = ""  
  replace proj_status = "active" if drug_phase == "Clinical"
  replace proj_status = "active" if drug_phase == "Discovery"
  replace proj_status = "active" if drug_phase == "Phase 1"
  replace proj_status = "active" if drug_phase == "Phase 2"
  replace proj_status = "active" if drug_phase == "Phase 3"
  replace proj_status = "active" if drug_phase == "Pre"
  replace proj_status = "active" if drug_phase == "Pre-registration"
  replace proj_status = "active" if drug_phase == "Registered"
  
  replace proj_status = "launched" if drug_phase == "Launched"
  
  replace proj_status = "disc" if discontinued_qtr <= dev_quarter
  replace proj_status = "disc" if drug_phase == "No Development Reported"
  replace proj_status = "disc" if drug_phase == "Discontinued"
  replace proj_status = "disc" if drug_phase == "Suspended"
  replace proj_status = "disc" if drug_phase == "Withdrawn"

  drop if status_test == 0 & proj_status == ""
  
  drop status_test
  
drop if companyid == "."

keep if proj_status ==  "active"  

*count number of TRIALS a company is going through in a quarter
bysort companyid dev_quarter: gen current_trials_tmp = _N 
bysort companyid dev_quarter: egen current_CCCs = max(current_trials_tmp) 
  replace current_CCCs = 0 if current_CCCs == .  
  
*need to account for drugs undergoing trials for multiple CCCs
duplicates tag companyid dev_quarter drugid, g(multi_cccs)
bysort companyid dev_quarter drugid: replace multi_cccs = 0 if _n != _N
bysort companyid dev_quarter: egen qtr_dups = sum(multi_cccs) 

*Number of Active drug projets = total number of trials minus the number of times a drug is undergoing a trial for multiple cccs
gen current_drugs = current_CCCs - qtr_dups  
  replace current_drugs = 0 if current_drugs == .
  drop qtr_dups multi_cccs
  
order current_drugs companyid dev_quarter drugid
sort companyid dev_quarter drugid

*Number of Drugs in Each Age Group
bysort companyid dev_quarter CCC_value: gen current_CCC_trials = _N 

foreach age of numlist 0 25 45 65 {
	bysort companyid dev_quarter: egen count_`age' = sum(share_`age')
}


/**Total Projects Ever
   *find first-ranked id and set to 1
bysort companyid drugid (dev_quarter): gen cumulative_projects = (_n == 1)
  *sums all the "first-ranked" ids -- equivalent to summing unique ids
bysort companyid (dev_quarter drugid): replace cumulative_projects = sum(cumulative_projects)

by companyid dev_quarter: replace cumulative_projects = cumulative_projects[_N]
*/

*merge in compustat company id and name
merge m:1 companyid using "prop_data/companyid_public_crswlk (3).dta"
  keep if _merge ==3
  drop _merge


collapse (mean) share* ///
         (first) count_* ///
         (first) current_drugs cortellis_company ///
		         current_CCCs conm companyid, ///
		 by(gvkey dev_quarter)

		 
tostring dev_quarter, replace force u
gen year = substr(dev_quarter, 1, 4)
destring year, replace
drop dev_quarter
		 
order gvkey conm companyid cortellis_company year current* share*
sort gvkey year		 
  
save "analysis_data/compustat_portfolio_shares_and_counts.dta", replace 

  
