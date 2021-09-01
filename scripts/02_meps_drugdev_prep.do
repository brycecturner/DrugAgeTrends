
cd "/Users/bturner/Dropbox (Harvard University)/Drug_Age_Trends/submission_files/"

*MEPS Data 
* use "analysis_data/duperid_ccc_1996_to_2015.dta", clear

********
** Clean Drug Dev Data
********
use "prop_data/drug_candidates_indications_progression.dta", clear

  rename Indications icd9_code

  tostring icd9_code, gen(icd9_str_short)
  replace icd9_str_short = substr(icd9_str, 1, 3)
  replace icd9_str_short = string(icd9_code,"%03.0f")

  order icd9*

********
** Mapping icd9-ccc Merge
******** 
  
  merge m:1 icd9_str_short using "raw_data/icd9_ccc_mapping.dta"

  order icd9* indicationname ccs*
  sort icd9_code


********
** Manually Map Missing Codes
********  
	
  replace ccs_code = "" if _merge == 1 	
	
  replace ccs_code = "7" if icd9_code == 59 & _merge == 1 // Moving POX into "Other viral infections"
  
  replace ccs_code = "13" if icd9_code == 209  & _merge == 1 & indicationname == "Stomach tumor" // Moving Stomach Tumor to Stomach Cancer
  replace ccs_code = "51" if icd9_code == 209  & _merge == 1 & indicationname == "Neuroendocrine tumor" // Moving neuroendocrine tumor into "Other Endo Disorders"
  replace ccs_code = "51" if icd9_code == 209  & _merge == 1 & indicationname == "Endocrine tumor" // Moving endocrine tumor into "Other Endo Disorders"
  replace ccs_code = "19" if icd9_code == 209  & _merge == 1 & indicationname == "Solid tumor" // Moving the generic solid tumor into lung cancer becasue it is teh most common solid cancer and we don't have ANY of them in the data
    
  replace ccs_code = "84" if icd9_code == 339  & _merge == 1  & indicationname == "Cluster headache" // Cluster headache into Headache/mig
  replace ccs_code = "95" if icd9_code == 339  & _merge == 1  & indicationname == "Guillain Barre suyndrome" //  Guillain Barre into "Other Nerve dx"
  
  replace ccs_code = "126" if icd9_code == 1094 & _merge == 1 // Otorhinolaryngological disease into Other Up Resp infection
  replace ccs_code = "135" if icd9_code == 2002 & _merge == 1 // Typhoid fever into "Intest infct"
  replace ccs_code = "10" if icd9_code == 2006 & _merge == 1 // Immunization into immunizaiton/screen
  replace ccs_code = "10" if icd9_code == 2007 & _merge == 1 // Prophylaxis into immunizaiton/screen
  replace ccs_code = "3" if icd9_code == 2009 & _merge == 1 // moving very specifica staph infectino ot bacterial infectino other
  replace ccs_code = "72" if icd9_code == 2011 & _merge == 1 // Moving PTSD into anxiety dx
  
  replace ccs_code = "7" if icd9_code == 2012 & _merge == 1 & indicationname == "Papillomavirus infection" // Papillomavirus infection into VIral Infection
  replace ccs_code = "139" if icd9_code == 2012 & _merge == 1 & indicationname == "Peptic ulcer" // Peptic Ulcer into gasduo ulcer
  replace ccs_code = "8" if icd9_code == 2012 & _merge == 1 & indicationname == "Parasitic infection" // Papillomavirus infection into VIral Infection
  
  replace ccs_code = "211" if icd9_code == 2013 & _merge == 1 // Polymyalgia rheumatica and Musculoskeletal disease into oth conn tiss
  
  *replace ccs_code == "" if icd9_code == 2017 & _merge == 1  // Tay Sachs Disease is a rare genetic disease and there aren't many instances of it in the sample
  *replace ccs_code == "" if icd9_code == 2018 & _merge == 1  // rare metabolic disorder and only 1 instance, so not bothering to map
  replace ccs_code = "218" if icd9_code == 2024 & _merge == 1 // parturition is an esoteric word for "borth", so mapping into the "liveborn" ccs code
 
  replace ccs_code = "176" if icd9_code == 2025 & _merge == 1 // Contraceptives into contraceptives
 
  replace ccs_code = "212" if icd9_code == 2042 & _merge == 1 & indicationname == "Bone marrow transplantation" // Bone marrow transplant to ot bone dx
  replace ccs_code = "158" if icd9_code == 2042 & _merge == 1 & indicationname == "Kidney transplantation" // Kidney transplant to chr ren fail
  replace ccs_code = "91" if icd9_code == 2042 & _merge == 1 & indicationname == "Corneal transplantation" // Corneal transplant to other eye dx
  replace ccs_code = "259" if icd9_code == 2042 & _merge == 1 & indicationname == "Organ transplantation" // Other organ transplant to Unclassified (Oth Organ Transplant)
 
  replace ccs_code = "158" if icd9_code == 2045 & _merge == 1 & indicationname == "Kidney dialysis" // Kidney dialysis to chr ren fail
  replace ccs_code = "16" if icd9_code == 2045 & _merge == 1 & indicationname == "Biliary tumor" // Biliary tumor to Liver/ibd ca
  
  replace ccs_code = "74" if icd9_code == 2062 & _merge == 1 // Suicidal ideation to ot mentl cnd
  *replace ccs_code = "" if icd9_code == 2064 & _merge == 1 // "Surgical Procedure", so not helpful and not very many of them
  *replace ccs_code = "" if icd9_code == 2072 & _merge == 1 // "Surgical Procedure", so not helpful and not very many of them
  *replace ccs_code = "" if icd9_code == 2084 & _merge == 1 // Rare genetic disorder, and only 1 case

  drop if ccs_code == ""
  
********
** Clean Up Merge
********  
  drop if _merge == 2
  drop _merge
  
  rename ccs_code CCC_value
  
********
** Wrangling
******** 
   
  local date_vars dev_date_earliest dev_date_earliest_p1 dev_date_earliest_p2 dev_date_earliest_p3 dev_date_earliest_launch dev_date_last earliest_trial_start_ik latest_trial_end_ik latest_trial_start_ik_phase earliest_trial_start_i latest_trial_end_i latest_trial_start_i_phase earliest_trial_start_1 latest_trial_end_1 earliest_trial_start_2 latest_trial_end_2 earliest_trial_start_3 latest_trial_end_3 latest_trial_end_termsusp 
  
  
  foreach var of varlist `date_vars' {
  	format `var' %tq
	
	di(`var')
  	tostring `var', replace u force
	
  }
  
  
  save "analysis_data/mapping_prewrangle.dta", replace
 
