## Git Repo whence I first learned about the MEPS package
#install.packages("devtools")
#install_github("e-mitchell/meps_r_pkg/MEPS")

#install.packages("httr")
#install.packages("RCurl")
#install.packages("pryr")
#install.packages("data.table")
#install.packages("ggthemes")

rm(list = ls())

#start_time <- Sys.time()

library(pryr)
library(devtools)
library(MEPS)
library(tidyverse)
#library(ggthemes)
library(httr)
library(data.table)
library(Hmisc)
library(haven)



# Directory Options -------------------------------------------------------
base_dir <- "~/Dropbox (Harvard University)/Drug_Age_Trends/submission_files/"
work_dir <- file.path(base_dir)

setwd(work_dir)


# Extract File Names ------------------------------------------------------
# the MEPS github has a document showing the name of each file by type & year
# will use this data frame as a key when compiling data
MEPS_file_names <- function(){
  
  git_url <- "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"
  dest_path <- "/Users/bturner/Dropbox (Harvard University)/Drug_Age_Trends/Input_Data/MEPs/MEPS_jkcopy/meps_file_names.csv"
  
  download.file(url = git_url,
                destfile = dest_path)


  file_names <- read_csv(dest_path) %>%
    filter(!is.na(FYC))

  colnames <- colnames(file_names) %>% tolower(.) %>% gsub(" ", "_", .)
  colnames(file_names) <- colnames
  
  return(file_names)
}
# End: File Names ---------------------------------------------------------


# MEPS Annual Data --------------------------------------------------------
MEPS_annual <- function(year){
  
  fyc_file <- file_names$fyc[file_names$year == year]
  drug_file <- file_names$pmed_events[file_names$year == year]

  # Weight Variable changes names
  if(year <= 1998){
    weight_var <- paste0("WTDPER", substr(year, 3,4))
  } else {
    weight_var <- paste0("PERWT", substr(year, 3,4), "F")
  }

  fyc_vars <- c("PERSON ID (DUID + PID)" =  "DUPERSID",
                  "DATE OF BIRTH: YEAR" = "DOBYY",
		  "PERSON WEIGHT" = weight_var)
  
  fyc_data <- as_tibble(read_MEPS(file = fyc_file)) %>%
    select(unname(fyc_vars)) 
  
  ## Cleaning Drug Data Set
    #The payment var has the date encoded in it
    generic_payment_vars <- c("self_paid" = "RXSFYYX",
                              "medicare_paid" = "RXMRYYX",
                              "medicaid_paid" = "RXMDYYX",
                              "prv_ins_paid" = "RXPVYYX",
                              "vetadm_paid" = "RXVAYYX",
                              #"tricare_paid" = "RXTRYYX",
                              "oth_fed_paid" =  "RXOFYYX",
                              "stl_gov_paid" = "RXSLYYX",
                              "wrkr_comp_paid" = "RXWCYYX",
                              "oth_ins_paid" = "RXOTYYX", 
                              "oth_prv_paid" = "RXORYYX",
                              "oth_pub_paid" = "RXOUYYX",
                              "total_paid" = "RXXPYYX")
    
    drug_payment_vars <- gsub("YY", substr(year, 3,4), unname(generic_payment_vars)) 
    names(drug_payment_vars) <- names(generic_payment_vars)
    
    
    drug_id_vars <- c("PERSON ID (DUID + PID)" =  "DUPERSID", 
                      "UNIQUE PRESCRIPTION EVENT ID" = "RXRECIDX",
                      "CLININAL CLASSIFICATION CODE 1" = "RXCCC1X", 
                      "CLININAL CLASSIFICATION CODE 1" = "RXCCC2X", 
                      "CLININAL CLASSIFICATION CODE 1" = "RXCCC3X")
  
    
    
    drug_data <- as_tibble(read_MEPS(file = drug_file)) %>%
      #sample_n(size = 200) %>%
      select(all_of(c(unname(drug_id_vars), unname(drug_payment_vars)) ) ) %>%
      
      #Splitting costs when there are more than 1 CCC assigned to a drug
      pivot_longer(cols = starts_with("RXCCC"),
                   names_to = "CCC_type",
                   values_to = "CCC_value") %>%
      
      filter(!CCC_value %in% c("-1", "-9") ) %>%
      
      group_by(DUPERSID, RXRECIDX) %>% 
      #counts the number of duplicates in terms of person & medicine event ( buying a single prescription)
      #If a drug is coded to treat multiple CCCs...
      #... we want to split the expenditures evenly across those different CCCs
      #if the drug only has 1 CCC code, then replicate will show up as 1, 2 CCC codes = 2, etc.
      mutate(replicate=n())  %>%
      #mutating the payment variables to split the expenditures across the CCC
      mutate_at(grep("[0-9]{2}X$", colnames(.), value = TRUE),  ~ . / replicate) %>%
      
      #Cleaning up the data set
      ungroup() %>%
      mutate(prescrip_id = RXRECIDX) %>% 
      select(-CCC_type, -replicate, -RXRECIDX) %>% 
      #Collapse into dupersid-CCC level
      group_by(DUPERSID, CCC_value) %>%
      dplyr::summarize(across(starts_with("RX"), sum),
                       prescrips = n_distinct(prescrip_id)) %>%
      ungroup() 
    
  #Merging Data Sets
  #This variable combines the "dwelling id" and the "person id" to get a unique observation
  merge_var <- "DUPERSID"
  
  merge_data <- merge(x = fyc_data, 
                      y = drug_data, 
                      by = merge_var,
                      all = TRUE) %>%
      
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate_if(is.character, ~replace(., is.na(.), "-1"))  %>%
        mutate(YEAR = year) 
    
    
  #combining al the variables we need into a single one to make the loop easier
  all_vars <- c(fyc_vars, 
                drug_id_vars, 
                drug_payment_vars,
                "Survey Year" = "YEAR") 
  
  #Loops for consistent labels and variable names
  for(var in unname(all_vars)){
    
    #Relabeling vars
    if(var %in% colnames(merge_data)){
      label(merge_data[[var]]) <- unique(names(all_vars[all_vars == var]))
    } 
    
    #renaming payment vars back to the generic so can be rbind-ed intelligently
    if(var %in% c(drug_payment_vars, paste0("PERWT", substr(year, 3,4), "F"))){
      generic_name <- gsub(substr(year, 3,4), "YY", var)
      colnames(merge_data)[colnames(merge_data) == var] <- generic_name
    }


  }
  
  unique_fyc_people <- length(unique(fyc_data$DUPERSID))
  unique_merge_people <- length(unique(merge_data$DUPERSID))
  
  if(unique_fyc_people != unique_merge_people){
    stop("The unique number of people in the FYC dataset is different from the unique nuymber of people in the merged data")
  }
  
  return(merge_data)
  
}
# End: MEPS Compilation ---------------------------------------------------



# MEPS Drugs Compile -----------------------------------------------------
  #Apply to annual data to generate a list with annual dataframes
MEPS_compile <- function(cutoff_year = 2015){
  
  file_names <<- MEPS_file_names()
  
  #cutoff_year <- 2016
  
  MEPS_years <- file_names$year[file_names$year <= cutoff_year]
  #test <- c("1996", "1997")
  
  final_list <- lapply(MEPS_years, MEPS_annual)
  
  return(final_list)
  
  }

compiled_list <- MEPS_compile()

# Collapse the list into one data frame
cost_dataframe <- as_tibble(rbindlist(compiled_list,
                            fill = TRUE)
                  ) %>%
  mutate(YEAR = as.numeric(YEAR))
  
#Tri-care variable didn't exist until 2000 survey, so want to show up as zero
#cost_dataframe$RXTRYYX[is.na(cost_dataframe$RXTRYYX)] <- 0
# End: Drug Compile -------------------------------------------------------


# Writing Drug Data to  Stata ---------------------------------------------
cost_data_file_name <- "duperid_ccc_1996_to_2015.dta"

cost_data_path <- file.path(base_dir, "analysis_data", cost_data_file_name)

write_dta(data = cost_dataframe,
          path = cost_data_path)



# # Quick Hist of Age -------------------------------------------------------
# age_data <- cost_dataframe %>%
#   transmute(AGE = YEAR - DOBYY,
#             DUPERSID = DUPERSID) %>%
#   unique(.) 
# 
# age_hist_plot <- 
# ggplot(data=age_data, aes(x=AGE)) + 
#   geom_histogram(breaks=seq(0, 90, by= 4), 
#                  col="grey", 
#                  fill="yellow", 
#                  alpha = .2) + 
#   theme_minimal() + 
#   labs(title="MEPS Age Histogram", 
#        subtitle = "Each individual is counted twice due to follow-up survey, but data is only recorded for the year of the survey",
#        caption = "Source: MEPS Full Year Consolidated Data and Prescribed Medicines Files for the years 1996 to 2015",
#        x="Age", y="Count")
# 
# age_hist_path <- file.path(base_dir, "charts_bryce", "age_histogram_R.pdf")
# 
# ggsave(filename = age_hist_path,
#        plot = age_hist_plot)
# 
# # Timing Notes ------------------------------------------------------------
# end_time <- Sys.time()
# run_time <- end_time - start_time
# 
# run_time

