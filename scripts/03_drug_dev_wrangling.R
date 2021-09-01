#Counts the progress of drugs in the development process


rm(list = ls())

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(foreign)
library(haven)
library(parallel)
library(zoo)


# Options -----------------------------------------------------------------
dir <- "~/Dropbox (Harvard University)/Drug_Age_Trends/submission_files/"

setwd(dir = dir)


# Data Wrangling ----------------------------------------------------------
raw_file <- "analysis_data/mapping_prewrangle.dta"


raw_data <- read_dta(file = raw_file) %>%
  mutate_all(~replace(., grepl("\\.", .), NA_character_ ))

trial_cols <- grep("dev|earliest|latest", colnames(raw_data), value = T)

long_data <- raw_data %>%
  pivot_longer(cols = all_of(trial_cols), 
               values_to = "date",
               names_to = "action")

all_dates <- sort(unique(long_data$date)) %>% tail(., -1)

all_drug_ids <- unique(long_data$drugid)


# Drug Progression --------------------------------------------------------
drug_progression_tags <- function(dataframe, row_num, date_vec) {

#  dataframe <- raw_data
#  row_num <- 1
# date_vec <- all_dates

  num_dups <- length(date_vec) - 1
  
  single_drug_vec <- dataframe[row_num,]

  
  drug_df <- rbind(single_drug_vec, single_drug_vec[rep(1,num_dups), ]) %>%
    cbind(date_vec) %>%
    filter(date_vec >= dev_date_earliest) %>%
    mutate(drug_phase = case_when(date_vec >= dev_date_last  ~ HighestStatus,
                                  date_vec >= dev_date_earliest_launch ~ "Launched",
                                  date_vec >= dev_date_earliest_p3 ~ "Phase 3",
                                  date_vec >= dev_date_earliest_p2 ~ "Phase 2",
                                  date_vec >= dev_date_earliest_p1 ~ "Phase 1",
                                  date_vec <= dev_date_earliest_p1 ~ "Pre Trial"
                                  ),
           drug_phase = na.locf(drug_phase, fromLast = T),
           drug_phase = gsub(" Clinical", "", drug_phase),
           new_in_sample = case_when(date_vec == dev_date_earliest ~ 1,
                                     TRUE ~ 0),
           new_drug = case_when(date_vec == dev_date_earliest_launch ~ 1,
                                TRUE ~ 0),
           pretrial = case_when(drug_phase %in% c("Pre Trial", "Discovery") ~ 1,
                                TRUE ~ 0),
           pretrial_start = case_when(pretrial == 1 & lag(pretrial) == 0 ~ 1,
                                      pretrial == 1 & is.na(lag(pretrial)) ~ 1,
                                    TRUE ~ 0),
              #Want a Q4 signal to track if the drug entered pretrials this year
           new_pretrial = case_when(substr(date_vec, 6, 6) != "4" ~ 0,
                                    pretrial_start == 1 ~ 1,
                                    lag(pretrial_start, 1) == 1 ~ 1,
                                    lag(pretrial_start, 2) == 1 ~ 1,
                                    lag(pretrial_start, 3) == 1 ~ 1,
                                    TRUE ~ 0),
           new_phase1 = case_when(substr(date_vec, 6, 6) != "4" ~ 0,
                                  drug_phase != "Phase 1" ~ 0,
                                  drug_phase == lag(drug_phase, 4) ~ 0,
                                  !is.na(lag(drug_phase, 4)) ~ 0, 
                                  TRUE ~ 1),
           new_phase2 = case_when(substr(date_vec, 6, 6) != "4" ~ 0,
                                  drug_phase != "Phase 2" ~ 0,
                                  drug_phase == lag(drug_phase, 4) ~ 0,
                                  !is.na(lag(drug_phase, 4)) ~ 0, 
                                  TRUE ~ 1),
           new_phase3 = case_when(substr(date_vec, 6, 6) != "4" ~ 0,
                                  drug_phase != "Phase 3" ~ 0,
                                  drug_phase == lag(drug_phase, 4) ~ 0,
                                  !is.na(lag(drug_phase, 4)) ~ 0, 
                                  TRUE ~ 1),
           trials = case_when(drug_phase %in% c("Phase 3", "Phase 2", "Phase 1", "Clinical") ~ 1,
                              TRUE ~ 0),
           launched = case_when(drug_phase == "Launched"~ 1,
                                TRUE ~ 0),
           failed = case_when(drug_phase %in% c("Discontinued", "Suspended", "Withdrawn") ~ 1,
                              TRUE ~ 0)
     ) %>%
    select(CCC_value, drugid,cortellis_company = OriginatorCompany, icd9_code,
           date = date_vec, drug_phase, 
           new_drug, pretrial, new_in_sample, trials, launched, failed, new_pretrial, pretrial_start,
           new_phase1, new_phase2, new_phase3)

  
  # if(exists("full_history_df")){
  #   full_history_df <<- rbind(full_history_df, drug_df)
  # } else{
  #   full_history_df <<- drug_df
  # }

}

# Parallel Function Run ---------------------------------------------------
avail_cores <- 4
total_rows <- dim(raw_data)[1]

drug_level_data <- 
  mclapply(X = seq(1:total_rows),
           FUN = drug_progression_tags,
           dataframe = raw_data,
           #row_num = seq(1:20),
           date_vec = all_dates,
           mc.cores = 6) %>%
  bind_rows(.) %>%
  filter(substr(date, 5,6) == "q4") %>%
  mutate(year = as.numeric(substr(date, 1, 4)),
         CCC_value = str_pad(CCC_value, width = 3, pad = "0"))


# Pretrial Exploration ----------------------------------------------------
# test <- drug_level_data %>%
# #   filter(new_in_sample == 1) %>%
#   group_by(date) %>%
#   dplyr::summarise(
#     pretrial = sum(pretrial),
#     new_pretrials = sum(new_pretrial),
#     new_phase1 = sum(new_phase1),
#     new_phase2 = sum(new_phase2),
#     new_phase3 = sum(new_phase3)
#   )


# test2 <- drug_level_data %>%
#   filter(date  <= "1997q4") %>%
#   select(year, drugid,CCC_value, new_pretrial, pretrial)
# 
# test3 <- 
#   drug_level_data %>%
#   filter(date == "1996q4" & new_phase1 == 1)
# Collapse to CCC-year level ----------------------------------------------

ccc_level_data <- drug_level_data %>%
  group_by(CCC_value, date) %>%
  dplyr::summarise(
    new_in_sample = sum(new_in_sample),
    new_drugs = sum(new_drug),
    pretrial = sum(pretrial),
    trials = sum(trials),
    launched = sum(launched),
    failed = sum(failed),
    new_pretrial = sum(new_pretrial),
    new_phase1 = sum(new_phase1),
    new_phase2 = sum(new_phase2),
    new_phase3 = sum(new_phase3)
  ) %>%
  ungroup() %>%
  filter(substr(date, 5,6) == "q4") %>%
  mutate(year = as.numeric(substr(date, 1, 4)),
         CCC_value = str_pad(CCC_value, width = 3, pad = "0")) %>%
  select(-date)


# CCC Exploration ---------------------------------------------------------
# test4 <- 
#   ccc_level_data %>%
#   filter(year <= 2000)


# Output Data -------------------------------------------------------------
out_file <- "analysis_data/CCC_lv_drugdev_status.dta"
write_dta(data = ccc_level_data,
          path = out_file)

out_file <- "analysis_data/drug_lv_drugdev_status.dta"
write_dta(data = drug_level_data,
          path = out_file)
