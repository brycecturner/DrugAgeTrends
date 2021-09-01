#Pulling Out Lipitor trend over time



# Lipitor Notes -----------------------------------------------------------
# Began sales in 1997
# Patent expired in 2011


rm(list = ls())

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(MEPS)
library(parallel)
library(ggthemes)
library(zoo)
library(gridExtra)

# Options -----------------------------------------------------------------

dir <- "~/Dropbox (Harvard University)/Drug_Age_Trends/submission_files/"
setwd(dir)


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

# Return ------------------------------------------------------------------
  return(file_names)
}

file_names <- MEPS_file_names()


# Extract From MEPS -------------------------------------------------------
meps_drug <- function(year, drug_name){
# Inputs ------------------------------------------------------------------
#  year <- 2015
# drug_name <- "lipitor"
  search_term <- toupper(drug_name)
  
# Get Files ---------------------------------------------------------------

  fyc_file <- file_names$fyc[file_names$year == year]
  drug_file <- file_names$pmed_events[file_names$year == year]

# Get DOB -----------------------------------------------------------------
  fyc_data <- as_tibble(read_MEPS(file = fyc_file)) %>% 
    select(DUPERSID, DOBYY) %>%
    mutate(age = year - DOBYY)

# Get Lipitor Info --------------------------------------------------------
  
  payment_var <- paste0("RXXP", 
                        substr(year, 3,4), 
                        "X")
  
  # This is at the PRESCRIPTION level
  drug_data <- as_tibble(read_MEPS(file = drug_file)) %>%
    select(DUPERSID, RXNAME, RXNDC, total_cost = all_of(payment_var)) %>%
    filter(grepl(search_term, RXNAME))
                  


# Merge Together ----------------------------------------------------------
  prescription_all <- merge(x = fyc_data, by.x = "DUPERSID",
                             y = drug_data, by.y = "DUPERSID") %>%
    mutate(Year = year,
           drug_name = drug_name) %>%
  select(Year, DUPERSID, age, total_cost)
 

# Return ------------------------------------------------------------------
  return(prescription_all)
   
}

# Lipitor Totals ----------------------------------------------------------
lipitor_prescriptions <-
  mcmapply(FUN = meps_drug,
          drug_name = "lipitor",
           year = seq(1996, 2011, 1),
           mc.preschedule = T,
           mc.cores = 4,
           SIMPLIFY = F)


lipitor_customers <- lipitor_prescriptions %>%
  bind_rows(.) %>%
  mutate(age_buckets = cut(age, 
                      breaks = c(0, 25, 45, 65, 91),
                      #breaks=seq(0, 90, 10),
                      right = F, 
                      labels = c("0-24",
                                 "25-44",
                                 "45-64",
                                 "65+"))) %>%
  group_by(age_buckets, Year) %>%
  dplyr::summarize(
    total_cost = sum(total_cost) / 1000,
    Patients = n_distinct(DUPERSID)
  ) %>%
  group_by(Year) %>%
  mutate(
    annual_patients = sum(Patients),
    bucket_share = round(Patients / annual_patients, 2),
    bucket_share_65 = ifelse(age_buckets == "65+", bucket_share, NA_real_),
    bucket_share_65 = na.locf(bucket_share_65,fromLast = T),
    
    annual_cost = sum(total_cost),
    cost_share = round(total_cost/annual_cost, 2),
    cost_share_65 = ifelse(age_buckets == "65+", cost_share, NA_real_),
    cost_share_65 = na.locf(cost_share_65, fromLast = T)
  ) 



# Cost Plot ---------------------------------------------------------------
cost_plot <- 
  ggplot(data = lipitor_customers, aes(fill = age_buckets, y = total_cost, x = Year)) +
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  ylim(0, 1000) +
  geom_text(aes(label = cost_share_65, group = Year), 
            stat = 'summary', fun = sum, vjust = -0.7) +
  theme_stata() + 
  scale_fill_grey(start = 0.8, end = 0.0) + 
  annotate(
    geom = "text",
    x = 1999.7,
    y = 630,
    label = "Elderly Expense \nShare = "
  ) + 
  # labs(title = "Lipitor Expenditures Over Time",
  #      caption = "Source: MEPS") +
   ylab(element_blank()) + 
  guides(fill = guide_legend(title = "Age Buckets")) + 
  theme(legend.title = element_text(color = "black"))


# Export Charts -----------------------------------------------------------


# ggsave(
#   filename = "drug_specific/lipitor_patients.pdf",
#   plot = patient_plot,
#   width = 8.5, 
#   height = 5.5
#   
# )

ggsave(
  filename = "lipitor_cost.pdf",
  plot = cost_plot,
  width = 8.5, 
  height = 5.5
  
)


# Save Data ---------------------------------------------------------------
out_file <- "lipitor_raw.csv"
write_csv(x = lipitor_customers,
          path = out_file)
