
require_fn <- function(library_str){
  print(library_str)
  install.packages(library_str)
  require(library_str)
}

all_libraries <- 
  c("devtools",
    "tidyverse",
    "httr",
    "data.tale",
    "Hmisc",
    "haven",
    "parallel",
    "ggthemes", 
    "zoo", 
    "gridExtra",
    "foreign")


lapply(X = all_libraries,
       FUN = require_fn)

install.packages("e-mitchell/meps_r_pkg/MEPS")
require(MEPS)


