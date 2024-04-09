###explore MAT as covariate instead of aridity###
library(tidyverse)
library(tidylog)

#import nint results
all_result <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv", row.names = 1)

#import siteinfo
siteinfo <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\BIODESERT_sites_information.csv") |> 
  select(ID, ARIDITY.v3, AMT, RAI)
  
