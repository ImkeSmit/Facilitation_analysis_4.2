##This script is to check if the facilitation data is spatially autocorrelated
library(tidyverse)
library(tidylog)
library(sf)


#import nint results
nint_result <- read.csv("Facilitation data//results//NIntc_results_allcountries_6Feb2024.csv", row.names = 1)

#import the siteinfo, which has coordinates for each site
siteinfo <- read.csv("Facilitation data//BIODESERT_sites_information.csv") |> 
  select(ID, Lat_decimal, Long_decimal)

#join the coordinates to nint result
nint_result <- nint_result |> 
  left_join(siteinfo, by = "ID")

