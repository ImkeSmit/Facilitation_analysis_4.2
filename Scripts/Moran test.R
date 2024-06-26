##This script is to check if the facilitation data is spatially autocorrelated
#we will do the moran test at the site level. Thus we need to summarise the nintc at the site level
library(tidyverse)
library(tidylog)
library(ape)
library(geosphere)

#import nint results
nint_result <- read.csv("Facilitation data//results//NIntc_results_allcountries_6Feb2024.csv", row.names = 1)

#import the siteinfo, which has coordinates for each site
siteinfo <- read.csv("Facilitation data//BIODESERT_sites_information.csv") |> 
  select(ID, Lat_decimal, Long_decimal)

#join the coordinates to nint result
nint_result_join <- nint_result |> 
  left_join(siteinfo, by = "ID") |> 
  #summarise the mean NINtc richness by plot
  group_by(site_ID) |> 
  mutate(mean_NIntc_richness = mean(NIntc_richness, na.rm = T), 
         mean_NIntc_cover = mean(NIntc_cover, na.rm = T)) |> 
  ungroup() |> 
  distinct(site_ID, mean_NIntc_richness, .keep_all = T) |> 
  select(!c(ID, plot, graz, aridity, NIntc_richness, NIntc_cover, NIntc_shannon, NInta_cover, NInta_richness, NInta_shannon, replicate_no, nurse))

#generate a matrix with the inverse distance between each coordinate
#matrix of distances between points
#use distGeo because points are spread accross the world, need to take curvature of earth into account
distmat <- 
  as.matrix(distm(nint_result_join[, c(which(colnames(nint_result_join) == "Long_decimal"), which(colnames(nint_result_join) == "Lat_decimal"))], 
        fun = distGeo))
#get the inverse:
distmat_inv <- 1/distmat
diag(distmat_inv) <- 0
#we only need one half of the matrix
distmat_inv <- distmat_inv[1:nrow(nint_result_join), 1:nrow(nint_result_join)]


#do the moran test to see if NIntc richness values are spatially autocorrelated
MI <- Moran.I(nint_result_join$mean_NIntc_richness, weight = distmat_inv)
#p large, thus data are not autocorrelated
