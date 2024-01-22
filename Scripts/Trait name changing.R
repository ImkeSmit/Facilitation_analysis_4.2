##There are still species name discrepancies
#Let's fix them
library(tidyverse)
library(tidylog)


###read in the facilitation data for each country
wd <- "Facilitation data\\Countriesv2"
data_files <- list.files(wd)
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Countriesv2\\",
                          data_files[i])))
}

###Get the species names
#first rbind all the names
vars <- c("COU", "ID","ID_Microsite",  "Species.within.quadrat")
all_countries <- rbind(algeria[ , which(colnames(algeria) %in% vars)], argentina[ , which(colnames(argentina) %in% vars)], australia[ , which(colnames(australia) %in% vars)], chile[ , which(colnames(chile) %in% vars)], 
                       chinachong[ , which(colnames(chinachong) %in% vars)], chinaxin[ , which(colnames(chinaxin) %in% vars)], iranabedi[ , which(colnames(iranabedi) %in% vars)], iranfarzam[ , which(colnames(iranfarzam) %in% vars)], 
                       israel[ , which(colnames(israel) %in% vars)], namibiablaum[ , which(colnames(namibiablaum) %in% vars)], namibiawang[ , which(colnames(namibiawang) %in% vars)], southafrica[ , which(colnames(southafrica) %in% vars)],  
                       spainmaestre[ , which(colnames(spainmaestre) %in% vars)], spainrey[ , which(colnames(spainrey) %in% vars)])

fac_names_nurse <- all_countries |>  #unique names of nurses
  distinct(ID_Microsite) |>          #remove bare
  filter(!(ID_Microsite == "Bare")) |> 
  rename(fac_taxon = ID_Microsite)
  

fac_names_quadrat <- all_countries |> #unique names of target sp
  distinct(Species.within.quadrat) |> 
  filter(!(is.na(Species.within.quadrat))) |> #remove na
  rename(fac_taxon = Species.within.quadrat)

#combine the quadrat and nurse names, but remove duplicates again 
#(instances where a species is both a nurse and target)
fac_names_total <- fac_names_nurse |> 
  bind_rows(fac_names_quadrat) |> 
  distinct(fac_taxon) |> 
  arrange(fac_taxon)


###Get the trait data names
#import the version that is subsetted for the facilitation sites, we don't care about the species in the other sites
trait_raw <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis\\Functional trait data\\FT_match_facilitation_plots.csv")

trait_names <- trait_raw |> 
  mutate(trait_taxon = str_c(Genus, Species, sep = " ")) |> 
  distinct(trait_taxon) |> 
  arrange(trait_taxon)
  

#now, the trait data names have to be changed to match the fac names
#I don't want to change the fac names because I've already completed that analysis


