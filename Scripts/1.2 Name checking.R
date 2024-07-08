##There are still species name discrepancies
#Let's fix them
library(tidyverse)
library(tidylog)
library(readxl)
library(DescTools)


###read in the facilitation data for each country
wd <- "Facilitation data\\Countriesv3"
data_files <- list.files(wd)
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Countriesv3\\",
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
  arrange(fac_taxon) |> 
  rename(taxon = fac_taxon)


###Get the trait data names
#import the version that is subsetted for the facilitation sites, we don't care about the species in the other sites
trait_raw <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis\\Functional trait data\\FT_match_facilitation_plots_plotspecific_species_31jan.csv", row.names = 1)

trait_names <- trait_raw |> 
  distinct(taxon) |> 
  arrange(taxon)


###Get the names from the quadrat data
quad <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis\\Functional trait data\\quadrat_survey_for_facilitation_plots.csv", row.names = 1)

quad_names <- quad |> 
  distinct(taxon) |> 
  arrange(taxon)


#now, the trait data names have to be changed to match the fac names
#I don't want to change the fac names because I've already completed that analysis
#but you have to because there are duplicate names

write_csv2(fac_names_total, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Name changes\\fac_names_31jan.csv") 

write_csv2(trait_names, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Name changes\\trait_names_31jan.csv") 

write_csv2(quad_names, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Name changes\\quadrat_survey_names_31jan.csv")


only_in_fac <- fac_names_total |> #in fac but not in quadrat
anti_join(quad_names) 


anti_join(trait_names, fac_names_total, by = "taxon") #there are no names in trait_names that are not in fac_names_total
#so if we just compare fac_names_total with quadrat_names, we have covered ourselves

#The sheet names_and_synonyms contains names and their synonyms. This list was made by compaing only_in_fac to the quadrat data
name_trail <- read_excel("Facilitation data\\Name changes\\names_and_synonyms.xlsx") |> 
  mutate(correct_name = str_squish(correct_name), ##remove spaces before or after strings, and replace internal whitespace with a single space
         synonym1 = str_squish(synonym1),
         synonym2 = str_squish(synonym2))

#create synonyms variable which is all the synonyms concatenated
name_trail$synonyms <- paste(name_trail$synonym1, name_trail$synonym2, sep = "; ") 

name_trail <- name_trail %>% 
  select(correct_name, synonyms) # only keep "correct_name and "synonyms"

  
#In the trait and quadrat data: remove spaces before or after strings, and replace internal whitespace with a single space 
trait_raw <- trait_raw |>
  mutate(sp_fullname = str_squish(sp_fullname)) 

quad <- quad |> 
  mutate(taxon = str_squish(taxon)) 




###Fix names in trait_raw
trait_change_tracker <- data.frame( # create a dataframe that stores information about samples for which species names are modified by the code below
  old_spec = character(), 
  new_spec = character(), 
  stringsAsFactors=FALSE) 
trait_data_harmony <-  trait_raw# create a copy of the raw data that will store the changes

for (i in 1:nrow(trait_raw)) {
  old_sp <- trait_data_harmony[i, which(colnames(trait_data_harmony) == "sp_fullname")]
  new_sp <- NA
  
  found <- FALSE
  for (j in 1:nrow(name_trail)) { # looks whether species name is a synonym and replaces it with the true_name if it is found to be a synonym
    found <- grepl(old_sp, name_trail[j, 2]) 
    
    if (found){ # only runs if the species is a synonym
      new_sp <- name_trail[j, 1] # finds the true name of the species and saves it 
      break
    }
  }
  
  if (found) { # replaces the species in the trait database with the saved true name if "found" is "TRUE"
    trait_data_harmony[i, which(colnames(trait_data_harmony) == "sp_fullname")] <- new_sp 
    
    # add a new row with information about change to the change trackers dataset
    trait_change_tracker[i, 1] <- old_sp
    trait_change_tracker[i, 2] <- new_sp
    
    trait_change_tracker <- na.omit(trait_change_tracker)
  }
}; rm(old_sp, new_sp, found, i, j)  # remove unnecessary variables
head(trait_change_tracker, 10)
         
  



i
trait_raw[1400 ,]
?grepl
