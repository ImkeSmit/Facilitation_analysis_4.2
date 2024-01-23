##There are still species name discrepancies
#Let's fix them
library(tidyverse)
library(tidylog)
library(readxl)
library(DescTools)


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

write_csv2(fac_names_total, "C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis\\Functional trait data\\fac_names.csv") 

write_csv2(trait_names, "C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis\\Functional trait data\\trait_names.csv") 


###Let's look at the state of the names in the quadrat data###
#The quadrat data has a sheet for every country, lets extract only the countries for which we have facilitation data
# Specify the file path to your Excel file
excel_file <- "C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis\\Functional trait data\\Cover quadrats_98sites_16_12_19.xlsx"

# Specify the sheets you want to extract
fac_countries <- c("Algeria", "Argentina", "Australia", "Chile", "China", "Iran", 
                                     "Israel", "Namibia", "South Africa",  "Spain")

# Create a list to store the data frames for each selected sheet
selected_sheets_data <- list()

# Loop through each sheet and extract the data
for (sheet_name in fac_countries) {
  sheet_data <- read_excel(excel_file, sheet = sheet_name)
  selected_sheets_data[[sheet_name]] <- sheet_data
}# Now, selected_sheets_data contains data frames for the fac countries
length(selected_sheets_data)
names(selected_sheets_data)



##Each country's sheet contains multiple species x site matrices, one for each plot.
#each matrix is separated by a row of NA's. We need to cut the dataframe at each row of NA's 
# Function to cut dataframe at each row with NA in the first column
cut_dataframe <- function(df) {
  # Find the row indices where the first column is NA
  cut_indices <- which(is.na(df[ , 1]))
  
  # Split the dataframe at the identified indices
  split_dataframes <- split(df, cumsum(seq_along(df[ , 1]) %in% cut_indices))
  
  # Remove empty dataframes (resulting from consecutive NA rows)
  split_dataframes <- split_dataframes[sapply(split_dataframes, function(x) !all(is.na(x[ , 1])))]
  
  return(split_dataframes)
}

##Now cut the dataframes of each country
for (i in 1:length(selected_sheets_data)) {
  country <- as.data.frame(selected_sheets_data[[i]]) #select one country from the list
  country <- rbind(colnames(country), country) #change the column names to the first row
  colnames(country) <- c("Plotname", seq(1:(ncol(country)-1))) #give new column names
  if (i == 1) {
  country_cut <- cut_dataframe(country)
  } else {
    temp_country_cut <- cut_dataframe(country)
    country_cut <- c(country_cut, temp_country_cut)
  }
} #country_cut is a list of dataframes. each dataframe is a species x site matrix of a plot. It contains the matrices of all countries


###Now we need to extract the plots that are also in the facilitation data
##read in the facilitation data for each country
wd <- "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Countriesv2"
data_files <- list.files(wd)
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Countriesv2\\",
                          data_files[i])))
}

#get the site names from the facilitation data
for (k in 1:length(countrynames)) {
  cou <- get(countrynames[k])
  if (k == 1) {
    fac_sitenames <- c(unique(cou$SITE))
  } else {
    temp_sitenames <- c(unique(cou$SITE))
    fac_sitenames <- c(fac_sitenames, temp_sitenames)
  }
}

##Now subset
country_cut_copy <- country_cut
keep <- list()

for (n in 1:length(country_cut_copy)) {
  plot <- country_cut_copy[[n]]
  plot <- plot[which(!is.na(plot$Plotname)) , ] #remove any rows with NA if they are there
  
  if(plot[1,1] %like% c(paste(fac_sitenames, "%", sep = "")) == TRUE) {
    
    if(n == 1) {
    keep[[1]] <- plot
    }else{
        keep_temp <- plot
        keep <- c(keep, keep_temp)
      }
    } 
  }

length(keep)
keep[1:3]
