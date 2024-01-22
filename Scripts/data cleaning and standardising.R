###CLEANING AND STANDARDISING THE FACILITATION DATA
library(stringr)
library(data.table)
library(dplyr)
wd <- "C:\\Users\\imke6\\Documents\\Msc Projek"
setwd(wd)

##RULES:
#Rows with dead plants removed if they are not the only entry in the microsite
#If there is more than one variety of a species they are combined into one sp

#import the information of each site
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv")

#import the sp_matches file because we have to standardise names with the facilitation dataset
#names with the same number in the tofix column must have the same spelling
#All names in the facilitation data will just be changed to match the spelling of the FT data, correctness doesn't matter
namedat <- read.csv("Functional trait data\\sp_matches.csv", row.names = 1, col.names = c("ID", "spname", "position", "tofix"))
#RULES: Infraspecies ranks were removed in the fac and the FT data so that species match
#Names  were not matched if only the genus matched. Eg. Thymus in teh trait data and Thymus vulgare in the FT data are not regarded as the same
#Names with sp. or indet. were regarded as the same. Eg Thymus sp. in the fac data regarded the same as Thymus indet. in the FT data
#Names with cf regarded as the same. Eg. cf. Rhuschia spinosa in the fac data regarded the same as Ruschia spinosa in the FT data. 



algeria <- read.csv("Facilitation data\\Countries\\Algeria_facilitation.csv")
southafrica <- read.csv("Facilitation data\\Countries\\SouthAfrica_facilitation.csv")
argentina <- read.csv("Facilitation data\\Countries\\Argentina_facilitation.csv")
australia <- read.csv("Facilitation data\\Countries\\Australia_facilitation.csv")
chile <- read.csv("Facilitation data\\Countries\\CHile_facilitation.csv")
chinachong <- read.csv("Facilitation data\\Countries\\China_Chong_facilitation.csv")
chinaxin <- read.csv("Facilitation data\\Countries\\China_Xin_facilitation.csv")
iranabedi <- read.csv("Facilitation data\\Countries\\Iran_Abedi_facilitation.csv")
iranfarzam <- read.csv("Facilitation data\\Countries\\Iran_Farzam_facilitation.csv")
israel <- read.csv("Facilitation data\\Countries\\Israel_facilitation.csv")
namibiablaum <- read.csv("Facilitation data\\Countries\\Namibia_Blaum_facilitation.csv")
namibiawang <- read.csv("Facilitation data\\Countries\\Namibia_Wang_facilitation.csv")
spainmaestre <- read.csv("Facilitation data\\Countries\\Spain_Maestre_facilitation.csv")
spainrey <- read.csv("Facilitation data\\Countries\\Spain_Rey_facilitation.csv")

#remove all the empty rows from each country dataset
algeria <- algeria[which(!is.na(algeria[,1])),]
southafrica <- southafrica[which(!is.na(southafrica[,1])),]
argentina <- argentina[which(!is.na(argentina[,1])),] #has no NA values
australia <- australia[which(!is.na(australia[,1])),]
chile <- chile[which(!is.na(chile[,1])),]
chinachong <- chinachong[which(!is.na(chinachong[,1])),]
chinaxin <- chinaxin[which(!is.na(chinaxin[,1])),]
iranabedi <- iranabedi[which(!is.na(iranabedi[,1])),]
iranfarzam <- iranfarzam[which(!is.na(iranfarzam[,1])),]
israel <- israel[which(!is.na(israel[,1])),]
namibiablaum <- namibiablaum[which(!is.na(namibiablaum[,1])), ]
namibiawang <- namibiawang[which(!is.na(namibiawang[,1])),]
spainmaestre <- spainmaestre[which(!is.na(spainmaestre[,1])),]
spainrey <- spainrey[which(!is.na(spainrey[,1])),]

####Fixing Algeria####
#Change the name of the Cover column
colnames(algeria)[which(colnames(algeria) == "Cover....")] <- "Cover"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
code <- paste(algeria$SITE_ID, algeria$PLOT, sep = "-")
code2 <- paste(algeria$ID_Microsite, algeria$Number.of.replicate, sep = "-")
algeria$microsite_rep <- paste(code, code2, sep = "-")

#In site 1 plot 3 replicate 7 there is only a Bare microsite in the replicate
#rename the replicate no to include the second "1-3-Stipa tenacissima-6" in replicate 7
algeria[which(algeria$microsite_rep == "1-3-Stipa tenacissima-6")[2], which(colnames(algeria) == "Number.of.replicate" )] <- 7
#Update microsite_rep
code <- paste(algeria$SITE_ID, algeria$PLOT, sep = "-")
code2 <- paste(algeria$ID_Microsite, algeria$Number.of.replicate, sep = "-")
algeria$microsite_rep <- paste(code, code2, sep = "-")

#look for strange entries in Species within quadrat
sort(unique(algeria$Species.within.quadrat))

#Change Empty to NA
algeria[which(algeria$Species.within.quadrat == "Empty"), which(colnames(algeria) == "Species.within.quadrat")] <- NA

#There are entries of "BSC bryophytes", remove these rows if there are other species in the replicate, 
#but change BSC bryophytes to NA if it is the only entry in the replicate
algeria <- algeria[-c(which(algeria$Species.within.quadrat == "BSC Bryophytes")),]#They are not the only entries for replicate so row 91 and 92 can just be removed

#Fix nurse names with multiple spellings
#"Stipa tenacissima"
algeria[algeria$ID_Microsite %like% "Stipa" , which(colnames(algeria) == "ID_Microsite") ] <- "Stipa tenacissima"

#Fix speciesnames with multiple spellings
#"Ajuga iva"
algeria[algeria$Species.within.quadrat %like% "Ajuga" , which(colnames(algeria) == "Species.within.quadrat") ] <- "Ajuga iva"
#"Artemisia campestris"
algeria[algeria$Species.within.quadrat %like% "campestris" , which(colnames(algeria) == "Species.within.quadrat") ] <- "Artemisia campestris"
"Echium trygorrhizum"
algeria[algeria$Species.within.quadrat %like% "trygor" , which(colnames(algeria) == "Species.within.quadrat") ] <- "Echium trygorrhizum"
#Iris sisyrinchium
algeria[algeria$Species.within.quadrat %like% "Iris" , which(colnames(algeria) == "Species.within.quadrat") ] <- "Iris sisyrinchium"
#Paronychia argentea
algeria[algeria$Species.within.quadrat %like% "argentea" , which(colnames(algeria) == "Species.within.quadrat") ] <- "Paronychia argentea"


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the algerian plots
algeria_namedat <- namedat[which(namedat$ID %in% c(unique(algeria$ID))) , ]
#get names that must be changed
algeria_namedat <- algeria_namedat[which(!is.na(algeria_namedat$tofix)) , ]

#Iris sisyrynchium
algeria[algeria$Species.within.quadrat %like% "Iris sisyrinchium" , which(colnames(algeria) == "Species.within.quadrat") ] <- "Iris sisyrynchium"
#Noaea mucronata
algeria[algeria$Species.within.quadrat %like% "Noea mucronata" , which(colnames(algeria) == "Species.within.quadrat") ] <- "Noaea mucronata"


####Fixing Argentina####
#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- argentina
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
argentina$microsite_rep <- paste(code, code2, sep = "-")

#Change the name of the cover column
colnames(argentina)[which(colnames(argentina) == "Cover....")] <- "Cover"

#overwrite the microsite variable to make sure it's correct
records <- c(1:nrow(argentina))
for (k in records) {
  if (argentina[k, which(colnames(argentina) == "ID_Microsite")] == "Bare") {
    argentina[k, which(colnames(argentina) == "Microsite")] <- 1
  } else {
    argentina[k, which(colnames(argentina) == "Microsite")] <- 2
  }
}

#In site 7 plot 2 there is only nurse sites in replicate 52. Fix this by renaming the replicates so 
#that replicate 52 and 53 have nurse and bare microsites
argentina$new_rep <- argentina$Number.of.replicate
argentina[which(argentina$microsite_rep == "7-2-Bare-53"), which(colnames(argentina) == "new_rep")] <- 52
argentina[which(argentina$microsite_rep == "7-2-Bare-54")[1], which(colnames(argentina) == "new_rep")] <- 53

#In site 6 plot 1 there are three nurses each with replicate numbers 1-25.
#However the nurses are mixed, ie the replicate numbers do not follow on each other within a nurse
#The nurses are Condalia microphylla, Prosopis flexuosa, var. depressa, Larrea divaricata
#subset the data for each nurse

site6_plot1 <- argentina[which(argentina$microsite_rep %like% "6-1"), ]
lower <- which(site6_plot1$microsite_rep == "6-1-Condalia microphylla-1")
upper <- which(site6_plot1$microsite_rep == "6-1-Bare-25")
range <- c(lower[1]:upper[11]) #There are 11 species in the last bare microsite paired with Condalia
Condalia_6_1 <- site6_plot1[range, ] #with nurse Condalia

othernurses <- site6_plot1[-range, ] # with the other two nurses

#now get all the rows that correspond to positions with Prosopis
a <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-1")[1]
b <- which(othernurses$microsite_rep == "6-1-Bare-4")[5] #There are 5 species in the last bare microsite paired with Prosopis

c <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-5")[1]
d <- which(othernurses$microsite_rep == "6-1-Bare-6")[3]

e <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-7")[1]
f <- which(othernurses$microsite_rep == "6-1-Bare-8")[8]

g <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-9")[1]
h <- which(othernurses$microsite_rep == "6-1-Bare-9")[3]

i <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-10")[1]
j <- which(othernurses$microsite_rep == "6-1-Bare-10")[6]

k <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-11")[1]
l <- which(othernurses$microsite_rep == "6-1-Bare-13")[4]

m <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-14")[1]
n <- which(othernurses$microsite_rep == "6-1-Bare-14")[4]

o <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-15")[1]
p <- which(othernurses$microsite_rep == "6-1-Bare-15")[5]

q <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-16")[1]
r <- which(othernurses$microsite_rep == "6-1-Bare-16")[10]

s <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-17")[1]
t <- which(othernurses$microsite_rep == "6-1-Bare-20")[5]

u <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-21")[1]
v <- which(othernurses$microsite_rep == "6-1-Bare-23")[4]

w <- which(othernurses$microsite_rep == "6-1-Prosopis flexuosa var. depressa-24")[1]
x <- which(othernurses$microsite_rep == "6-1-Bare-25")[5]

#dataframe with only prosopis nurse
Prosopis_6_1 <- othernurses[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x) , ] #with nurse Prosopis
#change the replicate numbers to follow on from condalia
Prosopis_6_1$new_rep <- Prosopis_6_1$Number.of.replicate +25

#dataframe with only Larrea nurse
Larrea_6_1 <- othernurses[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x) , ] #with nurse Larrea
#change the replicate numbers to follow on from Prosopis
Larrea_6_1$new_rep <- Larrea_6_1$Number.of.replicate +25 +25

#Now join Condalia_6_1, Prosopis_6_1 and Larrea_6_1 to the rest of the argentina dataframe
argentina_min6_1 <- argentina[-which(argentina$microsite_rep %like% "6-1"), ] #the argentina data without site 6 plot 1 
argentina <- rbind(Condalia_6_1, Prosopis_6_1, Larrea_6_1, argentina_min6_1) #bind it to the 3 nurses that make up plot 1

#Delete the "new number of replicate" column and update "the number of replicate" and the "microsite rep" column
argentina$Number.of.replicate <- argentina$new_rep
argentina <- argentina[, -which(colnames(argentina) == "new_rep")]

#Update the microsite_rep
country <- argentina
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
argentina$microsite_rep <- paste(code, code2, sep = "-")


#look for any strange entries in "species within quadrat"
sort(unique(argentina$Species.within.quadrat))

#delete row containing "", it has no microsite info
argentina <- argentina[-c(which(argentina$Species.within.quadrat == "")), ]


#Change Biological crust to NA if it is the only entry in the microsite, but delete row if it is not
bsc <- argentina[which(argentina$Species.within.quadrat %like% "crust") , ] 
bsc_reps <- c(bsc$microsite_rep)
for (n in bsc_reps) {
  subset <- argentina[which(argentina$microsite_rep == n),] #subset by unique microsite_rep
  coversum <- sum(subset$Cover)
  if (coversum > 1) {
    print(n)
  }
  else {
    print (paste(n, "remove", sep = "_"))
  }
}
#All the rows containing biological soil crust can just be removed, it is never the only entry in a microsite
argentina <- argentina[-c(which(argentina$Species.within.quadrat %like% "crust")) , ]

#Fix speciesnames with multiple spellings
#Acantholippia seriphioides
argentina[argentina$Species.within.quadrat %like% "ppia seriphioides" , which(colnames(argentina) == "Species.within.quadrat") ] <- "Acantholippia seriphioides"
#Baccharis melanopotomica
argentina[argentina$Species.within.quadrat %like% "melanopotamica" , which(colnames(argentina) == "Species.within.quadrat") ] <- "Baccharis melanopotamica"
#Baccharis gilliesii
argentina[which(argentina$Species.within.quadrat == "Baccharis gilliesiii"), which(colnames(argentina) == "Species.within.quadrat")]<- "Baccharis gilliesii"
#Gutierrezia baccharoides
argentina[which(argentina$Species.within.quadrat == "Gutierrezia Baccharis gilliesii"), which(colnames(argentina) == "Species.within.quadrat")]<- "Gutierrezia baccharoides"
#"Lycium gillesianum"
argentina[which(argentina$Species.within.quadrat == "Lycium Baccharis gilliesiianum"), which(colnames(argentina) == "Species.within.quadrat")]<- "Lycium gilliesianum"
#Nassella tenuis
argentina[argentina$Species.within.quadrat %like% "tenuis" , which(colnames(argentina) == "Species.within.quadrat") ] <- "Nassella tenuis"

#remove subspecies or variety names
argentina[c("target_genus", "target_species", "infra1", "infra2")] <- str_split_fixed(argentina$Species.within.quadrat, " ", 4)
argentina$Species.within.quadrat <- paste(argentina$target_genus, argentina$target_species, sep = " " )
argentina <- argentina[, -c(19:22)] #remove the genus, species and author columns

#replace " " with NA
#Use the cover column to find these records, because all species names have spaces in them
tochange <- which(is.na(argentina$Cover))
argentina[c(tochange), which(colnames(argentina) == "Species.within.quadrat")] <- NA


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the argentinian plots
argentina_namedat <- namedat[which(namedat$ID %in% c(unique(argentina$ID))) , ]
#get names that must be changed
argentina_namedat <- argentina_namedat[which(!is.na(argentina_namedat$tofix)) , ]

#The issue with Prosopis flexuosa var. depressa and Chuquiraga erinacea subsp. hystrix will be fixed by removing infraspecies in the FT dataset
#Aloysia gratissima
argentina[argentina$Species.within.quadrat %like% "Aloysia gratissima" , which(colnames(argentina) == "Species.within.quadrat") ] <- "Aloysia gratissima"

#In site 6 plot 3 there are multiple entries of nasella tenuis in some bare microsites
site6_plot3 <- argentina[which(argentina$microsite_rep %like% "6-3"), ]
#isolate entries of nasella tenuis
nas_ten <- site6_plot3[which(site6_plot3$Species.within.quadrat == "Nassella tenuis") , ]

#Loop to create a dataframe showing which replicates have more than two entries
reps <- unique(nas_ten$Number.of.replicate)
nas_ten_occurrences <- data.frame(matrix(nrow = 1, ncol = 2))
colnames(nas_ten_occurrences) <- c("replicate", "n_entries")

for (i in 1:length(reps)) {
  one_rep <- nas_ten[which(nas_ten$Number.of.replicate == reps[i]) , ]
  
  if(i == 1) {
    nas_ten_occurrences$replicate <- reps[i]
    nas_ten_occurrences$n_entries <- nrow(one_rep)
  } else {
    temp <- cbind(reps[i], nrow(one_rep))
    colnames(temp) <- c("replicate", "n_entries")
    nas_ten_occurrences <- rbind(nas_ten_occurrences, temp)
  }
}

#get the replicates with more than 2 entries
problem_reps <- c(nas_ten_occurrences[which(nas_ten_occurrences$n_entries > 2) , ]$replicate)

#get the entries to delete
d_index1 <- which(nas_ten$Number.of.replicate == problem_reps[1])[3] #delete the 40th occurrence of NAsella tenuis
d_index2 <- which(nas_ten$Number.of.replicate == problem_reps[2])[3] #delete the 100th occurrence of NAsella tenuis
d_index3 <- which(nas_ten$Number.of.replicate == problem_reps[3])[3]
d_index4 <- which(nas_ten$Number.of.replicate == problem_reps[4])[3]
d_index5 <- which(nas_ten$Number.of.replicate == problem_reps[5])[3]

#add a variable to site6_plot3 that has the cumulative occurrences species
site6_plot3 <- site6_plot3 %>%
  group_by(Species.within.quadrat) %>%
  mutate(cumulative_occurrences = row_number()) %>%
  ungroup()

#get the rows to delete from site6_plot3
#the rows must be of species Nasella tenuis, and must have the cumulative occurrences in c(d_index1, d_index2, d_index3, d_index4, d_index5)
# Define the conditions
species_condition <- site6_plot3$Species.within.quadrat == "Nassella tenuis"
occurrences_condition <- site6_plot3$cumulative_occurrences %in% c(d_index1, d_index2, d_index3, d_index4, d_index5)

# Combine conditions using the & (AND) operator
combined_condition <- species_condition & occurrences_condition

# Get row numbers that meet the combined condition, and delete them
corrected_site6_plot3 <- site6_plot3[-c(which(combined_condition)) , ]
#remove the cumulative_occurrences column
corrected_site6_plot3 <- corrected_site6_plot3[ , -which(colnames(corrected_site6_plot3) == "cumulative_occurrences")]

#Rbind the corrected_site6_plot3 to the rest of the argentina data
rest <- argentina[-which(argentina$microsite_rep %like% "6-3"), ]
argentina <- rbind(rest, corrected_site6_plot3)


####Fixing Australia####
#The column names are wrong, fix them to match the column names of the other countries
colnames(australia) <- c("ID", "COU", "RES", "SITE", "SITE_ID", "PLOT", "Number.of.replicate", "ID_Microsite",      
                         "Species.within.quadrat", "Number.of.individuals", "Cover")

#Replace "Interspace" and "Open" with "Bare"
australia[which(australia$ID_Microsite == "Interspace"), which(colnames(australia) == "ID_Microsite")] <- "Bare"
australia[which(australia$ID_Microsite == "Open"), which(colnames(australia) == "ID_Microsite")] <- "Bare"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- australia
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
australia$microsite_rep <- paste(code, code2, sep = "-")

#The Microsite variable is missing. Create a Microsite column with 1 for nurse patches and 2 for bare patches
australia$Microsite <- NA
records <- c(1:nrow(australia))
for (k in records) {
  if (australia[k, which(colnames(australia) == "ID_Microsite")] == "Bare") {
    australia[k, which(colnames(australia) == "Microsite")] <- 1
  } else {
    australia[k, which(colnames(australia) == "Microsite")] <- 2
  }
}

#In site 14 plot 1 there are two nurses each with replicate numbers 1-25.
#Change the replicate numbers for the second nurse, Nitraria billardieri to range from 26-50
australia$new_rep <- australia$Number.of.replicate
#The rows to fix are 364:415
australia$new_rep[364:415] <- australia$Number.of.replicate[364:415] +25

#In site 14 plot 3 there are two nurses each with replicate numbers 1-25.
#Change the replicate numbers for the second nurse, Maireana aphylla to range from 26-50
#The rows to fix are 248:313
australia$new_rep[248:313] <- australia$Number.of.replicate[248:313] +25

#Update the "number of replicate" and "microsite_rep"columns
australia$Number.of.replicate <- australia$new_rep
australia <- australia[, - which(colnames(australia) == "new_rep")]
country <- australia
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
australia$microsite_rep <- paste(code, code2, sep = "-")

#Look for strange entries in "Species within quadrat
sort(unique(australia$Species.within.quadrat))
#Replace "No species present" with NA
australia[which(australia$Species.within.quadrat == "No species present"), which(colnames(australia) == "Species.within.quadrat")] <- NA


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the australian plots
australia_namedat <- namedat[which(namedat$ID %in% c(unique(australia$ID))) , ]
#get names that must be changed
australia_namedat <- australia_namedat[which(!is.na(australia_namedat$tofix)) , ]

#Stipa scabra
australia[australia$Species.within.quadrat %like% "Stipa scabra" , which(colnames(australia) == "Species.within.quadrat") ] <- "Austrostipa scabra"
#Nitraria billardieri
australia[australia$Species.within.quadrat %like% "Nitraria billardieri" , which(colnames(australia) == "Species.within.quadrat") ] <- "Nitraria billardierei"
#Sporobolus caroli
australia[australia$Species.within.quadrat %like% "Sporobolus caroli" , which(colnames(australia) == "Species.within.quadrat") ] <- "Sporobolus carolii"
#Craspedia sp
australia[australia$Species.within.quadrat %like% "Craspedia sp" , which(colnames(australia) == "Species.within.quadrat") ] <- "Craspedia sp."


####Fixing Chile####
#Replace "bare soil"with "Bare"in ID_MIcrosite
chile[which(chile$ID_Microsite == "bare soil"), which(colnames(chile) == "ID_Microsite")] <- "Bare"

#Change the name of the Cover column
colnames(chile)[which(colnames(chile) == "Cover....")] <- "Cover"
chile$Cover <- as.numeric(chile$Cover)

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- chile
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
chile$microsite_rep <- paste(code, code2, sep = "-")

#the 7th mulinum replicate is missing in site 25 plot 3
#it was mislabelled as festuca, fix this
chile[which(chile$microsite_rep == "25-3-Festuca pallescens-7")[4] , which(colnames(chile) == "ID_Microsite")] <- "Mulinum spinosum"

#the 16th mulinum replicate is missing in site 25 plot 3
#it was mislabelled as festuca, fix this
chile[which(chile$microsite_rep == "25-3-Festuca pallescens-16")[3:5] , which(colnames(chile) == "ID_Microsite")] <- "Mulinum spinosum"

#There are two different spellings of Festuca pallescens in the ID_Microsite column, standardise them
chile[chile$ID_Microsite %like% "pallescens" , which(colnames(chile) == "ID_Microsite") ] <- "Festuca pallescens"

#There is a second instance of 25-3-Mulinum spinosum-22 with associated bare microsite
#this is probably a numbering mistake
#assign a replicate number of 26 to these records
lower <- which(chile$microsite_rep == "25-3-Mulinum spinosum-22")[2]
upper <- which(chile$microsite_rep == "25-3-Bare-22")[2]
chile[c(lower:upper), which(colnames(chile) == "Number.of.replicate")] <- 26

#Update microsite_rep
country <- chile
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
chile$microsite_rep <- paste(code, code2, sep = "-")

##In site 25 plot 1 there are 2 nurses, Acaena integerrima and Festuca pallescens, each with reps 1-25
#reassign replicate numbers so that it follows on from the previous nurse
chile$new_rep <- chile$Number.of.replicate
site25_plot1 <- chile[which(chile$microsite_rep %like% "25-1") , ]

#with nurse festuca
a <- which(site25_plot1$microsite_rep == "25-1-Festuca pallescens-1")[1]
b <- which(site25_plot1$microsite_rep == "25-1-Bare-2")[5] 

c <- which(site25_plot1$microsite_rep == "25-1-Festuca pallescens-3")[1]
d <- which(site25_plot1$microsite_rep == "25-1-Bare-3")[3] 

e <- which(site25_plot1$microsite_rep == "25-1-Festuca pallescens-4")[1]
f <- which(site25_plot1$microsite_rep == "25-1-Bare-4")[6] 

g <- which(site25_plot1$microsite_rep == "25-1-Festuca pallescens-5")[1]
h <- which(site25_plot1$microsite_rep == "25-1-Bare-5")[4] 

i <- which(site25_plot1$microsite_rep == "25-1-Festuca pallescens-6")[1]
j <- which(site25_plot1$microsite_rep == "25-1-Bare-7")[6] 

k <- which(site25_plot1$microsite_rep == "25-1-Festuca pallescens-8")[1]
l <- which(site25_plot1$microsite_rep == "25-1-Bare-9")[2] 

m <- which(site25_plot1$microsite_rep == "25-1-Festuca pallescens-10")[1]
n <- which(site25_plot1$microsite_rep == "25-1-Bare-10")[7] 

o <- which(site25_plot1$microsite_rep == "25-1-Festuca pallescens-11")[1]
p <- which(site25_plot1$microsite_rep == "25-1-Bare-25")[7] 

festuca_25_1 <- site25_plot1[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p) , ] #with nurse festuca
festuca_25_1$new_rep <- festuca_25_1$Number.of.replicate + 25

#with nurse acaena
acaena_25_1 <- site25_plot1[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p) , ] #with nurse festuca

#repaired site 25 plot 1 
repaired_site25_plot1 <- rbind(acaena_25_1, festuca_25_1)

##In site 25 plot 2 there are 2 nurses, Festuca pallescens and mulinum spinosum each with reps 1-25
#reassign rep numbers so that Mulinum follows on from festuca
site25_plot2 <- chile[which(chile$microsite_rep %like% "25-2") , ]

#with nurse mulinum
a <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-1")[1]
b <- which(site25_plot2$microsite_rep == "25-2-Bare-1")[4] 

c <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-2")[1]
d <- which(site25_plot2$microsite_rep == "25-2-Bare-7")[4] 

e <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-8")[1]
f <- which(site25_plot2$microsite_rep == "25-2-Bare-9")[1] 

g <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-10")[1]
h <- which(site25_plot2$microsite_rep == "25-2-Bare-10")[2] 

i <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-11")[1]
j <- which(site25_plot2$microsite_rep == "25-2-Bare-11")[1] 

k <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-12")[1]
l <- which(site25_plot2$microsite_rep == "25-2-Bare-13")[1] 

m <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-14")[1]
n <- which(site25_plot2$microsite_rep == "25-2-Bare-14")[2]

o <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-15")[1]
p <- which(site25_plot2$microsite_rep == "25-2-Bare-16")[2] 

q <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-17")[1]
r <- which(site25_plot2$microsite_rep == "25-2-Bare-17")[1] 

s <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-18")[1]
t <- which(site25_plot2$microsite_rep == "25-2-Bare-19")[1] 

u <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-20")[1]
v <- which(site25_plot2$microsite_rep == "25-2-Bare-21")[3] 

w <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-22")[1]
x <- which(site25_plot2$microsite_rep == "25-2-Bare-22")[2] 

y <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-23")[1]
z <- which(site25_plot2$microsite_rep == "25-2-Bare-23")[3]

aa <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-24")[1]
bb <- which(site25_plot2$microsite_rep == "25-2-Bare-24")[1] 

cc <- which(site25_plot2$microsite_rep == "25-2-Mulinum spinosum-25")[1]
dd <- which(site25_plot2$microsite_rep == "25-2-Bare-25")[3] 

mulinum_25_2 <- site25_plot2[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, 
                               aa:bb, cc:dd) , ] #with nurse mulinum
mulinum_25_2$new_rep <- mulinum_25_2$Number.of.replicate +25

festuca_25_2 <- site25_plot2[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, 
                               aa:bb, cc:dd) , ] #with nurse festuca

#repaired site 25 plot 2
repaired_site25_plot2 <- rbind(festuca_25_2, mulinum_25_2)

##In site 25 plot 3 there are 2 nurses, Festuca pallescens and mulinum spinosum each with reps 1-25
#reassign rep numbers so that Mulinum follows on from festuca
site25_plot3 <- chile[which(chile$microsite_rep %like% "25-3") , ]

festuca_rep <- site25_plot3[which(site25_plot3$microsite_rep %like% "25-3-Festuca"), which(colnames(site25_plot3) == "microsite_rep")]
unique(festuca_rep)

mulinum_rep <- site25_plot3[which(site25_plot3$microsite_rep %like% "25-3-Mulinum"), which(colnames(site25_plot3) == "microsite_rep")]
unique(mulinum_rep)

#with nurse mulinum
a <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-1")[1]
b <- which(site25_plot3$microsite_rep == "25-3-Bare-2")[1] 

c <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-3")[1]
d <- which(site25_plot3$microsite_rep == "25-3-Bare-4")[3] 

e <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-5")[1]
f <- which(site25_plot3$microsite_rep == "25-3-Bare-5")[3] 

g <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-6")[1]
h <- which(site25_plot3$microsite_rep == "25-3-Bare-6")[5] 

i <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-7")[1]
j <- which(site25_plot3$microsite_rep == "25-3-Bare-9")[1] 

k <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-10")[1]
l <- which(site25_plot3$microsite_rep == "25-3-Bare-12")[3]

m <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-13")[1]
n <- which(site25_plot3$microsite_rep == "25-3-Bare-14")[1] 

o <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-15")[1]
p <- which(site25_plot3$microsite_rep == "25-3-Bare-15")[4] 

q <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-16")[1]
r <- which(site25_plot3$microsite_rep == "25-3-Bare-18")[3] 

s <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-19")[1]
t <- which(site25_plot3$microsite_rep == "25-3-Bare-22")[1] 

u <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-26")[1]
v <- which(site25_plot3$microsite_rep == "25-3-Bare-26")[1] 

w <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-23")[1]
x <- which(site25_plot3$microsite_rep == "25-3-Bare-23")[2]

y <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-24")[1]
z <- which(site25_plot3$microsite_rep == "25-3-Bare-24")[2] 

aa <- which(site25_plot3$microsite_rep == "25-3-Mulinum spinosum-25")[1]
bb <- which(site25_plot3$microsite_rep == "25-3-Bare-25")[1] 

mulinum_25_3 <- site25_plot3[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, 
                               aa:bb) , ] #with nurse mulinum
mulinum_25_3$new_rep <- mulinum_25_3$Number.of.replicate +25

festuca_25_3 <- site25_plot3[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, 
                               aa:bb) , ] #with nurse festuca

#repaired site 25 plot 3
repaired_site25_plot3 <- rbind(festuca_25_3, mulinum_25_3)


#join all the repaired tables together and update the replicate number
chile <- rbind(repaired_site25_plot1, repaired_site25_plot2, repaired_site25_plot3)
chile$Number.of.replicate <- chile$new_rep
chile <- chile[ , - which(colnames(chile) == "new_rep")]

#Update microsite_rep
country <- chile
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
chile$microsite_rep <- paste(code, code2, sep = "-")


#Look for strange entries in "Species within quadrat
sort(unique(chile$Species.within.quadrat))
#Replace - with NA in Species within quadrat
chile[which(chile$Species.within.quadrat == "-"), which(colnames(chile) == "Species.within.quadrat")] <- NA
#There are two different spellings of Acaena integerrima, standardise them
chile[chile$Species.within.quadrat %like% "integerrima" , which(colnames(chile) == "Species.within.quadrat") ] <- "Acaena integerrima"
#There are two different spellings of Mulinum spinosum, standardise them
chile[chile$Species.within.quadrat %like% "spinosum" , which(colnames(chile) == "Species.within.quadrat") ] <- "Mulinum spinosum"



##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the chilean plots
chile_namedat <- namedat[which(namedat$ID %in% c(unique(chile$ID))) , ]
#only NA's in the tofix column, nothong to change

###Did they use multiple quadrats to make up the area of a nurse?
plotlist <- c(unique(chile[which(!is.na(chile$Quadrat)) , ]$ID))
###Yes, in all plots (83,84,85) they sometimes used multiple quadrats to make up the area of a nurse

##We need to get the percent cover of each species over the whole microsite area
##Make a table with average cover of each species in each microsite that used multiple quadrats

for (i in 1:length(plotlist)) {
  #isolate one plot
  plot <- chile[which(chile$ID == plotlist[i]) , ]
  
  replist <- c(unique(plot$Number.of.replicate))
  
  for (r in 1:length(replist)) {
    #Isolate the bare microsite of each replicate
    sub <- plot[which(plot$Number.of.replicate == replist[r] & plot$Microsite == 1) , ]
    
    #now we need to get the average % cover of each species
    #This is the sum of species % cover, divided by the total no of quadrats sampled to make up the bare microsite
    
    if (i + r == 2) { #only make a new dataframe for the first replicate in the first plot
      
      sp_cover <- as.data.frame(tapply(sub$Cover, sub$Species.within.quadrat, FUN = sum))
      colnames(sp_cover) <- "sum_percent_cover"
      sp_cover$spname <- rownames(sp_cover)
      rownames(sp_cover) <- c(1:nrow(sp_cover))
      #get the total no of quadrats sampled to make up the microsite
      sp_cover$nquadrats <-  max(sub$Quadrat)
      #now divide the sum by the number of quadrats
      sp_cover$avg_percent_cover <- sp_cover$sum_percent_cover / sp_cover$nquadrats
      #Add the ID variable and the replicate number
      sp_cover$ID <- plotlist[i]
      sp_cover$Number.of.replicate <- replist[r]
      sp_cover$Microsite <- sub$Microsite[1]
      
    } else { #for subsequent runs we rbind to the table we made the first time
      
      if(length(which(is.na(sub$Species.within.quadrat) == TRUE)) == nrow(sub)) { #if species within quadrat is empty, tapply won't work, so just make a table with an NA cover value
      
        temp <- data.frame(sum_percent_cover = NA, spname = NA, nquadrats = max(sub$Quadrat), 
                           avg_percent_cover = NA, ID = plotlist[i], Number.of.replicate = replist[r], 
                           Microsite = sub$Microsite[1])
      
      } else { #only do tapply if there are entries in species within quadrat
                           
        temp <- as.data.frame(tapply(sub$Cover, sub$Species.within.quadrat, FUN = sum))
      colnames(temp) <- "sum_percent_cover"
      temp$spname <- rownames(temp)
      rownames(temp) <- c(1:nrow(temp))
      #get the total no of quadrats sampled to make up the microsite
      temp$nquadrats <-  max(sub$Quadrat)
      #now divide the sum by the number of quadrats
      temp$avg_percent_cover <- temp$sum_percent_cover / temp$nquadrats
      #Add the ID variable and the replicate number
      temp$ID <- plotlist[i]
      temp$Number.of.replicate <- replist[r]
      temp$Microsite <- sub$Microsite[1]
      }
      
      sp_cover <- rbind(sp_cover, temp)
    }
  }
} ##where there is NA in the nquadrat column, there are no entries in the quadrat column of the plot. 
###Thus, multiple quadrats were not used in these microsites

###Now we need to delete the duplicate entries in a microsite, and replace the cover values with the values from the sp_cover table
problem_plots <- chile #make a copy to work with
length(which(is.na(problem_plots$Cover) == TRUE)) #1 NA values in the cover column

# Filter out duplicate species names within each microsite of each replicate
filtered_problem_plots <- problem_plots %>%
  group_by(ID,Number.of.replicate, Microsite) %>%
  filter(!duplicated(Species.within.quadrat))

length(which(is.na(filtered_problem_plots$Cover) == TRUE)) #1 NA values in the cover column
#They just forgot to record a cover value for this sp


##Now add the cover values from sp_cover to plot196_197
corrected_problem_plots <- merge(filtered_problem_plots, sp_cover, by.x = c("ID", "Number.of.replicate", "Microsite", "Species.within.quadrat"), 
                               by.y = c("ID", "Number.of.replicate", "Microsite", "spname"), all.x = TRUE, all.y = FALSE, no.dups = TRUE)

##If there is a value in the avg_percent_cover column, it must replace the value in the Cover column
#make a new cover column to do this in
corrected_problem_plots$cover_corrected <- NA

for(t in 1:nrow(corrected_problem_plots)) {
  if(is.na(corrected_problem_plots[t , ]$avg_percent_cover) == TRUE) { 
    #if there is no value in avg_percent cover, keep the original Cover value
    corrected_problem_plots[t , ]$cover_corrected <- corrected_problem_plots[t , ]$Cover
  } else {
    #if there is a valu ein avg_percent_cover, assign that as the cover for the species
    corrected_problem_plots[t , ]$cover_corrected <- corrected_problem_plots[t , ]$avg_percent_cover
  }
}

#Last thing do is remove the uneccessary columns, and remove NA's that are left in microsites where there are other species entries
corrected_problem_plots <- 
  corrected_problem_plots[ , -which(colnames(corrected_problem_plots) %in% 
                                    c("Cover", "sum_percent_cover", "nquadrats", "avg_percent_cover"))]
#Rename the cover_corrected column,
colnames(corrected_problem_plots)[17] <- "Cover"

#and remove NA's that are left in microsites where there are other species entries
#(These are left from quadrats that were sampled to make up nurse area, but had no species in them)
corrected_problem_plots <- corrected_problem_plots %>%
  group_by(ID, Number.of.replicate, Microsite) %>%
  filter(!(is.na(Species.within.quadrat) & n() > 1) | !is.na(Species.within.quadrat))

##Lastly, rbind these corrected plots to the rest of the data
chile <- corrected_problem_plots
##NOW THE QUADRAT COLUMN IS NONSENSICAL, DO NOT USE IT IN ANY ANALYSES





####Fixing China Chong####
#Change the name of the Cover column
colnames(chinachong)[which(colnames(chinachong) == "Cover....")] <- "Cover"

#The species names in ID_Microsite and Species within quadrat have author names, remove these
#Species within quadrat
chinachong[c("target_genus", "target_species", "author1", "author2", "author3")] <- str_split_fixed(chinachong$Species.within.quadrat, " ", 5)
chinachong$Species.within.quadrat <- paste(chinachong$target_genus, chinachong$target_species, sep = " " )
chinachong <- chinachong[, -c(18:22)] #remove the genus, species and author columns
#ID MIcrosite
chinachong[c("nurse_genus", "nurse_species", "author1", "author2", "author3")] <- str_split_fixed(chinachong$ID_Microsite, " ", 5)
chinachong$ID_Microsite <- paste(chinachong$nurse_genus, chinachong$nurse_species, sep = " " )
chinachong <- chinachong[, -c(18:22)] #remove the genus, species and author columns

#Replace "bare " with "Bare"in ID_MIcrosite
chinachong[which(chinachong$ID_Microsite == "bare "), which(colnames(chinachong) == "ID_Microsite")] <- "Bare"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- chinachong
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
chinachong$microsite_rep <- paste(code, code2, sep = "-")

#In site 29 plot 2 there are two nurses each with replicate numbers 1-25.
#Change the replicate numbers for the second nurse, Achnatherum splendens to range from 26-50
#Get the range to fix
lower <- which(chinachong$microsite_rep == "29-2-Achnatherum splendens-1")
upper <- which(chinachong$microsite_rep == "29-2-Bare-25")
range <- c(lower[1]:upper[length(upper)])
chinachong$new_rep <- chinachong$Number.of.replicate
#The rows to fix are 1281:1555
chinachong$new_rep[range] <- chinachong$Number.of.replicate[range] +25

#In site 30 plot 2 there are two nurses each with replicate numbers 1-25.
#Change the replicate numbers for the second nurse, Artemisia desertorum to range from 26-50
#Get the range to fix
lower <- which(chinachong$microsite_rep == "30-2-Artemisia desertorum-1")
upper <- which(chinachong$microsite_rep == "30-2-Bare-25")
range <- c(lower[1]:upper[length(upper)])
#The rows to fix are 464:573
chinachong$new_rep[range] <- chinachong$Number.of.replicate[range] +25

#In site 30 plot 3 there are two nurses each with replicate numbers 1-25.
#Change the replicate numbers for the second nurse, Salix cheilophila to range from 26-50
#Get the range to fix
lower <- which(chinachong$microsite_rep == "30-3-Salix cheilophila-1")
upper <- which(chinachong$microsite_rep == "30-3-Bare-25")
range <- c(lower[1]:upper[length(upper)])
#The rows to fix are 102:234
chinachong$new_rep[range] <- chinachong$Number.of.replicate[range] +25

#In site 30 plot 4 there are two nurses each with replicate numbers 1-25.
#Change the replicate numbers for the second nurse, Caragana Korshinski to range from 26-50
#Get the range to fix
lower <- which(chinachong$microsite_rep == "30-4-Caragana Korshinskii-1")
upper <- which(chinachong$microsite_rep == "30-4-Bare-25")
range <- c(lower[1]:upper[length(upper)])
#The rows to fix are 654:763
chinachong$new_rep[range] <- chinachong$Number.of.replicate[range] +25

#There is no bare microsite in site 28, plot 1 replicate 12.
#Delete this entry
chinachong <- chinachong[-which(chinachong$microsite_rep == "28-1-Juniperus formosana-12") , ]

#Update the "number of replicate" and "microsite_rep"columns
chinachong$Number.of.replicate <- chinachong$new_rep
chinachong <- chinachong[, - which(colnames(chinachong) == "new_rep")]
country <- chinachong
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
chinachong$microsite_rep <- paste(code, code2, sep = "-")


#Look for strange entries in "Species within quadrat
sort(unique(chinachong$Species.within.quadrat))
#replace " " with NA
chinachong[which(chinachong$Species.within.quadrat == " "), which(colnames(chinachong) == "Species.within.quadrat")] <- NA


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the australian plots
chinachong_namedat <- namedat[which(namedat$ID %in% c(unique(chinachong$ID))) , ]
#get names that must be changed
chinachong_namedat <- chinachong_namedat[which(!is.na(chinachong_namedat$tofix)) , ]

#Tribulus terrester
chinachong[chinachong$Species.within.quadrat %like% "Tribulus terrester" , which(colnames(chinachong) == "Species.within.quadrat") ] <- "Tribulus terrestris"
#Oxytropis psamocharis
chinachong[chinachong$Species.within.quadrat %like% "Oxytropis psamocharis" , which(colnames(chinachong) == "Species.within.quadrat") ] <- "Oxytropis psammocharis"
#Euphorbia Esula
chinachong[chinachong$Species.within.quadrat %like% "Euphorbia Esula" , which(colnames(chinachong) == "Species.within.quadrat") ] <- "Euphorbia esula"
#Caragana Korshinskii
chinachong[chinachong$Species.within.quadrat %like% "Caragana Korshinskii" , which(colnames(chinachong) == "Species.within.quadrat") ] <- "Caragana korshinskii"
chinachong[chinachong$ID_Microsite %like% "Caragana Korshinskii" , which(colnames(chinachong) == "ID_Microsite") ] <- "Caragana korshinskii"


####Fixing China Xin####
#Change the name of the Cover column
colnames(chinaxin)[which(colnames(chinaxin) == "Cover....")] <- "Cover"

#Replace "bare" with "Bare"in ID_MIcrosite
chinaxin[which(chinaxin$ID_Microsite == "bare"), which(colnames(chinaxin) == "ID_Microsite")] <- "Bare"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- chinaxin
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
chinaxin$microsite_rep <- paste(code, code2, sep = "-")

#delete the first row, it doesn't have any info
chinaxin <- chinaxin[-1, ]


##In site 34 plot 2 there are multiple nurses in a plot, but different nurses never have the same replicate number, so I won't reassign repnumbers
chinaxin$new_rep <- chinaxin$Number.of.replicate
repaired_site34_plot2 <- chinaxin[which(chinaxin$microsite_rep %like% "34-2") , ]

##In site 34 plot 3 there are 2 nurses, Ceretoides latens (1-25), Haloxylon ammodendron (1-25)
#renumber haloxylon to follow on from ceretoides
site34_plot3 <- chinaxin[which(chinaxin$microsite_rep %like% "34-3") , ]

#with nurse haloxylon
a <- which(site34_plot3$microsite_rep == "34-3-Haloxylon ammodendron-1")[1]
b <- which(site34_plot3$microsite_rep == "34-3-Bare-1")[2]

c <- which(site34_plot3$microsite_rep == "34-3-Haloxylon ammodendron-2")[1]
d <- which(site34_plot3$microsite_rep == "34-3-Bare-3")[2]

e <- which(site34_plot3$microsite_rep == "34-3-Haloxylon ammodendron-5")[1]
f <- which(site34_plot3$microsite_rep == "34-3-Bare-7")[1]

g <- which(site34_plot3$microsite_rep == "34-3-Haloxylon ammodendron-6")[1]
h <- which(site34_plot3$microsite_rep == "34-3-Bare-18")[1]

i <- which(site34_plot3$microsite_rep == "34-3-Haloxylon ammodendron-19")[1]
j <- which(site34_plot3$microsite_rep == "34-3-Bare-25")[1]


halo_34_3 <- site34_plot3[c(a:b, c:d, e:f, g:h, i:j) , ]
halo_34_3$new_rep <- halo_34_3$Number.of.replicate + 25

#with nurse ceretoides
cere_34_3 <- site34_plot3[-c(a:b, c:d, e:f, g:h, i:j) , ]

#repaired site 34 plot 3
repaired_site34_plot3 <- rbind(cere_34_3, halo_34_3)

#Put the repaired sites back together
chinaxin <- rbind(repaired_site34_plot2, repaired_site34_plot3)
#Update the replicate numbers and microsite rep
chinaxin$Number.of.replicate <- chinaxin$new_rep
chinaxin <- chinaxin[ , -which(colnames(chinaxin) == "new_rep")]

country <- chinaxin
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
chinaxin$microsite_rep <- paste(code, code2, sep = "-")

#Look for strange entries in "Species within quadrat
sort(unique(chinaxin$Species.within.quadrat))


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the chinaxin plots
chinaxin_namedat <- namedat[which(namedat$ID %in% c(unique(chinaxin$ID))) , ]
#get names that must be changed
chinaxin_namedat <- chinaxin_namedat[which(!is.na(chinaxin_namedat$tofix)) , ]

#Ceretoides latens
chinaxin[chinaxin$ID_Microsite %like% "Ceretoides latens" , which(colnames(chinaxin) == "ID_Microsite") ] <- "Ceratoides latens"
#Seipa glareosa
chinaxin[chinaxin$Species.within.quadrat %like% "Seipa glareosa" , which(colnames(chinaxin) == "Species.within.quadrat") ] <- "Stipa glareosa"
#Anabasisi aphylla
chinaxin[chinaxin$Species.within.quadrat %like% "Anabasisi aphylla" , which(colnames(chinaxin) == "Species.within.quadrat") ] <- "Anabasis aphylla"


#In site 34 plot 3 there are multiple entries of Seriphidium borotalense in some bare microsites
site34_plot3 <- chinaxin[which(chinaxin$microsite_rep %like% "34-3") , ]
#isolate entries of Seriphidium borotalense
ser_brot <- site34_plot3[which(site34_plot3$Species.within.quadrat == "Seriphidium borotalense") , ]

#Loop to create a dataframe showing which replicates have more than two entries
reps <- unique(ser_brot$Number.of.replicate)
ser_brot_occurrences <- data.frame(matrix(nrow = 1, ncol = 2))
colnames(ser_brot_occurrences) <- c("replicate", "n_entries")

for (i in 1:length(reps)) {
  one_rep <- ser_brot[which(ser_brot$Number.of.replicate == reps[i]) , ]
  
  if(i == 1) {
    ser_brot_occurrences$replicate <- reps[i]
    ser_brot_occurrences$n_entries <- nrow(one_rep)
  } else {
    temp <- cbind(reps[i], nrow(one_rep))
    colnames(temp) <- c("replicate", "n_entries")
    ser_brot_occurrences <- rbind(ser_brot_occurrences, temp)
  }
}

#get the replicates with more than 2 entries
problem_reps <- c(ser_brot_occurrences[which(ser_brot_occurrences$n_entries > 2) , ]$replicate)
test <- site34_plot3[which(site34_plot3$Number.of.replicate == 34) , ]
#the last two entries in rep 34 can be deleted. 

#get the entries to delete
d_index1 <- which(ser_brot$Number.of.replicate %in% problem_reps)[3:4] #delete the 67th and 68th occurrence of NAsella tenuis

#add a variable to site6_plot3 that has the cumulative occurrences species
site34_plot3 <- site34_plot3 %>%
  group_by(Species.within.quadrat) %>%
  mutate(cumulative_occurrences = row_number()) %>%
  ungroup()

#get the rows to delete from site34_plot3
#the rows must be of species Seriphidium brotoalense, and must have the cumulative occurrences in c(d_index1)
# Define the conditions
species_condition <- site34_plot3$Species.within.quadrat == "Seriphidium borotalense"
occurrences_condition <- site34_plot3$cumulative_occurrences %in% c(d_index1)

# Combine conditions using the & (AND) operator
combined_condition <- species_condition & occurrences_condition

# Get row numbers that meet the combined condition, and delete them
corrected_site34_plot3 <- site34_plot3[-c(which(combined_condition)) , ]
#remove the cumulative_occurrences column
corrected_site34_plot3 <- corrected_site34_plot3[ , -which(colnames(corrected_site34_plot3) == "cumulative_occurrences")]

#Rbind the corrected_site6_plot3 to the rest of the argentina data
rest <- chinaxin[-which(chinaxin$microsite_rep %like% "34-3"), ]
chinaxin <- rbind(rest, corrected_site34_plot3)


####Fixing iranabedi####
#Change the name of the Cover column
colnames(iranabedi)[which(colnames(iranabedi) == "Cover....")] <- "Cover"

#Replace "bare" with "Bare"in ID_MIcrosite
iranabedi[which(iranabedi$ID_Microsite == "bare"), which(colnames(iranabedi) == "ID_Microsite")] <- "Bare"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- iranabedi
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
iranabedi$microsite_rep <- paste(code, code2, sep = "-")

#In site 41 plot 1 there is a second nurse Salsola arbusculiformis in replicate 1. remove this row
iranabedi <- iranabedi[-which(iranabedi$microsite_rep == "41-1-Salsola arbusculiformis-1") , ]
#In site 41 plot 2 there is a second nurse Salsola arbusculiformis in replicate 1. remove this row
iranabedi <- iranabedi[-which(iranabedi$microsite_rep == "41-2-Salsola arbusculiformis-1") , ]

#In site 41 plot 3 there are two nurses each with replicate numbers 1-25.
#Change the replicate numbers for the second nurse, Salsola arbusculiformis to range from 26-50
#Get the range to fix
lower <- which(iranabedi$microsite_rep == "41-3-Salsola arbusculiformis-1")
upper <- which(iranabedi$microsite_rep == "41-3-Bare-25")
range <- c(lower[1]:upper[length(upper)])
iranabedi$new_rep <- iranabedi$Number.of.replicate
iranabedi$new_rep[range] <- iranabedi$Number.of.replicate[range] + 25

#In site 42 plot 1 there are two nurses 
#Rhamnus pallasii has 20 reps and Paliurus spina has 18 reps
#Change the replicate numbers for the second nurse, Paliurus spina to range from 21-38
#Get the range to fix
lower <- which(iranabedi$microsite_rep == "42-1-Paliurus spina-1")
upper <- which(iranabedi$microsite_rep == "42-1-Bare-18")
range <- c(lower[1]:upper[length(upper)])
iranabedi$new_rep[range] <- iranabedi$Number.of.replicate[range] +20

#In site 42 plot 2 there are two nurses 
#Rhamnus pallasii has 19 reps and Paliurus spina has 20 reps
#Change the replicate numbers for the second nurse, Paliurus spina to range from 20-39
#Get the range to fix
lower <- which(iranabedi$microsite_rep == "42-2-Paliurus spina-1")
upper <- which(iranabedi$microsite_rep == "42-2-Bare-20")
range <- c(lower[1]:upper[length(upper)])
iranabedi$new_rep[range] <- iranabedi$Number.of.replicate[range] +19

#In site 42 plot 3 there are two nurses 
#Rhamnus pallasii has 20 reps and Paliurus spina has 20 reps
#Change the replicate numbers for the second nurse, Paliurus spina to range from 21-40
#Get the range to fix
lower <- which(iranabedi$microsite_rep == "42-3-Paliurus spina-1")
upper <- which(iranabedi$microsite_rep == "42-3-Bare-20")
range <- c(lower[1]:upper[length(upper)])
iranabedi$new_rep[range] <- iranabedi$Number.of.replicate[range] +20

#Update the "number of replicate" and "microsite_rep"columns
iranabedi$Number.of.replicate <- iranabedi$new_rep
iranabedi <- iranabedi[, - which(colnames(iranabedi) == "new_rep")]
code <- paste(iranabedi$SITE_ID, iranabedi$PLOT, sep = "-")
code2 <- paste(iranabedi$ID_Microsite, iranabedi$Number.of.replicate, sep = "-")
iranabedi$microsite_rep <- paste(code, code2, sep = "-")

#Look for strange entries in "Species within quadrat
sort(unique(iranabedi$Species.within.quadrat))
#There is a species called "5" but it has a cover and abundance so just leave it
#replace 0 with NA
iranabedi[which(iranabedi$Species.within.quadrat == "0"), which(colnames(iranabedi) == "Species.within.quadrat")] <- NA
#There are many species with multiple spellings, fix them
#"Achillea biebersteinii"
iranabedi[iranabedi$Species.within.quadrat %like% "biebersteinii" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Achillea biebersteinii"
#"Astragalus sp"
iranabedi[iranabedi$Species.within.quadrat %like% "Astragalus" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Astragalus sp."
#"Centaurea kotschyi"
iranabedi[iranabedi$Species.within.quadrat %like% "kotschyi" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Centaurea kotschyi"
#"Cerasus pseudoprostrata"
iranabedi[iranabedi$Species.within.quadrat %like% "pseudoprostrata" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Cerasus pseudoprostrata"
#"Convolvulus commutatus"
iranabedi[iranabedi$Species.within.quadrat %like% "commutatus" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Convolvulus commutatus"
#"Cousinia decipiens"
iranabedi[iranabedi$Species.within.quadrat %like% "decipiens" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Cousinia decipiens"
#"Crataegus azarolus"
iranabedi[iranabedi$Species.within.quadrat %like% "Crataegus" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Crataegus azarolus"
#"Dianthus orientalis"
iranabedi[iranabedi$Species.within.quadrat %like% "orientalis" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Dianthus orientalis"
#"Echinops ritrodes"
iranabedi[iranabedi$Species.within.quadrat %like% "ritrodes" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Echinops ritrodes"
#"Inula oculus-christi"
iranabedi[iranabedi$Species.within.quadrat %like% "oculus" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Inula oculus-christi"
#"Phlomis cancellata"
iranabedi[iranabedi$Species.within.quadrat %like% "cancel" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Phlomis cancellata"
#"Rhamnus pallasii"
iranabedi[iranabedi$Species.within.quadrat %like% "pallasii" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Rhamnus pallasii"
#"Stachys turcomanica"
iranabedi[iranabedi$Species.within.quadrat %like% "turcomanica" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Stachys turcomanica"
#"Stipa lessingiana"
iranabedi[iranabedi$Species.within.quadrat %like% "lessingiana" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Stipa lessingiana"


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the iranabedi plots
iranabedi_namedat <- namedat[which(namedat$ID %in% c(unique(iranabedi$ID))) , ]
#get names that must be changed
iranabedi_namedat <- iranabedi_namedat[which(!is.na(iranabedi_namedat$tofix)) , ]

#Acanthophyllum sp
iranabedi[iranabedi$Species.within.quadrat %like% "Acanthophyllum sp" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Acanthophyllum sp."
#Acantholimon sp
iranabedi[iranabedi$Species.within.quadrat %like% "Acantholimon sp" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Acantholimon sp."
#Centaurea sp
iranabedi[iranabedi$Species.within.quadrat %like% "Centaurea sp" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Centaurea sp."
#Paliurus spina
iranabedi[iranabedi$ID_Microsite %like% "Paliurus spina" , which(colnames(iranabedi) == "ID_Microsite") ] <- "Paliurus spina-christi"
#onobrychis sintenisii
iranabedi[iranabedi$Species.within.quadrat %like% "onobrychis sintenisii" , which(colnames(iranabedi) == "Species.within.quadrat") ] <- "Onobrychis sintenisii"



####Fixing Iran Farzam####
#Change the name of the Cover column
colnames(iranfarzam)[which(colnames(iranfarzam) == "Cover....")] <- "Cover"
iranfarzam$Cover <- as.numeric(iranfarzam$Cover) #make it numeric

#Replace "bare" with "Bare"in ID_MIcrosite
iranfarzam[which(iranfarzam$ID_Microsite == "bare"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Bare"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- iranfarzam
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
iranfarzam$microsite_rep <- paste(code, code2, sep = "-")

##Fix multiple spellings in nurse names
unique(iranfarzam$ID_Microsite)
#There is a space after the name of the nurse Astragalus jolderensis, remove it
iranfarzam[which(iranfarzam$ID_Microsite == "Astragalus jolderensis "), which(colnames(iranfarzam) == "ID_Microsite")] <- "Astragalus jolderensis"
#There is a space after the name of the nurse Paliurus spina-christi, remove it
iranfarzam[which(iranfarzam$ID_Microsite %like% "spina-christi"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Paliurus spina-christi"
#There is two spellings of the nurse Astragalus chrysostachys, standardise them
iranfarzam[which(iranfarzam$ID_Microsite %like% "galus chrysostachys"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Astragalus chrysostachys"
##There are multiple spellings of Artemisia kopetdaghensis
iranfarzam[which(iranfarzam$ID_Microsite %like% "daghensis"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Artemisia kopetdaghensis"
##The nurses Astragalus and astragalus must be Astragalus chrysostachys, because they are all in the same plot
iranfarzam[which(iranfarzam$ID_Microsite == "Astragalus"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Astragalus chrysostachys"
iranfarzam[which(iranfarzam$ID_Microsite == "astragalus"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Astragalus chrysostachys"
#Pistacia vera
iranfarzam[which(iranfarzam$ID_Microsite %like% "istacia vera"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Pistacia vera"
#Zygophyllum atriplicoides
iranfarzam[which(iranfarzam$ID_Microsite %like% "atriplicoides"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Zygophyllum atriplicoides"
#"Cerasus microcarpa"  
iranfarzam[which(iranfarzam$ID_Microsite %like% "microcarpa"), which(colnames(iranfarzam) == "ID_Microsite")] <- "Cerasus microcarpa"


#remove rows with no entry in the ID_Microsite column
#first inspect
iranfarzam[which(iranfarzam$ID_Microsite == "") , ]
#The last record, row 4081 has information in the Species within quadrat column. They just forgot to enter a microsite ID
#Rename the Microsite ID of row 4081 to Bare
duds <- which(iranfarzam$ID_Microsite == "")
length(duds)
iranfarzam[duds[length(duds)] , which(colnames(iranfarzam) == "ID_Microsite")] <- "Bare"
#Now we can delete therest of the rows that are really empty
iranfarzam <- iranfarzam[-duds[1:6] , ]

#Redo the microsite_rep to reflect the change above
country <- iranfarzam
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
iranfarzam$microsite_rep <- paste(code, code2, sep = "-")


#overwrite the microsite variable to make sure it's correct
records <- c(1:nrow(iranfarzam))
for (k in records) {
  if (iranfarzam[k, which(colnames(iranfarzam) == "ID_Microsite")] == "Bare") {
    iranfarzam[k, which(colnames(iranfarzam) == "Microsite")] <- 1
  } else {
    iranfarzam[k, which(colnames(iranfarzam) == "Microsite")] <- 2
  }
}

##IN site 43 plot 1 there are 3 nurses, each with reps 1-25
#Acantholimon erinaceum, Artemisia kopetdaghensis, Astragalus chrysostachys
#Assign new reps to follow on from the previous reps
iranfarzam$new_rep <- iranfarzam$Number.of.replicate
site43_plot1 <- iranfarzam[which(iranfarzam$microsite_rep %like% "43-1") , ]

#with nurse Artemisia kopetdaghensis
a <- which(site43_plot1$microsite_rep == "43-1-Artemisia kopetdaghensis-1")[1]
b <- which(site43_plot1$microsite_rep == "43-1-Bare-25")[3]
artemisia_43_1 <- site43_plot1[c(a:b) , ]
artemisia_43_1$new_rep <- artemisia_43_1$Number.of.replicate + 25

#with nurse astralagus
c <- which(site43_plot1$microsite_rep == "43-1-Astragalus chrysostachys-1")[1]
d <- nrow(site43_plot1)
astragalus_43_1 <- site43_plot1[c(c:d) , ]
astragalus_43_1$new_rep <- astragalus_43_1$Number.of.replicate + 25 + 25

#with nurse acantholimon
acantholimon_43_1 <- site43_plot1[-c(a:b , c:d) , ]

#repaired site 43 plot 1 
repaired_site43_plot1 <- rbind(acantholimon_43_1, artemisia_43_1, astragalus_43_1)


##In site 43 plot 2 there are 3 nurses, each with reps 1-25
#Cerasus microcarpa, Astragalus jolderensis, Acantholimon erinaceum
site43_plot2 <- iranfarzam[which(iranfarzam$microsite_rep %like% "43-2") , ]

#with nurse astragalus
#!!! Reps 2,4,6,7,9,10 do not have bare microsites with nurse astragalus
#Create empty bare microsites in a table to append to the data later
column_names <- colnames(site43_plot2)
adbare <- data.frame(matrix(nrow = 6, ncol = length(column_names)))
colnames(adbare) <- column_names
#Get values to fill in
test <- site43_plot2[which(site43_plot2$microsite_rep == "43-2-Astragalus jolderensis-1") , ][1 ,]
#The first 7 columns have no change in the data
adbare[ , c(1:7)] <- test[ , (1:7)]
#Change the microsite information
adbare$Microsite <- 1
adbare$ID_Microsite <- "Bare"
#Now add the replicate info
adbare[1, c(13, 18, 19)] <- list(2, "43-2-Bare-2", 2) #rep 2
adbare[2, c(13, 18, 19)] <- list(4, "43-2-Bare-4", 4) #rep 4
adbare[3, c(13, 18, 19)] <- list(6, "43-2-Bare-6", 6) #rep 6
adbare[4, c(13, 18, 19)] <- list(7, "43-2-Bare-7", 7) #rep 7
adbare[5, c(13, 18, 19)] <- list(9, "43-2-Bare-9", 9) #rep 9
adbare[6, c(13, 18, 19)] <- list(10, "43-2-Bare-10", 10) #rep 10

#!!! Reps 8 and 11 do not have nurse microsites
column_names <- colnames(site43_plot2)
adnurse <- data.frame(matrix(nrow = 2, ncol = length(column_names)))
colnames(adnurse) <- column_names
#The first 7 columns have no change in the data
adnurse[ , c(1:7)] <- test[ , (1:7)]
#Change the microsite information
adnurse$Microsite <- 2
adnurse$ID_Microsite <- "Astragalus jolderensis"
#Now add the replicate info
adnurse[1, c(13, 18, 19)] <- list(8, "43-2-Astragalus jolderensis-8", 8) #rep 8
adnurse[2, c(13, 18, 19)] <- list(11, "43-2-Astragalus jolderensis-11", 11) #rep 11

#isolate nurse astragalus
a <- which(site43_plot2$microsite_rep == "43-2-Astragalus jolderensis-1")[1]
b <- which(site43_plot2$microsite_rep == "43-2-Bare-25")[4]
astragalus_43_2 <- site43_plot2[c(a:b) , ]
astragalus_43_2 <- rbind(astragalus_43_2, adbare, adnurse)
astragalus_43_2$new_rep <- astragalus_43_2$Number.of.replicate + 25

#with nurse acantholimon
#!!!Rep 21 has no bare microsite
#Create an empty bare microsite for acantholimon rep 4
add_bare <- site43_plot2[which(site43_plot2$microsite_rep == "43-2-Acantholimon erinaceum-21") , ][1 ,] #get a row to work with
#Change the Microsite and species within quadrat info
add_bare[8:19] <- list(1, "Bare", NA, NA, NA, 21, NA, NA, NA, NA, "43-2-Bare-21", 21)

c <- which(site43_plot2$microsite_rep == "43-2-Acantholimon erinaceum-1")[1]
d <- nrow(site43_plot2)
acantholimon_43_2 <- site43_plot2[c(c:d) , ]
acantholimon_43_2[(nrow(acantholimon_43_2) + 1) , ] <- add_bare
acantholimon_43_2$new_rep <- acantholimon_43_2$Number.of.replicate + 25 + 25

#with nurse cerasus
cerasus_43_2 <- site43_plot2[-c(a:b , c:d) , ]

#repaired site 43 plot 2
repaired_site43_plot2 <- rbind(cerasus_43_2, astragalus_43_2, acantholimon_43_2)


##In site 43 plot 3 there are 2 nurses, Artemisia aucheri (1-50) and Astragalus chrysostachys (1-50)
site43_plot3 <- iranfarzam[which(iranfarzam$microsite_rep %like% "43-3") , ]

#with nurse astragalus
a <- which(site43_plot3$microsite_rep == "43-3-Astragalus chrysostachys-1")[1]
b <- nrow(site43_plot3)
astragalus_43_3 <- site43_plot3[c(a:b) , ]
astragalus_43_3$new_rep <- astragalus_43_3$Number.of.replicate + 50

#with nurse artemisia
artemisia_43_3 <- site43_plot3[-c(a:b) , ]

#repaired site 43 plot 3
repaired_site43_plot3 <- rbind(artemisia_43_3, astragalus_43_3)


##In site 43 plot 4 there are 3 nurses
#Artemisia kopetdaghensis (1-25), Astragalus chrysostachys (1-25), Cerasus microcarpa (1-25)
site43_plot4 <- iranfarzam[which(iranfarzam$microsite_rep %like% "43-4") , ]

#with nurse astragalus
a <- which(site43_plot4$microsite_rep == "43-4-Astragalus chrysostachys-1")[1]
b <- which(site43_plot4$microsite_rep == "43-4-Bare-25")[14]
astragalus_43_4 <- site43_plot4[c(a:b) , ]
astragalus_43_4$new_rep <- astragalus_43_4$Number.of.replicate + 25

#with nurse cerasus
c <- which(site43_plot4$microsite_rep == "43-4-Cerasus microcarpa-1")[1]
d <- nrow(site43_plot4)
cerasus_43_4 <- site43_plot4[c(c:d) , ]
cerasus_43_4$new_rep <- cerasus_43_4$Number.of.replicate + 25 +25

#with nurse artemisia
artemisia_43_4 <- site43_plot4[-c(a:b , c:d) , ]

#repaired site43 plot 4
repaired_site43_plot4 <- rbind(artemisia_43_4, astragalus_43_4, cerasus_43_4)


##In site 44 plot 1 there are 3 nurses
#Quercus castaneifolia (1-25), Astragalus jolderensis (1-25), and Paliurus spina-christi (1-25)
site44_plot1 <- iranfarzam[which(iranfarzam$microsite_rep %like% "44-1") , ]

#with nurse astragalus
#!!!Astragalus rep 4 has no bare microsite
#Create an empty bare microsite for astragalus rep 4
add_bare <- site44_plot1[which(site44_plot1$microsite_rep == "44-1-Astragalus jolderensis-4") , ][1 ,] #get a row to work with
#Change the Microsite and species within quadrat info
add_bare[8:19] <- list(1, "Bare", NA, NA, NA, 4, NA, NA, NA, NA, "44-1-Bare-4", 4)
#isolate astragalus
a <- which(site44_plot1$microsite_rep == "44-1-Astragalus jolderensis-1")[1]
b <- which(site44_plot1$microsite_rep == "44-1-Bare-25")[10]
astragalus_44_1 <- site44_plot1[c(a:b) , ]
astragalus_44_1[(nrow(astragalus_44_1) + 1), ] <- add_bare #add the bare microsite to the end of the table
astragalus_44_1$new_rep <- astragalus_44_1$Number.of.replicate + 25

#with nurse paliurus
c <- which(site44_plot1$microsite_rep == "44-1-Paliurus spina-christi-1")[1]
d <- nrow(site44_plot1)
paliurus_44_1 <- site44_plot1[c(c:d) , ]
paliurus_44_1$new_rep <- paliurus_44_1$Number.of.replicate + 25 + 25

#with nurse quercus
quercus_44_1 <- site44_plot1[-c(a:b, c:d) , ]

#repaired site 44 plot 1
repaired_site44_plot1 <- rbind(quercus_44_1, astragalus_44_1, paliurus_44_1)


##In site 44 plot 2 there are 3 nurses, Quercus castaneifolia (1-25), Artemisia kopetdaghensis (1-25) and Ephedra major (1-26)
site44_plot2 <- iranfarzam[which(iranfarzam$microsite_rep %like% "44-2") , ]

#with nurse artemisia
#!!!artemisia rep 2 has no bare microsite
#Create an empty bare microsite for artemisia rep 2
add_bare <- site44_plot2[which(site44_plot2$microsite_rep == "44-2-Artemisia kopetdaghensis-2") , ][1 ,] #get a row to work with
#Change the Microsite and species within quadrat info
add_bare[8:19] <- list(1, "Bare", NA, NA, NA, 2, NA, NA, NA, NA, "44-2-Bare-2", 2)

#!!!artemisia rep 4 has no nurse microsite
add_nurse <- site44_plot2[which(site44_plot2$microsite_rep == "44-2-Bare-4") , ][1 ,] #get a row to work with
add_nurse[8:19] <- list(2, "Artemisia kopetdaghensis", NA, NA, NA, 4, NA, NA, NA, NA, "44-2-Artemisia kopetdaghensis-4", 4)

a <- which(site44_plot2$microsite_rep == "44-2-Artemisia kopetdaghensis-1")[1]
b <- which(site44_plot2$microsite_rep == "44-2-Bare-25")[13]
artemisia_44_2 <- site44_plot2[c(a:b) , ]
artemisia_44_2[(nrow(artemisia_44_2) + 1) , ] <- add_bare #add the empty bare microsite
artemisia_44_2[(nrow(artemisia_44_2) + 1) , ] <- add_nurse #add the empty bare microsite
artemisia_44_2$new_rep <- artemisia_44_2$Number.of.replicate + 25

#with nurse ephedra
#!!!rep 22 has no bare microsite
add_bare <- site44_plot2[which(site44_plot2$microsite_rep == "44-2-Ephedra major -22") , ][1 ,] #get a row to work with
add_bare[8:19] <- list(1, "Bare", NA, NA, NA, 22, NA, NA, NA, NA, "44-2-Bare-22", 22)

c <- which(site44_plot2$microsite_rep == "44-2-Ephedra major -1")[1]
d <- nrow(site44_plot2)
ephedra_44_2 <- site44_plot2[c(c:d) , ]
ephedra_44_2[(nrow(ephedra_44_2) + 1) , ] <- add_bare
ephedra_44_2$new_rep <- ephedra_44_2$Number.of.replicate + 25 +25

#with nurse quercus
quercus_44_2 <- site44_plot2[-c(a:b, c:d) , ]

#repaired site 44 plot 2
repaired_site44_plot2 <- rbind(quercus_44_2, artemisia_44_2, ephedra_44_2)


##There is no plot 3 in site 44


##In site 44 plot 4 there are 2 nurses, Quercus castaneifolia (1-25) and celtis (1-25)
site44_plot4 <- iranfarzam[which(iranfarzam$microsite_rep %like% "44-4") , ]

#with nurse celtis
a <- which(site44_plot4$microsite_rep == "44-4-celtis-1")[1]
b <- nrow(site44_plot4)
celtis_44_4 <- site44_plot4[c(a:b) , ]
celtis_44_4$new_rep <- celtis_44_4$Number.of.replicate + 25

#with nurse quercus
quercus_44_4 <- site44_plot4[-c(a:b) , ]

#repaired site 44 plot 1
repaired_site44_plot4 <- rbind(quercus_44_4, celtis_44_4)


##In site 45 plot 1 there is only one nurse, Pistacia vera (1-10)
site45_plot1 <- iranfarzam[which(iranfarzam$microsite_rep %like% "45-1") , ]
repaired_site45_plot1 <- site45_plot1


##In site 45 plot 2 there are three nurses, Kochia prostrata (1-25), Pistacia vera (1-25), Artemisia kopetdaghensis (1-25)
site45_plot2 <- iranfarzam[which(iranfarzam$microsite_rep %like% "45-2") , ]

#with nurse pistacia
a <- which(site45_plot2$microsite_rep == "45-2-Pistacia vera-1")[1]
b <- which(site45_plot2$microsite_rep == "45-2-Bare-25")[3]
pistacia_45_2 <- site45_plot2[c(a:b) , ]
pistacia_45_2$new_rep <- pistacia_45_2$Number.of.replicate + 25

#with nurse artemisia
c <- which(site45_plot2$microsite_rep == "45-2-Artemisia kopetdaghensis-1")[1]
d <- nrow(site45_plot2)
artemisia_45_2 <- site45_plot2[c(c:d) , ]
artemisia_45_2$new_rep <- artemisia_45_2$Number.of.replicate + 25 + 25

#with nurse kochia
kochia_45_2 <- site45_plot2[-c(a:b, c:d) , ]

#repaired site 45 plot2 
repaired_site45_plot2 <- rbind(kochia_45_2, pistacia_45_2, artemisia_45_2)


##In site 45 plot 3 there are 3 nurses, Pistacia vera (1-25), Zygophyllum atriplicoides (1-25), Artemisia kopetdaghensis (1-25)
site45_plot3 <- iranfarzam[which(iranfarzam$microsite_rep %like% "45-3") , ]

#with nurse zygophyllum
a <- which(site45_plot3$microsite_rep == "45-3-Zygophyllum atriplicoides-1")[1]
b <- which(site45_plot3$microsite_rep == "45-3-Bare-25")[2]
zygo_45_3 <- site45_plot3[c(a:b) , ]
zygo_45_3$new_rep <- zygo_45_3$Number.of.replicate + 25

#with nurse artemisia
c <- which(site45_plot3$microsite_rep == "45-3-Artemisia kopetdaghensis-1")[1]
d <- nrow(site45_plot3)
artemisia_45_3 <- site45_plot3[c(c:d) , ]
artemisia_45_3$new_rep <- artemisia_45_3$Number.of.replicate + 25 + 25

#with nurse pistacia
pistacia_45_3 <- site45_plot3[-c(a:b, c:d) , ]

#repaired site45 plot 3
repaired_site45_plot3 <- rbind(pistacia_45_3, zygo_45_3, artemisia_45_3)


###!!!!!
##IN site 45 plot 4 there are 3 nurses, Pistacia vera (1-25), Zygophyllum atriplicoides (1-25) , Artemisia kopetdaghensis (1-25)
site45_plot4 <- iranfarzam[which(iranfarzam$microsite_rep %like% "45-4") , ]

#with nurse pistacia
#!!!pistacia rep 2,4,5,11,12,17,18,19,23,24 has no bare microsite
#Create empty bare microsites in a table to append to the data later
column_names <- colnames(site45_plot4)
adbare <- data.frame(matrix(nrow = 10, ncol = length(column_names)))
colnames(adbare) <- column_names
#Get values to fill in
test <- site45_plot4[which(site45_plot4$microsite_rep == "45-4-Pistacia vera-2") , ][1 ,]
#The first 7 columns have no change in the data
adbare[ , c(1:7)] <- test[ , (1:7)]
#Change the microsite information
adbare$Microsite <- 1
adbare$ID_Microsite <- "Bare"
#Now add the replicate info
adbare[1, c(13, 18, 19)] <- list(2, "45-4-Bare-2", 2) #rep 2
adbare[2, c(13, 18, 19)] <- list(4, "45-4-Bare-4", 4) #rep 4
adbare[3, c(13, 18, 19)] <- list(5, "45-4-Bare-5", 5) #rep 5
adbare[4, c(13, 18, 19)] <- list(11, "45-4-Bare-11", 11) #rep 11
adbare[5, c(13, 18, 19)] <- list(12, "45-4-Bare-12", 12) #rep 12
adbare[6, c(13, 18, 19)] <- list(17, "45-4-Bare-17", 17) #rep 17
adbare[7, c(13, 18, 19)] <- list(18, "45-4-Bare-18", 18) #rep 18
adbare[8, c(13, 18, 19)] <- list(19, "45-4-Bare-19", 19) #rep 19
adbare[9, c(13, 18, 19)] <- list(23, "45-4-Bare-23", 23) #rep 23
adbare[10, c(13, 18, 19)] <- list(24, "45-4-Bare-24", 24) #rep 24

#isolate nurse pistacia
a <- which(site45_plot4$microsite_rep == "45-4-Pistacia vera-1")[1]
b <- which(site45_plot4$microsite_rep == "45-4-Bare-25")[5]
pistacia_45_4 <- site45_plot4[c(a:b) , ]
#add the empty bare microsites
pistacia_45_4 <- rbind(pistacia_45_4, adbare)
pistacia_45_4$new_rep <- pistacia_45_4$Number.of.replicate + 25


#with nurse zygophyllum
#!!! zygophyllum rep 1,2,3,4,5,6,7,8,9,10,11,13,15,17,18,19,20,22,24,25 has no bare microsite
#Create empty bare microsites in a table to append to the data later
column_names <- colnames(site45_plot4)
adbare_zygo <- data.frame(matrix(nrow = 20, ncol = length(column_names)))
colnames(adbare_zygo) <- column_names
#Get values to fill in
test <- site45_plot4[which(site45_plot4$microsite_rep == "45-4-Zygophyllum atriplicoides-1") , ][1 ,]
#The first 7 columns have no change in the data
adbare_zygo[ , c(1:7)] <- test[ , (1:7)]
#Change the microsite information
adbare_zygo$Microsite <- 1
adbare_zygo$ID_Microsite <- "Bare"

#Now add the replicate info
adbare_zygo[1, c(13, 18, 19)] <- list(1, "45-4-Bare-1", 1) #rep 1
adbare_zygo[2, c(13, 18, 19)] <- list(2, "45-4-Bare-2", 2) #rep 2
adbare_zygo[3, c(13, 18, 19)] <- list(3, "45-4-Bare-3", 3) #rep 3
adbare_zygo[4, c(13, 18, 19)] <- list(4, "45-4-Bare-4", 4) #rep 4
adbare_zygo[5, c(13, 18, 19)] <- list(5, "45-4-Bare-5", 5) #rep 5
adbare_zygo[6, c(13, 18, 19)] <- list(6, "45-4-Bare-6", 6) #rep 6
adbare_zygo[7, c(13, 18, 19)] <- list(7, "45-4-Bare-7", 7) #rep 7
adbare_zygo[8, c(13, 18, 19)] <- list(8, "45-4-Bare-8", 8) #rep 8
adbare_zygo[9, c(13, 18, 19)] <- list(9, "45-4-Bare-9", 9) #rep 9
adbare_zygo[10, c(13, 18, 19)] <- list(10, "45-4-Bare-10", 10) #rep 10
adbare_zygo[11, c(13, 18, 19)] <- list(11, "45-4-Bare-11", 11) #rep 11
adbare_zygo[12, c(13, 18, 19)] <- list(13, "45-4-Bare-13", 13) #rep 13
adbare_zygo[13, c(13, 18, 19)] <- list(15, "45-4-Bare-15", 15) #rep 15
adbare_zygo[14, c(13, 18, 19)] <- list(17, "45-4-Bare-17", 17) #rep 17
adbare_zygo[15, c(13, 18, 19)] <- list(18, "45-4-Bare-18", 18) #rep 18
adbare_zygo[16, c(13, 18, 19)] <- list(19, "45-4-Bare-19", 19) #rep 19
adbare_zygo[17, c(13, 18, 19)] <- list(20, "45-4-Bare-20", 20) #rep 20
adbare_zygo[18, c(13, 18, 19)] <- list(22, "45-4-Bare-22", 22) #rep 22
adbare_zygo[19, c(13, 18, 19)] <- list(24, "45-4-Bare-24", 24) #rep 24
adbare_zygo[20, c(13, 18, 19)] <- list(25, "45-4-Bare-25", 25) #rep 25

#isolate nurse zygophylum
c <- which(site45_plot4$microsite_rep == "45-4-Zygophyllum atriplicoides-1")[1]
d <- which(site45_plot4$microsite_rep == "45-4-Zygophyllum atriplicoides-25")[1]
zygophyllum_45_4 <- site45_plot4[c(c:d) , ]
#add the empty bare microsites
zygophyllum_45_4 <- rbind(zygophyllum_45_4, adbare_zygo)
zygophyllum_45_4$new_rep <- zygophyllum_45_4$Number.of.replicate + 25 + 25


#with nurse artemisia
artemisia_45_4 <- site45_plot4[-c(a:b , c:d) , ]

#repaired site 45_plot4
repaired_site45_plot4 <- rbind(artemisia_45_4, pistacia_45_4, zygophyllum_45_4)


#Bind all the repaired sites together and update the replicate numbers
iranfarzam <- rbind(repaired_site43_plot1, repaired_site43_plot2, repaired_site43_plot3, repaired_site43_plot4, 
      repaired_site44_plot1, repaired_site44_plot2, repaired_site44_plot4, 
      repaired_site45_plot1, repaired_site45_plot2, repaired_site45_plot3, repaired_site45_plot4) 
iranfarzam$Number.of.replicate <- iranfarzam$new_rep
iranfarzam <- iranfarzam[ , -which(colnames(iranfarzam) == "new_rep")]
country <- iranfarzam
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
iranfarzam$microsite_rep <- paste(code, code2, sep = "-")


#Look for strange entries in "Species within quadrat
sort(unique(iranfarzam$Species.within.quadrat))
#There is a species called "1"but it has a cover value so just leave it
#replace "" with NA
iranfarzam[which(iranfarzam$Species.within.quadrat == ""), which(colnames(iranfarzam) == "Species.within.quadrat")] <- NA

#There is one entry of "Lichen" , but it is not the only entry in the microsite, so change it to NA
iranfarzam[which(iranfarzam$Species.within.quadrat == "Lichen"), which(colnames(iranfarzam) == "Species.within.quadrat")] <- NA

#!!! "Astragalus berividence" , "Astragalus brevedensis" - neither of these sp exist, they are changed to A brevidens
#!!! "dead Pistacia vera" - row is removed because it isnt the only entry in the microsite
iranfarzam <- iranfarzam[-which(iranfarzam$Species.within.quadrat == "dead Pistacia vera"), ]

#There are many species with multiple spellings, fix them
#Acanthophyllum sp.
iranfarzam[which(iranfarzam$Species.within.quadrat == "Acanthophyllum") , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Acanthophyllum sp."
iranfarzam[which(iranfarzam$Species.within.quadrat == "Acanthophylum sp,") , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Acanthophyllum sp."
iranfarzam[which(iranfarzam$Species.within.quadrat == "Acantiphyllum sp.") , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Acanthophyllum sp."
#"Achillea biebersteinii"
iranfarzam[iranfarzam$Species.within.quadrat %like% "bieberstein" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Achillea biebersteinii"
#"Acinos graveolens"
iranfarzam[iranfarzam$Species.within.quadrat %like% "graveolens" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Acinos graveolens"
#"Agropyron sp."
iranfarzam[iranfarzam$Species.within.quadrat %like% "Agropyrn" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <-"Agropyron sp."
#"Allium xiphopetalum"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Allium xiphopetalum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <-"Allium xiphopetalum"
#"Alyssopsis mollis"
iranfarzam[iranfarzam$Species.within.quadrat %like% "mollis" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Alyssopsis mollis"
#"Amygdalus spinosissima"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Amygdalus" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Amygdalus spinosissima"
#"Anthemis triumfetti"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Anthemis triumfetti" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Anthemis triumfetti"
#"Aractium  lappal"
iranfarzam[iranfarzam$Species.within.quadrat %like% "lappal" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Aractium  lappal"
#"Artemisia kopetdaghensis"
iranfarzam[iranfarzam$Species.within.quadrat %like% "daghensis" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Artemisia kopetdaghensis"
#"Astragalus brevidens"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Astragalus b" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Astragalus brevidens"
#"Astragalus jolderensis"
iranfarzam[iranfarzam$Species.within.quadrat %like% "jolder" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Astragalus jolderensis"
#"Astragalus verus"
iranfarzam[iranfarzam$Species.within.quadrat %like% "versus" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Astragalus verus"
#"Berberis integerrima"
iranfarzam[iranfarzam$Species.within.quadrat %like% "integerrima" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Berberis integerrima"
#"Bothriochloa ischaemum"
iranfarzam[iranfarzam$Species.within.quadrat %like% "ischaemum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Bothriochloa ischaemum"
#"Bupleurum falcatum"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Bupleurum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Bupleurum falcatum"
#"Carex halleriana"
iranfarzam[iranfarzam$Species.within.quadrat %like% "hall" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Carex halleriana"
#"Centaurea sintenisiana"
iranfarzam[iranfarzam$Species.within.quadrat %like% "sintenisiana" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Centaurea sintenisiana"
#"Centaurea virgata"
iranfarzam[iranfarzam$Species.within.quadrat %like% "virgata" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Centaurea virgata"
#"Cirsium sp."
iranfarzam[which(iranfarzam$Species.within.quadrat == "Circium sp.") , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Cirsium sp."
#"Cirsium bornmuelleri"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Cirsium bornmu" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Cirsium bornmuelleri"
#"Cleistogenes seratina"
iranfarzam[iranfarzam$Species.within.quadrat %like% "seratina" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Cleistogenes seratina"
#Colutea buhsei
iranfarzam[iranfarzam$Species.within.quadrat %like% "buhsei" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Colutea buhsei"
#Convolvulus arvensis
iranfarzam[iranfarzam$Species.within.quadrat %like% "lus arvensis" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Convolvulus arvensis"
#"Convolvulus lineatus"
iranfarzam[iranfarzam$Species.within.quadrat %like% "lineatus" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Convolvulus lineatus"
#"Cotoneaster nummularius"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Cotone" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Cotoneaster nummularius"
#"Cousinia sp."
iranfarzam[iranfarzam$Species.within.quadrat %like% "Cousinia sp." , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Cousinia sp."
#"Crucianella gilanica"
iranfarzam[iranfarzam$Species.within.quadrat %like% "gilanica" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Crucianella gilanica"
#"Dactylis glomerata"
iranfarzam[iranfarzam$Species.within.quadrat %like% "glomerata" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Dactylis glomerata"
#"Dianthus polylepis"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Dianthus" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Dianthus polylepis"
#"Echinops koelzii"
iranfarzam[iranfarzam$Species.within.quadrat %like% "koelzii" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Echinops koelzii"
#"Elymus repens""
iranfarzam[iranfarzam$Species.within.quadrat %like% "Elymus repens" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Elymus repens"
#"Ephedra foliata"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Ephedra foliata" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Ephedra foliata"
#"Eremopyrum"
iranfarzam[which(iranfarzam$Species.within.quadrat == "Eromopyrum") , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Eremopyrum"
#"Eryngium caucasicum"
iranfarzam[iranfarzam$Species.within.quadrat %like% "caucasicum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Eryngium caucasicum"
#"Euphorbia falcata"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Euphorbia falcata" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Euphorbia falcata"
#"Galium verum"
iranfarzam[iranfarzam$Species.within.quadrat %like% "verum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Galium verum"
#"Haplophyllum acutifolium" 
iranfarzam[iranfarzam$Species.within.quadrat %like% "acutifolium" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Haplophyllum acutifolium" 
#"Hordeum bulbosum"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Hordeum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Hordeum bulbosum"
#"Hypericum scabrum"
iranfarzam[iranfarzam$Species.within.quadrat %like% "scabrum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Hypericum scabrum"
#"Iris songarica"
iranfarzam[iranfarzam$Species.within.quadrat %like% "songarica" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Iris songarica"
#"Juniperus communis"
iranfarzam[iranfarzam$Species.within.quadrat %like% "communis" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Juniperus communis"
#"Kochia prostrata"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Kochia" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Kochia prostrata"
#"Koelpinia linearis"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Koelpinia" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Koelpinia linearis"
#"Lactuca orientalis"
iranfarzam[iranfarzam$Species.within.quadrat %like% "orientalis" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Lactuca orientalis"
#"Lactuca serriola"
iranfarzam[iranfarzam$Species.within.quadrat %like% "serriola" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Lactuca serriola"
iranfarzam[which(iranfarzam$Species.within.quadrat == "Latuca scariola") , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Lactuca serriola"
iranfarzam[which(iranfarzam$Species.within.quadrat == "Luctuca scariola") , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Lactuca serriola"
#"Lappula barbata"
iranfarzam[iranfarzam$Species.within.quadrat %like% "barbata" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Lappula barbata"
#"Lonicera nummulariifolia"
iranfarzam[iranfarzam$Species.within.quadrat %like% "onicera" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Lonicera nummulariifolia"
#"Noaea mucronata"
iranfarzam[iranfarzam$Species.within.quadrat %like% "cronata" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Noaea mucronata"
#"Phlomis cancellata"
iranfarzam[iranfarzam$Species.within.quadrat %like% "cancellata" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Phlomis cancellata"
#"Poa bulbosa"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Poa bulb" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Poa bulbosa"
#"Polygonum aviculare"
iranfarzam[iranfarzam$Species.within.quadrat %like% "avicular" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Polygonum aviculare"
#"Quercus castaneifolia"
iranfarzam[iranfarzam$Species.within.quadrat %like% "castaneifolia" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Quercus castaneifolia"
#"Rosa canina"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Rosa can" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Rosa canina"
#"Rosa persica"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Rosa persica" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Rosa persica"
#"Serratula latifolia"
iranfarzam[iranfarzam$Species.within.quadrat %like% "atula latifolia" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Serratula latifolia"
#'Stachys lavandulifolia'
iranfarzam[iranfarzam$Species.within.quadrat %like% "lavandulifolia" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Stachys lavandulifolia"
#"Stachys trinervis"
iranfarzam[iranfarzam$Species.within.quadrat %like% "trine" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Stachys trinervis"
#"Stachys turcomanica"
iranfarzam[iranfarzam$Species.within.quadrat %like% "turcomanica" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Stachys turcomanica"
#"Stipa arabica"
iranfarzam[iranfarzam$Species.within.quadrat %like% "arabica" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Stipa arabica"
#"Stipa turkestanica"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Stipa turkestan" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Stipa turkestanica"
#"Teucrium polium"
iranfarzam[iranfarzam$Species.within.quadrat %like% "polium" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Teucrium polium"
#"Thymus transcaspius"
iranfarzam[iranfarzam$Species.within.quadrat %like% "transcaspius" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Thymus transcaspius"
#"Tragopogon graminifolius"
iranfarzam[iranfarzam$Species.within.quadrat %like% "graminifolius" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Tragopogon graminifolius"
#"Tragopogon vedenskyi"
iranfarzam[iranfarzam$Species.within.quadrat %like% "vedenskyi" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Tragopogon vedenskyi"
#"Tulipa undulatifolia"
iranfarzam[iranfarzam$Species.within.quadrat %like% "undulatifolia" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Tulipa undulatifolia"
#"Verbascum sp."
iranfarzam[which(iranfarzam$Species.within.quadrat == "Verbascum") , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Verbascum sp."
#"Verbascum cheiranthifolium"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Verbascum cheiranthi" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Verbascum cheiranthifolium"
#"Veronica capillipes"
iranfarzam[iranfarzam$Species.within.quadrat %like% "capillipes" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Veronica capillipes"
#"Vincetoxicum scandens"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Vincetoxicum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Vincetoxicum scandens"
#"Ziziphora persica"
iranfarzam[iranfarzam$Species.within.quadrat %like% "Ziziphora persica" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Ziziphora persica"
#"Zygophyllum atriplicoides"
iranfarzam[iranfarzam$Species.within.quadrat %like% "atriplicoides" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Zygophyllum atriplicoides"


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the iranfarzam plots
iranfarzam_namedat <- namedat[which(namedat$ID %in% c(unique(iranfarzam$ID))) , ]
#get names that must be changed
iranfarzam_namedat <- iranfarzam_namedat[which(!is.na(iranfarzam_namedat$tofix)) , ]

#Euphorbia Boissieriana
iranfarzam[iranfarzam$Species.within.quadrat %like% "Euphorbia Boissieriana" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Euphorbia boissieri"
#Marabium vulgare
iranfarzam[iranfarzam$Species.within.quadrat %like% "Marabium vulgare" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Marrubium vulgare"
#Polygonatum sewerzowii
iranfarzam[iranfarzam$Species.within.quadrat %like% "Polygonatum sewerzowii" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Polygonum sewerzowii"
#Tragopogon vedenskyi
iranfarzam[iranfarzam$Species.within.quadrat %like% "Tragopogon vedenskyi" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Tragopogon vvedenskyi"
#Thesium kotschanum
iranfarzam[iranfarzam$Species.within.quadrat %like% "Thesium kotschanum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Thesium kotschyanum"
#Acantholimon erinaceoum
iranfarzam[iranfarzam$Species.within.quadrat %like% "Acantholimon erinaceoum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Acantholimon erinaceum"
#Dracocephalum kotschy
iranfarzam[iranfarzam$Species.within.quadrat %like% "Dracocephalum kotschy" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Dracocephalum kotschyi"
#Cleistogenes seratina
iranfarzam[iranfarzam$Species.within.quadrat %like% "Cleistogenes seratina" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Cleistogenes serotina"
#Allium fibrosum
iranfarzam[iranfarzam$Species.within.quadrat %like% "Allium fibrosum" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Allium fibrosum"
#Leontodon asperrimus
iranfarzam[iranfarzam$Species.within.quadrat %like% "Leontodon asperrimus" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Leontodon asperrimus"
#Paliurus spina-christi
iranfarzam[iranfarzam$Species.within.quadrat %like% "Paliurus spina-christi" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Paliurus spina-christi"
#Thymus transcaspius
iranfarzam[iranfarzam$Species.within.quadrat %like% "Thymus transcaspius" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Thymus transcaspicus"
#Buffonia oliveriana
iranfarzam[iranfarzam$Species.within.quadrat %like% "Buffonia oliveriana" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Bufonia oliveriana"
#Rhamnus pallasii
iranfarzam[iranfarzam$Species.within.quadrat %like% "Rhamnus pallasii" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Rhamnus pallasii"
#Acer monspessulanum  seedling
iranfarzam[iranfarzam$Species.within.quadrat %like% "Acer monspessulanum  seedling" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Acer monspessulanum"
#Eremodaucus lehmannii
iranfarzam[iranfarzam$Species.within.quadrat %like% "Eremodaucus lehmannii" , which(colnames(iranfarzam) == "Species.within.quadrat") ] <- "Eremodaucus lehmannii"

##There are entries of 2 in Quadrat, but it doesnt make sense. Assume there is no problem here

####Fixing israel####
#Change the name of the Cover column
colnames(israel)[which(colnames(israel) == "Cover....")] <- "Cover"

#Replace "bare" with "Bare"in ID_MIcrosite
israel[which(israel$ID_Microsite == "bare"), which(colnames(israel) == "ID_Microsite")] <- "Bare"

#In site 47, plot 1 the nurse microsites are also coded with 1
#reassign the microsites for the whole dataset to make sure they are correct
records <- c(1:nrow(israel))
for (k in records) {
  if (israel[k, which(colnames(israel) == "ID_Microsite")] == "Bare") {
    israel[k, which(colnames(israel) == "Microsite")] <- 1
  } else {
    israel[k, which(colnames(israel) == "Microsite")] <- 2
  }
}

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- israel
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
israel$microsite_rep <- paste(code, code2, sep = "-")

#In site 46, plot 3 there are two nurses in one replicate. Assign the first nurse to the previous replicate
israel[which(israel$microsite_rep == "46-3-Sarcopoterium spinosum-26"), which(colnames(israel) == "Number.of.replicate")] <- 25
#Update the microsite_rep column
country <- israel
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
israel$microsite_rep <- paste(code, code2, sep = "-")

#Look for strange entries in "Species within quadrat
sort(unique(israel$Species.within.quadrat))

#replace "" with NA
israel[which(israel$Species.within.quadrat == ""), which(colnames(israel) == "Species.within.quadrat")] <- NA

#There are species with multiple spellings, fix them
#"Echium judaeum"
israel[israel$Species.within.quadrat %like% "Echium" , which(colnames(israel) == "Species.within.quadrat") ] <- "Echium judaeum"
#"Rhamnus lycioides"
israel[israel$Species.within.quadrat %like% "Rhamnus" , which(colnames(israel) == "Species.within.quadrat") ] <- "Rhamnus lycioides"


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the israel plots
israel_namedat <- namedat[which(namedat$ID %in% c(unique(israel$ID))) , ]
#only NA in tofix column, no names to change


####Fixing Namibia Blaum####
#Change the name of the Cover column
colnames(namibiablaum)[which(colnames(namibiablaum) == "Cover....")] <- "Cover"

#Replace "bare" with "Bare"in ID_MIcrosite
namibiablaum[which(namibiablaum$ID_Microsite == "bare"), which(colnames(namibiablaum) == "ID_Microsite")] <- "Bare"

#reassign the microsites for the whole dataset to make sure they are correct
records <- c(1:nrow(namibiablaum))
for (k in records) {
  if (namibiablaum[k, which(colnames(namibiablaum) == "ID_Microsite")] == "Bare") {
    namibiablaum[k, which(colnames(namibiablaum) == "Microsite")] <- 1
  } else {
    namibiablaum[k, which(colnames(namibiablaum) == "Microsite")] <- 2
  }
}

#Remove the approx and comments columns
namibiablaum <- namibiablaum[, -c(18,19)]

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- namibiablaum
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
namibiablaum$microsite_rep <- paste(code, code2, sep = "-")

#Look for strange entries in "Species within quadrat
sort(unique(namibiablaum$Species.within.quadrat))

#Replace "NULL" with NA
namibiablaum[which(namibiablaum$Species.within.quadrat == "NULL"), which(colnames(namibiablaum) == "Species.within.quadrat")] <- NA

#If Species.within.quadrat == NA, then Cover must also be NA, not zero
records <- c(1:nrow(namibiablaum))
for (k in records) {
  if (is.na(namibiablaum[k, which(colnames(namibiablaum) == "Species.within.quadrat")]) == TRUE) {
    namibiablaum[k, which(colnames(namibiablaum) == "Cover")] <- NA
  } 
}


#There are species with multiple spellings, fix them
#"Cyperus margaritaceus"
namibiablaum[namibiablaum$Species.within.quadrat %like% "Cyperus marga" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Cyperus margaritaceus"
#"Geophyte indet."
namibiablaum[namibiablaum$Species.within.quadrat %like% "eophyt" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Geophyte indet."
#"Pupalia lappacea"
namibiablaum[namibiablaum$Species.within.quadrat %like% "Pupalia" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Pupalia lappacea"
#"Senna italica"
namibiablaum[namibiablaum$Species.within.quadrat %like% "Senna italica" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Senna italica"

##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the namibiablaum plots
namibiablaum_namedat <- namedat[which(namedat$ID %in% c(unique(namibiablaum$ID))) , ]
#get names that must be changed
namibiablaum_namedat <- namibiablaum_namedat[which(!is.na(namibiablaum_namedat$tofix)) , ]

#Searsia ternuinervis
namibiablaum[namibiablaum$Species.within.quadrat %like% "Searsia ternuinervis" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Rhus tenuinervis"
#Melinis repens subsp. Repens
namibiablaum[namibiablaum$Species.within.quadrat %like% "Melinis repens subsp. Repens" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Melinis repens"
#Geophyte indet.
namibiablaum[namibiablaum$Species.within.quadrat %like% "Geophyte indet." , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Geophyt indet."
#Asparagus indet.
namibiablaum[namibiablaum$Species.within.quadrat %like% "Asparagus indet." , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Asparagus sp."
#Rhynchosia totta var. fenchelii
namibiablaum[namibiablaum$Species.within.quadrat %like% "Rhynchosia totta var. fenchelii" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Rhynchosia totta"
#commelina africana
namibiablaum[namibiablaum$Species.within.quadrat %like% "commelina africana" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Commelina africana"
#combretum collinum
namibiablaum[namibiablaum$Species.within.quadrat %like% "combretum collinum" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Combretum collinum"
#combretum zeyheri
namibiablaum[namibiablaum$Species.within.quadrat %like% "combretum zeyheri" , which(colnames(namibiablaum) == "Species.within.quadrat") ] <- "Combretum zeyheri"

###Did they use multiple quadrats to make up the area of a nurse?
plotlist <- c(unique(namibiablaum[which(!is.na(namibiablaum$Quadrat)) , ]$ID))
###In plot 196 and 197 they sometimes used multiple quadrats to make up the area of a nurse
##We need to get the percent cover of each species over the whole microsite area
##Make a table with average cover of each species in each microsite that used multiple quadrats

for (i in 1:length(plotlist)) {
  #isolate one plot
  plot <- namibiablaum[which(namibiablaum$ID == plotlist[i]) , ]
  
  replist <- c(unique(plot$Number.of.replicate))
  
  for (r in 1:length(replist)) {
    #Isolate the bare microsite of each replicate
    sub <- plot[which(plot$Number.of.replicate == replist[r] & plot$Microsite == 1) , ]
    
    #now we need to get the average % cover of each species
    #This is the sum of species % cover, divided by the total no of quadrats sampled to make up the bare microsite
    
    if (i + r == 2) { #only make a new dataframe for the first replicate in the first plot
      
      sp_cover <- as.data.frame(tapply(sub$Cover, sub$Species.within.quadrat, FUN = sum))
      colnames(sp_cover) <- "sum_percent_cover"
      sp_cover$spname <- rownames(sp_cover)
      rownames(sp_cover) <- c(1:nrow(sp_cover))
      #get the total no of quadrats sampled to make up the microsite
      sp_cover$nquadrats <-  max(sub$Quadrat)
      #now divide the sum by the number of quadrats
      sp_cover$avg_percent_cover <- sp_cover$sum_percent_cover / sp_cover$nquadrats
      #Add the ID variable and the replicate number
      sp_cover$ID <- plotlist[i]
      sp_cover$Number.of.replicate <- replist[r]
      sp_cover$Microsite <- sub$Microsite[1]
      
    } else { #for subsequent runs we rbind to the table we made the first time
      
      if(length(which(is.na(sub$Species.within.quadrat) == TRUE)) == nrow(sub)) { #if species within quadrat is empty, tapply won't work, so just make a table with an NA cover value
        
        temp <- data.frame(sum_percent_cover = NA, spname = NA, nquadrats = max(sub$Quadrat), 
                           avg_percent_cover = NA, ID = plotlist[i], Number.of.replicate = replist[r], 
                           Microsite = sub$Microsite[1])
        
      } else { #only do tapply if there are entries in species within quadrat
        
        temp <- as.data.frame(tapply(sub$Cover, sub$Species.within.quadrat, FUN = sum))
        colnames(temp) <- "sum_percent_cover"
        temp$spname <- rownames(temp)
        rownames(temp) <- c(1:nrow(temp))
        #get the total no of quadrats sampled to make up the microsite
        temp$nquadrats <-  max(sub$Quadrat)
        #now divide the sum by the number of quadrats
        temp$avg_percent_cover <- temp$sum_percent_cover / temp$nquadrats
        #Add the ID variable and the replicate number
        temp$ID <- plotlist[i]
        temp$Number.of.replicate <- replist[r]
        temp$Microsite <- sub$Microsite[1]
      }
      
      sp_cover <- rbind(sp_cover, temp)
    }
  }
} ##where there is NA in the nquadrat column, there are no entries in the quadrat column of the plot. 
###Thus, multiple quadrats were not used in these microsites

###Now we need to delete the duplicate entries in a microsite, and replace the cover values with the values from the sp_cover table
plot196_197 <- namibiablaum[which(namibiablaum$ID %in% c(196, 197)) , ]
length(which(is.na(plot196_197$Cover) == TRUE)) #4 NA values in the cover column
rest_plots <- namibiablaum[-which(namibiablaum$ID %in% c(196, 197)) , ]
  
# Filter out duplicate species names within each microsite of each replicate
filtered_196_197 <- plot196_197 %>%
  group_by(ID,Number.of.replicate, Microsite) %>%
  filter(!duplicated(Species.within.quadrat))


##Now add the cover values from sp_cover to plot196_197
corrected_plot196_197 <- merge(filtered_196_197, sp_cover, by.x = c("ID", "Number.of.replicate", "Microsite", "Species.within.quadrat"), 
              by.y = c("ID", "Number.of.replicate", "Microsite", "spname"), all.x = TRUE, all.y = FALSE, no.dups = TRUE)

##If there is a value in the avg_percent_cover column, it must replace the value in the Cover column
#make a new cover column to do this in
corrected_plot196_197$cover_corrected <- NA

for(t in 1:nrow(corrected_plot196_197)) {
  if(is.na(corrected_plot196_197[t , ]$avg_percent_cover) == TRUE) { 
    #if there is no value in avg_percent cover, keep the original Cover value
    corrected_plot196_197[t , ]$cover_corrected <- corrected_plot196_197[t , ]$Cover
  } else {
    #if there is a valu ein avg_percent_cover, assign that as the cover for the species
    corrected_plot196_197[t , ]$cover_corrected <- corrected_plot196_197[t , ]$avg_percent_cover
  }
}

#Last thing do is remove the uneccessary columns, and remove NA's that are left in microsites where there are other species entries
corrected_plot196_197 <- 
  corrected_plot196_197[ , -which(colnames(corrected_plot196_197) %in% 
                            c("Cover", "sum_percent_cover", "nquadrats", "avg_percent_cover"))]
#Rename the cover_corrected column,
colnames(corrected_plot196_197)[18] <- "Cover"

#and remove NA's that are left in microsites where there are other species entries
#(These are left from quadrats that were sample to make up nurse area, but had no species in them)
corrected_plot196_197 <- corrected_plot196_197 %>%
  group_by(ID, Number.of.replicate, Microsite) %>%
  filter(!(is.na(Species.within.quadrat) & n() > 1) | !is.na(Species.within.quadrat))

##Lastly, rbind these corrected plots to the rest of the data
namibiablaum <- rbind(rest_plots, corrected_plot196_197)
##NOW THE QUADRAT COLUMN IS NONSENSICAL, DO NOT USE IT IN ANY ANALYSES




####Fixing Namibia Wang####
colnames(namibiawang)[which(colnames(namibiawang) == "Cover....")] <- "Cover"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- namibiawang
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
namibiawang$microsite_rep <- paste(code, code2, sep = "-")

#Look for strange entries in "Species within quadrat
sort(unique(namibiawang$Species.within.quadrat))

#Replace "0" with NA
namibiawang[which(namibiawang$Species.within.quadrat == "0"), which(colnames(namibiawang) == "Species.within.quadrat")] <- NA

#The Cover and Number of individuals columns must have NA if species within quadrat has NA
records <- c(1:nrow(namibiawang))
for (k in records) {
  if (is.na(namibiawang[k, which(colnames(namibiawang) == "Species.within.quadrat")])) {
    namibiawang[k, which(colnames(namibiawang) == "Cover")] <- NA
    namibiawang[k, which(colnames(namibiawang) == "Number.of.individuals")] <- NA
  }
}

#The replicates are all numbered correctly


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the namibiawang plots
namibiawang_namedat <- namedat[which(namedat$ID %in% c(unique(namibiawang$ID))) , ]
#get names that must be changed
namibiawang_namedat <- namibiawang_namedat[which(!is.na(namibiawang_namedat$tofix)) , ]

#Acacia haematoxylin
namibiawang[namibiawang$ID_Microsite %like% "Acacia haematoxylin" , which(colnames(namibiawang) == "ID_Microsite") ] <- "Acacia haematoxylon"
#Aptosium elongatum
namibiawang[namibiawang$Species.within.quadrat %like% "Aptosium elongatum" , which(colnames(namibiawang) == "Species.within.quadrat") ] <- "Aptosimum elongatum"
#lycrum
namibiawang[namibiawang$Species.within.quadrat %like% "lycrum" , which(colnames(namibiawang) == "Species.within.quadrat") ] <- "Lycium sp."



####Fixing South Africa####
#Change the name of the Cover column
colnames(southafrica)[which(colnames(southafrica) == "Cover....")] <- "Cover"

#The speciesnames of nurses and targets have an _ , remove these
#Species within quadrat
southafrica[c("target_genus", "target_species","infra1", "infra2", "infra3", "infra4")] <- str_split_fixed(southafrica$Species.within.quadrat, "_", 6)
southafrica$Species.within.quadrat <- paste(southafrica$target_genus, southafrica$target_species, sep = " " ) #all variety or subsp names are removed
southafrica <- southafrica[, -c(19:24)] #remove the genus, species columns

#ID_Microsite
southafrica[c("nurse_genus", "nurse_species", "infra1", "infra2")] <- str_split_fixed(southafrica$ID_Microsite, "_", 4)
southafrica$ID_Microsite <- paste(southafrica$nurse_genus, southafrica$nurse_species, sep = " " )#all variety or subsp names are removed
southafrica <- southafrica[, -c(19:22)] #remove the genus, species columns

#Replace "Bare " with "Bare" in ID_MIcrosite
southafrica[which(southafrica$ID_Microsite == "Bare "), which(colnames(southafrica) == "ID_Microsite")] <- "Bare"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- southafrica
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
southafrica$microsite_rep <- paste(code, code2, sep = "-")

#Look for strange entries in Species within quadrat
sort(unique(southafrica$Species.within.quadrat))

#Replace " " with NA
southafrica[which(southafrica$Species.within.quadrat == " "), which(colnames(southafrica) == "Species.within.quadrat")] <- NA


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the SA plots
southafrica_namedat <- namedat[which(namedat$ID %in% c(unique(southafrica$ID))) , ]
#get names that must be changed
southafrica_namedat <- southafrica_namedat[which(!is.na(southafrica_namedat$tofix)) , ]

#Stapeliopsis sp
southafrica[southafrica$Species.within.quadrat %like% "Stapeliopsis sp" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Stapeliopsis sp."
#Monsonia camdeboense
southafrica[southafrica$Species.within.quadrat %like% "Monsonia camdeboense" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Monsonia camdeboensis"
#Hereroa latepetala
southafrica[southafrica$Species.within.quadrat %like% "Hereroa latepetala" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Hereroa latipetala"
#Othonna sp
southafrica[southafrica$Species.within.quadrat %like% "Othonna sp" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Othonna sp."
#Monsonia crassicaule
southafrica[southafrica$Species.within.quadrat %like% "Monsonia crassicaule" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Monsonia crassicaulis"
southafrica[southafrica$ID_Microsite %like% "Monsonia crassicaule" , which(colnames(southafrica) == "ID_Microsite") ] <- "Monsonia crassicaulis"
#Trichodiadema sp1
southafrica[southafrica$Species.within.quadrat %like% "Trichodiadema sp1" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Trichodiadema sp."
southafrica[southafrica$ID_Microsite %like% "Trichodiadema sp1" , which(colnames(southafrica) == "ID_Microsite") ] <- "Trichodiadema sp."
#Trichodiadema cf setuliferum
southafrica[southafrica$Species.within.quadrat %like% "Trichodiadema cf setuliferum" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Trichodiadema setuliferum"
#cf. Phymaspermum parvifolium
southafrica[southafrica$Species.within.quadrat %like% "cf. Phymaspermum parvifolium" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Phymaspermum parvifolium"
#Indigofera sessifolia
southafrica[southafrica$Species.within.quadrat %like% "Indigofera sessifolia" , which(colnames(southafrica) == "Species.within.quadrat") ] <- "Indigofera sessilifolia"


####Fixing Spainmaestre####
#Change the name of the Cover column
colnames(spainmaestre)[which(colnames(spainmaestre) == "Cover....")] <- "Cover"

#Replace "bare" with "Bare" in ID_MIcrosite
spainmaestre[which(spainmaestre$ID_Microsite == "bare"), which(colnames(spainmaestre) == "ID_Microsite")] <- "Bare"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- spainmaestre
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
spainmaestre$microsite_rep <- paste(code, code2, sep = "-")

#In site 84 plot 1 there are two nurses with 25 and 18 reps
#Change the replicate numbers for the second nurse, Retama to range from 26-n
#Get the range to fix
lower <- which(spainmaestre$microsite_rep == "84-1-Retama-1")
upper <- which(spainmaestre$microsite_rep == "84-1-Bare-18")
range <- c(lower[1]:upper[length(upper)])
spainmaestre$new_rep <- spainmaestre$Number.of.replicate
spainmaestre$new_rep[range] <- spainmaestre$Number.of.replicate[range] + 25

#In site 84 plot 2 there are two nurses with 15 and 13 reps
#Change the replicate numbers for the second nurse, Genista scorpia to range from 16-n
#Get the range to fix
lower <- which(spainmaestre$microsite_rep == "84-2-Genista scorpia-1")
upper <- which(spainmaestre$microsite_rep == "84-2-Bare-13")
range <- c(lower[1]:upper[length(upper)])
spainmaestre$new_rep[range] <- spainmaestre$Number.of.replicate[range] + 15

#In site 84 plot 3 there are 3 nurses with 15, 14  and 15 reps
#Change the replicate numbers for the second nurse, Gypsophila to range from 16-30 
#and the third nurse, Genista scorpia to range from 31- 45
#Get the range to fix
lower <- which(spainmaestre$microsite_rep == "84-3-Gypsophila-1")
upper <- which(spainmaestre$microsite_rep == "84-3-Bare-14")
range <- c(lower[1]:upper[11])
spainmaestre$new_rep[range] <- spainmaestre$Number.of.replicate[range] + 15

lower <- which(spainmaestre$microsite_rep == "84-3-Genista scorpia-1")
upper <- which(spainmaestre$microsite_rep == "84-3-Bare-15")
range <- c(lower[1]:upper[length(upper)])
spainmaestre$new_rep[range] <- spainmaestre$Number.of.replicate[range] + 15 +14 

#Update the "number of replicate" and "microsite_rep"columns
spainmaestre$Number.of.replicate <- spainmaestre$new_rep
spainmaestre <- spainmaestre[, - which(colnames(spainmaestre) == "new_rep")]
code <- paste(spainmaestre$SITE_ID, spainmaestre$PLOT, sep = "-")
code2 <- paste(spainmaestre$ID_Microsite, spainmaestre$Number.of.replicate, sep = "-")
spainmaestre$microsite_rep <- paste(code, code2, sep = "-")

#Look for strange entries in Species within quadrat
sort(unique(spainmaestre$Species.within.quadrat))

#Fix species names with multiple spellings
#"Erodium"
spainmaestre[spainmaestre$Species.within.quadrat %like% "Erod" , which(colnames(spainmaestre) == "Species.within.quadrat") ] <- "Erodium"


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the spainmaestre plots
spainmaestre_namedat <- namedat[which(namedat$ID %in% c(unique(spainmaestre$ID))) , ]
#get names that must be changed
spainmaestre_namedat <- spainmaestre_namedat[which(!is.na(spainmaestre_namedat$tofix)) , ]

#Helianthemum \\small\\""
spainmaestre[spainmaestre$Species.within.quadrat %like% 'Helianthemum \"small\"' , which(colnames(spainmaestre) == "Species.within.quadrat") ] <- "Helianthemum small"
#Genista scorpia
spainmaestre[spainmaestre$Species.within.quadrat %like% "Genista scorpia" , which(colnames(spainmaestre) == "Species.within.quadrat") ] <- "Genista scorpius"
spainmaestre[spainmaestre$ID_Microsite %like% "Genista scorpia" , which(colnames(spainmaestre) == "ID_Microsite") ] <- "Genista scorpius"


####Fixing spainrey####
#Change the name of the Cover column
colnames(spainrey)[which(colnames(spainrey) == "Cover....")] <- "Cover"
spainrey$Cover <- as.numeric(spainrey$Cover) #make it numeric
#Change the name of the Nd individuals column
colnames(spainrey)[which(colnames(spainrey) == "Nd.individuals")] <- "Number.of.individuals"
#remove the extra empty column
spainrey <- spainrey[, -20]

#Replace "Bare soil" with "Bare"in ID_MIcrosite
spainrey[which(spainrey$ID_Microsite == "Bare soil"), which(colnames(spainrey) == "ID_Microsite")] <- "Bare"

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
country <- spainrey
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
spainrey$microsite_rep <- paste(code, code2, sep = "-")

#spainrey has an extra column "site" that has the names of plots. Delete this column 
spainrey <- spainrey[ , - which(colnames(spainrey) == "Site")]
#rename the height column
colnames(spainrey)[which(colnames(spainrey) == "height..cm.")] <- "height.cm."

##Site 89 plot 1 has 2 nurses, each with replicate numbers 1-15. 
#However these nurses are mixed so we must subset the data for each nurse and assign replicate numbers 16-30 to the second nurse, Stipa
spainrey$new_rep <- spainrey$Number.of.replicate
site89_plot1 <- spainrey[which(spainrey$microsite_rep %like% "89-1"),] #only plot 1 in site 89

#now subset all the parts where the nurse is stipa
a <- which(site89_plot1$microsite_rep == "89-1-Stipa-1")[1]
b <- which(site89_plot1$microsite_rep == "89-1-Bare-1")[length(which(site89_plot1$microsite_rep == "89-1-Bare-1"))] 

c <- which(site89_plot1$microsite_rep == "89-1-Stipa-2")[1]
d <- which(site89_plot1$microsite_rep == "89-1-Bare-2")[length(which(site89_plot1$microsite_rep == "89-1-Bare-2"))] 

cc <- which(site89_plot1$microsite_rep == "89-1-Stipa-3")[1]
dd <- which(site89_plot1$microsite_rep == "89-1-Bare-3")[length(which(site89_plot1$microsite_rep == "89-1-Bare-3"))] 

e <- which(site89_plot1$microsite_rep == "89-1-Stipa-4")[1]
f <- which(site89_plot1$microsite_rep == "89-1-Bare-4")[length(which(site89_plot1$microsite_rep == "89-1-Bare-4"))] 

g <- which(site89_plot1$microsite_rep == "89-1-Stipa-5")[1]
h <- which(site89_plot1$microsite_rep == "89-1-Bare-5")[length(which(site89_plot1$microsite_rep == "89-1-Bare-5"))] 

i <- which(site89_plot1$microsite_rep == "89-1-Stipa-6")[1]
j <- which(site89_plot1$microsite_rep == "89-1-Bare-6")[length(which(site89_plot1$microsite_rep == "89-1-Bare-6"))]

k <- which(site89_plot1$microsite_rep == "89-1-Stipa-7")[1]
l <- which(site89_plot1$microsite_rep == "89-1-Bare-7")[length(which(site89_plot1$microsite_rep == "89-1-Bare-7"))] 

m <- which(site89_plot1$microsite_rep == "89-1-Stipa-8")[1]
n <- which(site89_plot1$microsite_rep == "89-1-Bare-8")[length(which(site89_plot1$microsite_rep == "89-1-Bare-8"))] 

o <- which(site89_plot1$microsite_rep == "89-1-Stipa-9")[1]
p <- which(site89_plot1$microsite_rep == "89-1-Bare-9")[length(which(site89_plot1$microsite_rep == "89-1-Bare-9"))] 

q <- which(site89_plot1$microsite_rep == "89-1-Stipa-10")[1]
r <- which(site89_plot1$microsite_rep == "89-1-Bare-10")[length(which(site89_plot1$microsite_rep == "89-1-Bare-10"))] 

s <- which(site89_plot1$microsite_rep == "89-1-Stipa-11")[1]
t <- which(site89_plot1$microsite_rep == "89-1-Bare-11")[length(which(site89_plot1$microsite_rep == "89-1-Bare-11"))] 

u <- which(site89_plot1$microsite_rep == "89-1-Stipa-12")[1]
v <- which(site89_plot1$microsite_rep == "89-1-Bare-12")[length(which(site89_plot1$microsite_rep == "89-1-Bare-12"))] 

w <- which(site89_plot1$microsite_rep == "89-1-Stipa-13")[1]
x <- which(site89_plot1$microsite_rep == "89-1-Bare-13")[length(which(site89_plot1$microsite_rep == "89-1-Bare-13"))] 

y <- which(site89_plot1$microsite_rep == "89-1-Stipa-14")[1]
z <- which(site89_plot1$microsite_rep == "89-1-Bare-14")[length(which(site89_plot1$microsite_rep == "89-1-Bare-14"))] 

aa <- which(site89_plot1$microsite_rep == "89-1-Stipa-15")[1]
bb <- which(site89_plot1$microsite_rep == "89-1-Bare-15")[length(which(site89_plot1$microsite_rep == "89-1-Bare-15"))] 

#dataframe with only stipa nurse
stipa_89_1 <- site89_plot1[c(a:b, c:d, cc:dd, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, aa:bb) , ] #with nurse stipa
#change the replicate numbers to follow on from palmito
stipa_89_1$new_rep <- stipa_89_1$Number.of.replicate +15
#dataframe with only palmito nurse
palmito_89_1 <- site89_plot1[-c(a:b, c:d, cc:dd, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, aa:bb) , ] #with nurse palmito
#repaired site 89 plot1 
repaired_site89_plot1 <- rbind(palmito_89_1 , stipa_89_1)

###
##Site 89 plot 2 has 2 nurses, Launea and Stipa, each with replicate numbers 1-15. 
#However these nurses are mixed so we must subset the data for each nurse and assign replicate numbers 16-30 to the second nurse, Stipa
site89_plot2 <- spainrey[which(spainrey$microsite_rep %like% "89-2"),] #only plot 2 in site 89

#now subset all the parts where the nurse is stipa
a <- which(site89_plot2$microsite_rep == "89-2-Stipa-1")[1]
b <- which(site89_plot2$microsite_rep == "89-2-Bare-1")[length(which(site89_plot2$microsite_rep == "89-2-Bare-1"))] 

c <- which(site89_plot2$microsite_rep == "89-2-Stipa-2")[1]
d <- which(site89_plot2$microsite_rep == "89-2-Bare-2")[length(which(site89_plot2$microsite_rep == "89-2-Bare-2"))] 

cc <- which(site89_plot2$microsite_rep == "89-2-Stipa-3")[1]
dd <- which(site89_plot2$microsite_rep == "89-2-Bare-3")[length(which(site89_plot2$microsite_rep == "89-2-Bare-3"))] 

e <- which(site89_plot2$microsite_rep == "89-2-Stipa-4")[1]
f <- which(site89_plot2$microsite_rep == "89-2-Bare-4")[length(which(site89_plot2$microsite_rep == "89-2-Bare-4"))] 

g <- which(site89_plot2$microsite_rep == "89-2-Stipa-5")[1]
h <- which(site89_plot2$microsite_rep == "89-2-Bare-5")[length(which(site89_plot2$microsite_rep == "89-2-Bare-5"))] 

i <- which(site89_plot2$microsite_rep == "89-2-Stipa-6")[1]
j <- which(site89_plot2$microsite_rep == "89-2-Bare-6")[length(which(site89_plot2$microsite_rep == "89-2-Bare-6"))]

k <- which(site89_plot2$microsite_rep == "89-2-Stipa-7")[1]
l <- which(site89_plot2$microsite_rep == "89-2-Bare-7")[length(which(site89_plot2$microsite_rep == "89-2-Bare-7"))] 

m <- which(site89_plot2$microsite_rep == "89-2-Stipa-8")[1]
n <- which(site89_plot2$microsite_rep == "89-2-Bare-8")[length(which(site89_plot2$microsite_rep == "89-2-Bare-8"))] 

o <- which(site89_plot2$microsite_rep == "89-2-Stipa-9")[1]
p <- which(site89_plot2$microsite_rep == "89-2-Bare-9")[length(which(site89_plot2$microsite_rep == "89-2-Bare-9"))] 

q <- which(site89_plot2$microsite_rep == "89-2-Stipa-10")[1]
r <- which(site89_plot2$microsite_rep == "89-2-Bare-10")[length(which(site89_plot2$microsite_rep == "89-2-Bare-10"))] 

s <- which(site89_plot2$microsite_rep == "89-2-Stipa-11")[1]
t <- which(site89_plot2$microsite_rep == "89-2-Bare-11")[length(which(site89_plot2$microsite_rep == "89-2-Bare-11"))] 

u <- which(site89_plot2$microsite_rep == "89-2-Stipa-12")[1]
v <- which(site89_plot2$microsite_rep == "89-2-Bare-12")[length(which(site89_plot2$microsite_rep == "89-2-Bare-12"))] 

w <- which(site89_plot2$microsite_rep == "89-2-Stipa-13")[1]
x <- which(site89_plot2$microsite_rep == "89-2-Bare-13")[length(which(site89_plot2$microsite_rep == "89-2-Bare-13"))] 

y <- which(site89_plot2$microsite_rep == "89-2-Stipa-14")[1]
z <- which(site89_plot2$microsite_rep == "89-2-Bare-14")[length(which(site89_plot2$microsite_rep == "89-2-Bare-14"))] 

aa <- which(site89_plot2$microsite_rep == "89-2-Stipa-15")[1]
bb <- which(site89_plot2$microsite_rep == "89-2-Bare-15")[length(which(site89_plot2$microsite_rep == "89-2-Bare-15"))] 

#dataframe with only stipa nurse
stipa_89_2 <- site89_plot2[c(a:b, c:d, cc:dd, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, aa:bb) , ] #with nurse stipa
#change the replicate numbers to follow on from Laurea
stipa_89_2$new_rep <- stipa_89_2$Number.of.replicate +15
#dataframe with only Laure nurse
Launea_89_2 <- site89_plot2[-c(a:b, c:d, cc:dd, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, aa:bb) , ] #with nurse stipa
#repaired site 89 plot1 
repaired_site89_plot2 <- rbind(Launea_89_2 , stipa_89_2)

##site 89 plot 3 only has one nurse, stipa
repaired_site89_plot3 <- spainrey[which(spainrey$microsite_rep %like% "89-3"),] #only plot 3 in site 89

##site 89 plot 4 has 3 nurses, Macrocloa stipa, Genista ramosissima and Palmito
#assign replicate numbers that follow on from the previous nurse
site89_plot4 <- spainrey[which(spainrey$microsite_rep %like% "89-4"),] #only plot 4 in site 89

#subset where the nurse is Genista
a <- which(site89_plot4$microsite_rep == "89-4-Genista ramossisima-1")[1]
b <- which(site89_plot4$microsite_rep == "89-4-Bare-1")[5] 

c <- which(site89_plot4$microsite_rep == "89-4-Genista ramossisima-2")[1]
d <- which(site89_plot4$microsite_rep == "89-4-Bare-15")[7] 

genista_89_4 <- site89_plot4[c(a:b, c:d) , ] #with nurse genista
genista_89_4$new_rep <- genista_89_4$Number.of.replicate +17 #there are 17 reps of macrocloa stipa

#subset where nurse is Palmito
#the rest of the table is palmito
palmito_89_4 <- site89_plot4[c(which(site89_plot4$microsite_rep == "89-4-Palmito-1") : nrow(site89_plot4)) ,]
palmito_89_4$new_rep <- palmito_89_4$Number.of.replicate + 17 + 15 #there are 17 reps of macrocloa and 15 genista
#remove the rows with 89-4-Palmito-15 because there is no bare microsite in ths rep
palmito_89_4 <- palmito_89_4[-(which(palmito_89_4$microsite_rep == "89-4-Palmito-15")) , ]

#subset where nurse is Macrcocloa
inter <- site89_plot4[-c(a:b, c:d) , ] 
macrocloa_89_4 <- inter[-c(which(inter$microsite_rep == "89-4-Palmito-1") : nrow(inter)) ,]

#repaired site89 plot4
repaired_site89_plot4 <- rbind(macrocloa_89_4, genista_89_4, palmito_89_4)


##site 90 plot 1 has 3 nurses, Juniperus oxycedrus (1-16), Lavandula latifolia (1-16), Quercus ilex (1-15)
#reassign the replicate numbers so that they follow on from the previos nurse
site90_plot1 <- spainrey[which(spainrey$microsite_rep %like% "90-1") , ]

#with nurse Juniperus
juniperus_90_1 <- site90_plot1[c(1: which(site90_plot1$microsite_rep == "90-1-Bare-16")[1]) , ]

#with nurse Lavandula
lower <- which(site90_plot1$microsite_rep == "90-1-Lavandula latifolia-15")[1]
upper <- which(site90_plot1$microsite_rep == "90-1-Bare-16")[4]
lavandula_90_1 <- site90_plot1[c(lower:upper) , ]
lavandula_90_1$new_rep <- lavandula_90_1$Number.of.replicate + 16

#with nurse quercus
quercus_90_1 <- site90_plot1[-c(1: upper) , ]
quercus_90_1$new_rep <- quercus_90_1$Number.of.replicate + 16 +16 

#repaired site 90 plot 1 
repaired_site90_plot1 <- rbind(juniperus_90_1, lavandula_90_1, quercus_90_1)

####site 90 plot 2 has 3 nurses, Quercus ilex (1-15), Crataegus monogyna (1-15), Juniperus oxycedrus (1-15)
#reassign the replicate numbers so that they follow on from the previos nurse
site90_plot2 <- spainrey[which(spainrey$microsite_rep %like% "90-2") , ]

#with nurse quercus
a <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-1")[1]
b <- which(site90_plot2$microsite_rep == "90-2-Bare-1")[3] 

c <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-2")[1]
d <- which(site90_plot2$microsite_rep == "90-2-Bare-2")[2] 

e <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-3")[1]
f <- which(site90_plot2$microsite_rep == "90-2-Bare-3")[8] 

g <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-4")[1]
h <- which(site90_plot2$microsite_rep == "90-2-Bare-4")[7] 

i <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-5")[1]
j <- which(site90_plot2$microsite_rep == "90-2-Bare-5")[11] 

k <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-6")[1]
l <- which(site90_plot2$microsite_rep == "90-2-Bare-6")[6] 

m <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-7")[1]
n <- which(site90_plot2$microsite_rep == "90-2-Bare-7")[11] 

o <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-8")[1]
p <- which(site90_plot2$microsite_rep == "90-2-Bare-9")[4] 

q <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-10")[1]
r <- which(site90_plot2$microsite_rep == "90-2-Bare-11")[5] 

s <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-12")[1]
t <- which(site90_plot2$microsite_rep == "90-2-Bare-12")[5] 

u <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-13")[1]
v <- which(site90_plot2$microsite_rep == "90-2-Bare-13")[5] 

w <- which(site90_plot2$microsite_rep == "90-2-Quercus ilex-14")[1]
x <- which(site90_plot2$microsite_rep == "90-2-Bare-15")[2] 

quercus_90_2 <- site90_plot2[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x) , ] #with nurse quercus
quercus_90_2$new_rep <- quercus_90_2$Number.of.replicate + 15

#without nurse quercus
int <-site90_plot2[- c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x) , ] #without nurse quercus

#with nurse Juniperus
a <- which(int$microsite_rep == "90-2-Juniperus oxycedrus-1")[1]
b <- which(int$microsite_rep == "90-2-Bare-3")[length(which(int$microsite_rep == "90-2-Bare-3"))] 

c <- which(int$microsite_rep == "90-2-Juniperus oxycedrus-4")[1]
d <- which(int$microsite_rep == "90-2-Bare-4")[length(which(int$microsite_rep == "90-2-Bare-4"))] 

e <- which(int$microsite_rep == "90-2-Juniperus oxycedrus-5")[1]
f <- which(int$microsite_rep == "90-2-Bare-5")[length(which(int$microsite_rep == "90-2-Bare-5"))]

g <- which(int$microsite_rep == "90-2-Juniperus oxycedrus-6")[1]
h <- which(int$microsite_rep == "90-2-Bare-7")[length(which(int$microsite_rep == "90-2-Bare-7"))]

o <- which(int$microsite_rep == "90-2-Juniperus oxycedrus-8")[1]
p <- which(int$microsite_rep == "90-2-Bare-8")[length(which(int$microsite_rep == "90-2-Bare-8"))]

i <- which(int$microsite_rep == "90-2-Juniperus oxycedrus-9")[1]
j <- which(int$microsite_rep == "90-2-Bare-10")[length(which(int$microsite_rep == "90-2-Bare-10"))]

k <- which(int$microsite_rep == "90-2-Juniperus oxycedrus-11")[1]
l <- which(int$microsite_rep == "90-2-Bare-11")[length(which(int$microsite_rep == "90-2-Bare-11"))]

m <- which(int$microsite_rep == "90-2-Juniperus oxycedrus-12")[1]
n <- which(int$microsite_rep == "90-2-Bare-15")[1]

juniperus_90_2 <- int[c(a:b, c:d, e:f, g:h, o:p, i:j, k:l, m:n) , ] #with nurse juniperus
juniperus_90_2$new_rep <- juniperus_90_2$Number.of.replicate + 15 +15

#with nurse crateagus
crateagus_90_2 <- int[-c(a:b, c:d, e:f, g:h, o:p, i:j, k:l, m:n) , ] #with nurse crateagus

#repaired site 90 plot 2
repaired_site90_plot2 <- rbind(crateagus_90_2, quercus_90_2, juniperus_90_2)

##There are 2 nurses in site 90 plot 3, quercus ilex (1-20) and juniperus (1-20)
site90_plot3 <- spainrey[which(spainrey$microsite_rep %like% "90-3") , ]

lower <- which(site90_plot3$microsite_rep == "90-3-Juniperus oxycedrus-1")[1]
upper <- nrow(site90_plot3)

juniperus_90_3 <- site90_plot3[c(lower:upper) , ] #with nurse juniperus
juniperus_90_3$new_rep <- juniperus_90_3$Number.of.replicate + 20

quercus_90_3 <- site90_plot3[ - c(lower:upper) , ] #with nurse quercus

#repaired site 90 plot 3
repaired_site90_plot3 <- rbind(quercus_90_3, juniperus_90_3)


##There are 2 nurses in site 90 plot 4, quercus ilex (1-20) and juniperus (1-20)
site90_plot4 <- spainrey[which(spainrey$microsite_rep %like% "90-4") , ]

lower <- which(site90_plot4$microsite_rep == "90-4-Juniperus oxycedrus-1")[1]
upper <- nrow(site90_plot4)

juniperus_90_4 <- site90_plot4[c(lower:upper) , ] #with nurse juniperus
juniperus_90_4$new_rep <- juniperus_90_4$Number.of.replicate + 20

quercus_90_4 <- site90_plot4[ - c(lower:upper) , ] #with nurse quercus

#repaired site 90 plot 4
repaired_site90_plot4 <- rbind(quercus_90_4, juniperus_90_4)

###In site 91 plot 1 there are 3 nurses, Macrochloa tenacissima (1-15), Anthyllis cytisoides (1-15), Retama sphaerocarpa (1-15)
#reassign replicate numbers so that they follow on from the previous nurse
site91_plot1 <- spainrey[which(spainrey$microsite_rep %like% "91-1") , ]

#with nurse Macrocloa
a <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-1")[1]
b <- which(site91_plot1$microsite_rep == "91-1-Bare-1")[1] 

c <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-2")[1]
d <- which(site91_plot1$microsite_rep == "91-1-Bare-2")[1] 

e <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-3")[1]
f <- which(site91_plot1$microsite_rep == "91-1-Bare-3")[3] 

g <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-4")[1]
h <- which(site91_plot1$microsite_rep == "91-1-Bare-5")[1] 

i <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-6")[1]
j <- which(site91_plot1$microsite_rep == "91-1-Bare-6")[1] 

k <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-7")[1]
l <- which(site91_plot1$microsite_rep == "91-1-Bare-7")[3] 

m <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-8")[1]
n <- which(site91_plot1$microsite_rep == "91-1-Bare-8")[5] 

o <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-9")[1]
p <- which(site91_plot1$microsite_rep == "91-1-Bare-9")[4] 

q <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-10")[1]
r <- which(site91_plot1$microsite_rep == "91-1-Bare-10")[4]

s <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-11")[1]
t <- which(site91_plot1$microsite_rep == "91-1-Bare-11")[2] 

u <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-12")[1]
v <- which(site91_plot1$microsite_rep == "91-1-Bare-12")[1] 

w <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-13")[1]
x <- which(site91_plot1$microsite_rep == "91-1-Bare-13")[3] 

y <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-14")[1]
z <- which(site91_plot1$microsite_rep == "91-1-Bare-14")[2] 

aa <- which(site91_plot1$microsite_rep == "91-1-Macrochloa tenacissima-15")[1]
bb <- which(site91_plot1$microsite_rep == "91-1-Bare-15")[1] 

macrocloa_91_1 <- site91_plot1[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, aa:bb) , ] #with nurse macrocloa

#with the other 2 nurses
int <- site91_plot1[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x, y:z, aa:bb) , ] #with nurse macrocloa

#with nurse retama
a <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-1")[1]
b <- which(int$microsite_rep == "91-1-Bare-2")[2] 

c <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-3")[1]
d <- which(int$microsite_rep == "91-1-Bare-3")[1] 

e <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-4")[1]
f <- which(int$microsite_rep == "91-1-Bare-5")[1] 

g <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-6")[1]
h <- which(int$microsite_rep == "91-1-Bare-6")[1] 

i <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-7")[1]
j <- which(int$microsite_rep == "91-1-Bare-7")[1] 

k <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-8")[1]
l <- which(int$microsite_rep == "91-1-Bare-8")[3]

m <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-9")[1]
n <- which(int$microsite_rep == "91-1-Bare-9")[3]

o <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-10")[1]
p <- which(int$microsite_rep == "91-1-Bare-10")[2] 

q <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-11")[1]
r <- which(int$microsite_rep == "91-1-Bare-11")[4]

s <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-12")[1]
t <- which(int$microsite_rep == "91-1-Bare-12")[3] 

u <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-13")[1]
v <- which(int$microsite_rep == "91-1-Bare-14")[1] 

w <- which(int$microsite_rep == "91-1-Retama sphaerocarpa-15")[1]
x <- which(int$microsite_rep == "91-1-Bare-15")[1] 

retama_91_1 <- int[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x) , ] #with nurse retama
retama_91_1$new_rep <- retama_91_1$Number.of.replicate + 15

#with nurse anthyllis
anthyllis_91_1 <- int[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t, u:v, w:x) , ] #with nurse retama
anthyllis_91_1$new_rep <- anthyllis_91_1$Number.of.replicate + 15 + 15

#repaired site 91 plot 1
repaired_site91_plot1 <- rbind(macrocloa_91_1, retama_91_1, anthyllis_91_1)

###site 91 plot 2 has 3 nurses, Anthyllis cytisoides, Retama sphaerocarpa and Macrochloa tenacissima each with reps 1-15
#assign new rep numbers to follow on from the previous nurse
site91_plot2 <- spainrey[which(spainrey$microsite_rep %like% "91-2") , ]

#with nurse anthyllis
a <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-1")[1]
b <- which(site91_plot2$microsite_rep == "91-2-Bare-1")[1] 

c <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-2")[1]
d <- which(site91_plot2$microsite_rep == "91-2-Bare-2")[6]

e <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-3")[1]
f <- which(site91_plot2$microsite_rep == "91-2-Bare-3")[2] 

g <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-4")[1]
h <- which(site91_plot2$microsite_rep == "91-2-Bare-4")[4]

i <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-5")[1]
j <- which(site91_plot2$microsite_rep == "91-2-Bare-6")[1]

k <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-7")[1]
l <- which(site91_plot2$microsite_rep == "91-2-Bare-7")[1]

m <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-8")[1]
n <- which(site91_plot2$microsite_rep == "91-2-Bare-8")[3]

o <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-9")[1]
p <- which(site91_plot2$microsite_rep == "91-2-Bare-9")[3]

q <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-10")[1]
r <- which(site91_plot2$microsite_rep == "91-2-Bare-10")[3]

s <- which(site91_plot2$microsite_rep == "91-2-Anthyllis cytisoides-11")[1]
t <- which(site91_plot2$microsite_rep == "91-2-Bare-15")[5]

anthyllis_91_2 <- site91_plot2[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t) , ] #with nurse anthyllis

#with the other two nurses
int <-  site91_plot2[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t) , ] 

#with nurse macrocloa
a <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-1")[1]
b <- which(int$microsite_rep == "91-2-Bare-1")[3] 

c <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-2")[1]
d <- which(int$microsite_rep == "91-2-Bare-3")[1] 

e <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-4")[1]
f <- which(int$microsite_rep == "91-2-Bare-4")[1] 

g <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-5")[1]
h <- which(int$microsite_rep == "91-2-Bare-6")[1] 

i <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-7")[1]
j <- which(int$microsite_rep == "91-2-Bare-7")[1] 

k <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-8")[1]
l <- which(int$microsite_rep == "91-2-Bare-9")[1] 

m <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-10")[1]
n <- which(int$microsite_rep == "91-2-Bare-10")[2] 

o <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-11")[1]
p <- which(int$microsite_rep == "91-2-Bare-12")[2] 

q <- which(int$microsite_rep == "91-2-Macrochloa tenacissima-13")[1]
r <- which(int$microsite_rep == "91-2-Bare-15")[2] 

macrocloa_91_2 <- int[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r) , ] #with nurse macrocloa
macrocloa_91_2$new_rep <- macrocloa_91_2$Number.of.replicate + 15

#with nurse retama
retama_91_2 <- int[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r) , ] #with nurse retama
retama_91_2$new_rep <- retama_91_2$Number.of.replicate + 15 + 15

#repaired site 91 plot 2
repaired_site91_plot2 <- rbind(anthyllis_91_2, macrocloa_91_2, retama_91_2)

###Site 91 plot 3 has two nurses, Macrochloa tenacissima and Anthyllis cytisoides, each with reps 1-20
site91_plot3 <- spainrey[which(spainrey$microsite_rep %like% "91-3") , ]

#with nurse Anthyllis
a <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-1")[1]
b <- which(site91_plot3$microsite_rep == "91-3-Bare-1")[2] 

c <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-2")[1]
d <- which(site91_plot3$microsite_rep == "91-3-Bare-2")[2] 

e <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-3")[1]
f <- which(site91_plot3$microsite_rep == "91-3-Bare-8")[1] 

g <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-9")[1]
h <- which(site91_plot3$microsite_rep == "91-3-Bare-9")[2] 

i <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-10")[1]
j <- which(site91_plot3$microsite_rep == "91-3-Bare-10")[2] 

k <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-11")[1]
l <- which(site91_plot3$microsite_rep == "91-3-Bare-14")[1] 

m <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-15")[1]
n <- which(site91_plot3$microsite_rep == "91-3-Bare-16")[1] 

o <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-17")[1]
p <- which(site91_plot3$microsite_rep == "91-3-Bare-17")[1] 

q <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-18")[1]
r <- which(site91_plot3$microsite_rep == "91-3-Bare-18")[1] 

s <- which(site91_plot3$microsite_rep == "91-3-Anthyllis cytisoides-19")[1]
t <- which(site91_plot3$microsite_rep == "91-3-Bare-20")[3] 

anthyllis_91_3 <- site91_plot3[c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t) , ] #with nurse anthyllis
anthyllis_91_3$new_rep <- anthyllis_91_3$Number.of.replicate + 20

#with nurse macrocloa
macrocloa_91_3 <- site91_plot3[-c(a:b, c:d, e:f, g:h, i:j, k:l, m:n, o:p, q:r, s:t) , ] #with nurse macrocloa

#repaired site 91 plot 3
repaired_site91_plot3 <- rbind(macrocloa_91_3, anthyllis_91_3)

###Join all repaired plots together and upate the number of replicate column
spainrey <- rbind(repaired_site89_plot1, repaired_site89_plot2, repaired_site89_plot3, repaired_site89_plot4, 
      repaired_site90_plot1, repaired_site90_plot2, repaired_site90_plot3, repaired_site90_plot4, 
      repaired_site91_plot1, repaired_site91_plot2, repaired_site91_plot3)

spainrey$Number.of.replicate <- spainrey$new_rep
spainrey <- spainrey[, -which(colnames(spainrey) == "new_rep")]

country <- spainrey
code <- paste(country$SITE_ID, country$PLOT, sep = "-")
code2 <- paste(country$ID_Microsite, country$Number.of.replicate, sep = "-")
spainrey$microsite_rep <- paste(code, code2, sep = "-")


#Look for strange entries in "Species within quadrat
sort(unique(spainrey$Species.within.quadrat))

#Replace "-" with NA
spainrey[which(spainrey$Species.within.quadrat == "-"), which(colnames(spainrey) == "Species.within.quadrat")] <- NA

#Replace "" with NA
spainrey[which(spainrey$Species.within.quadrat == ""), which(colnames(spainrey) == "Species.within.quadrat")] <- NA

#Replace Nada, nada and no hay nada with NA
spainrey[spainrey$Species.within.quadrat %like% "ada" , which(colnames(spainrey) == "Species.within.quadrat") ] <- NA


###Dead plants noted with muerto, remove these rows if it isn't the only entry in a microsite
spainrey[spainrey$Species.within.quadrat %like% "muert" , which(colnames(spainrey) == "Species.within.quadrat") ] #only 21 dewad plants
#none of the muerto plants are alone in their replicates, so safe to remove these rows
spainrey <- spainrey[-which(spainrey$Species.within.quadrat %like% "muert" ) , ]

#Fix species with multiple different spellings
#"Genista ramosissima"
spainrey[spainrey$Species.within.quadrat %like% "Genista ramos" , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Genista ramosissima"
#"Lavandula"
spainrey[which(spainrey$Species.within.quadrat == "Lavandula ") , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Lavandula"
#"Lavandula multifida"
spainrey[spainrey$Species.within.quadrat %like% "multifid" , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Lavandula multifida"
#"Phagnalon sp."
spainrey[spainrey$Species.within.quadrat %like% "Phagnalon" , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Phagnalon sp."
#"Thymus"
spainrey[which(spainrey$Species.within.quadrat == "Thymus ") , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Thymus"
#"Anthyllis cytisoides "
spainrey[spainrey$ID_Microsite %like% "Anthyllis cytisoides" , which(colnames(spainrey) == "ID_Microsite") ] <- "Anthyllis cytisoides"


##Fix name spellings that do not correspond to the trait data names
#Subset namedat for the spainrey plots
spainrey_namedat <- namedat[which(namedat$ID %in% c(unique(spainrey$ID))) , ]
#get names that must be changed
spainrey_namedat <- spainrey_namedat[which(!is.na(spainrey_namedat$tofix)) , ]

#Tussock
spainrey[spainrey$Species.within.quadrat %like% "Tussock" , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Tussock sp."
#Ulex
spainrey[spainrey$Species.within.quadrat %like% "Ulex" , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Ulex sp."
#Helianthum almeriense
spainrey[spainrey$Species.within.quadrat %like% "Helianthum almeriense" , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Helianthemum almeriense"
#Hormathophylla spinosa
spainrey[spainrey$Species.within.quadrat %like% "Hormathophylla spinosa" , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Hormathophylla spinosa"
#Rhamnus myrtifolius
spainrey[spainrey$Species.within.quadrat %like% "Rhamnus myrtifolius" , which(colnames(spainrey) == "Species.within.quadrat") ] <- "Rhamnus myrtifolia"


####Add the Grazing pressure and aridity of each plot to the country datasets####
#We will joing GRAZ and ARIDITY_v3 to each country dataset by the ID variable, which is the unique ID for each plot
#GRAZ: Grazing intensity within the plot (0 ungrazed; 1:low grazing; 2:medium grazing; 3:high grazing)
#ARIDITY_v3 : Aridity value for each plot
plotvar <- siteinfo[, c(1, 7, 62)]
plotvar <- plotvar[- which(is.na(plotvar$ID)) , ]

v2_algeria <- merge(x = algeria, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_argentina <- merge(x = argentina, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_australia <- merge(x = australia, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_chile <- merge(x = chile, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_chinachong <- merge(x = chinachong, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_chinaxin <- merge(x = chinaxin, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_iranabedi <- merge(x = iranabedi, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_iranfarzam <- merge(x = iranfarzam, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_israel <- merge(x = israel, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_namibiablaum <- merge(x = namibiablaum, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_namibiawang <- merge(x = namibiawang, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_southafrica <- merge(x = southafrica, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_spainmaestre <- merge(x = spainmaestre, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)
v2_spainrey <- merge(x = spainrey, y = plotvar, by = "ID", all.x = FALSE, all.y = FALSE)

#Write all the v2 datasets to csv files
#dir.create("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2") #create a folder for the data
towrite <- c("v2_algeria", "v2_argentina", "v2_australia", "v2_chile", "v2_chinachong", "v2_chinaxin", "v2_iranabedi", "v2_iranfarzam",
             "v2_israel", "v2_namibiablaum", "v2_namibiawang", "v2_southafrica", "v2_spainmaestre", "v2_spainrey")

for(i in 1:length(towrite)) {                              
  write.csv2(get(towrite[i]),                              
             paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2\\",
                    towrite[i],
                    ".csv"),
             row.names = FALSE)
}






