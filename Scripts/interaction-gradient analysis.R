###HOW DO INTERACTION OUTCOMES CHANGE ALONG THE GRAZING-ARIDITY SPECTRUM###
wd <- "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2"
library(data.table)
library(hexbin)
library(lme4)
library(car)
library(nlme)
library(glmmTMB)
library(glmmADMB)
library(dplyr)

###read in the facilitation data for each country
data_files <- list.files(wd)
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2\\",
                          data_files[i])))
}

###summarise the data for each nurse-open pair so that we can calculate NIntc 
##Create a sum_ table with the info corresponding to each uniquerep
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey") 

column_names <- c("ID", "site_ID" , "plot" , "graz", "aridity", "microsite" , "microsite_ID" , "replicate_no", "uniquerep", "sprichness", "shannondiv_cover", "totalcover")

sum_list <- c("sum_algeria", "sum_argentina", "sum_australia", "sum_chile", "sum_chinachong", "sum_chinaxin", "sum_iranabedi", "sum_iranfarzam", 
              "sum_israel", "sum_namibiablaum", "sum_namibiawang", "sum_southafrica",  "sum_spainmaestre", "sum_spainrey")

for (i in 1:length(countrynames)) {
  cou <- get(countrynames[i]) #This is the original data for each country
  assign(paste("sum", countrynames[i], sep= "_"), #create an empty sum table that will have the values for each uniquerep
         data.frame(matrix(nrow = length(unique(cou$microsite_rep)), ncol = length(column_names))))
  sum_cou <- (get(sum_list[i])) #assign this empty table to sum_cou
  colnames(sum_cou) = column_names
  sum_cou$uniquerep <- unique(cou$microsite_rep)
  matches <- match(sum_cou$uniquerep, cou$microsite_rep)
  sum_cou$ID <- cou$ID[matches] #fill the table with the values matching every uniquerep
  sum_cou$site_ID <- cou$SITE_ID[matches]
  sum_cou$plot <- cou$PLOT[matches]
  sum_cou$graz <- cou$GRAZ[matches]
  sum_cou$aridity <- cou$ARIDITY.v3[matches]
  sum_cou$microsite <- cou$Microsite[matches]
  sum_cou$microsite_ID <- cou$ID_Microsite[matches]
  sum_cou$replicate_no <- cou$Number.of.replicate[matches]
  
  #Add the total vegetation cover in each uniquerep
  totalcover <- tapply(cou$Cover, cou$microsite_rep, sum)
  coverdf <- data.frame(totalcover)
  #order the rownames of coverdf and cou in the same order
  index <- match(sum_cou$uniquerep, rownames(coverdf)) #positions of uniquerep in rownames(coverdf)
  coverdfsort <- coverdf[index, ]
  sum_cou$totalcover <- coverdfsort
  #replace NA values with zero's
  sum_cou$totalcover[is.na(sum_cou$totalcover)] <- 0
  
  #Add the shannon diversity  and species richness in each uniquerep
  replist <- sum_cou$uniquerep
  l = 1
  for (n in 1:length(replist)) {
    subset <- cou[which(cou$microsite_rep == replist[n]),] #subset by uniquerep
    #Shannon diversity
    subset$p <- subset$Cover / sum(subset$Cover)
    subset$prod <- subset$p * log(subset$p)
    shannon <- -sum(subset$prod)
    sum_cou[l, which(colnames(sum_cou) == "shannondiv_cover")] <- shannon
    #Shannondiv is zero when there is one species
    #Shannondiv is NA when there are no species, or when there are NA abundance values
    
    #Species richness
    subset2 <- subset[which(!is.na(subset$Species.within.quadrat)),] #keep only rows that do not have NA values in the Species within quadrat column
    sprichness <- length(subset2$Species.within.quadrat)
    sum_cou[l, which(colnames(sum_cou) == "sprichness")] <- sprichness
    l = l+1
  }
  
  assign(paste("sum", countrynames[i], sep= "_"), sum_cou) #name the dataframe according to the country
} #####End loop through countries

#Check that is an equal number of "2" and "1" microsites
length(which(sum_algeria$microsite == 1))
length(which(sum_algeria$microsite == 2))

length(which(sum_argentina$microsite == 1))
length(which(sum_argentina$microsite == 2))

length(which(sum_australia$microsite == 1))
length(which(sum_australia$microsite == 2))

length(which(sum_chile$microsite == 1))
length(which(sum_chile$microsite == 2))

length(which(sum_chinachong$microsite == 1))
length(which(sum_chinachong$microsite == 2))

length(which(sum_chinaxin$microsite == 1))
length(which(sum_chinaxin$microsite == 2))

length(which(sum_iranabedi$microsite == 1))
length(which(sum_iranabedi$microsite == 2))

length(which(sum_iranfarzam$microsite == 1))
length(which(sum_iranfarzam$microsite == 2))

length(which(sum_israel$microsite == 1))
length(which(sum_israel$microsite == 2))

length(which(sum_namibiablaum$microsite == 1))
length(which(sum_namibiablaum$microsite == 2))

length(which(sum_namibiawang$microsite == 1))
length(which(sum_namibiawang$microsite == 2))

length(which(sum_southafrica$microsite == 1))
length(which(sum_southafrica$microsite == 2))

length(which(sum_spainmaestre$microsite == 1))
length(which(sum_spainmaestre$microsite == 2))

length(which(sum_spainrey$microsite == 1))
length(which(sum_spainrey$microsite == 2))




###For each country, Calculate the NIntc for each nurse-open pair per plot
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")

sum_list <- c("sum_algeria", "sum_argentina", "sum_australia", "sum_chile", "sum_chinachong", "sum_chinaxin", "sum_iranabedi", "sum_iranfarzam",
              "sum_israel", "sum_namibiablaum", "sum_namibiawang", "sum_southafrica",  "sum_spainmaestre", "sum_spainrey")

result_list <- c("result_algeria", "result_argentina", "result_australia", "result_chile", "result_chinachong", "result_chinaxin", 
                 "result_iranabedi", "result_iranfarzam", "result_israel", "result_namibiablaum", "result_namibiawang", "result_southafrica", 
                 "result_spainmaestre", "result_spainrey")

column_names <- c("country", "ID", "site_ID", "plot", "graz", "aridity","replicate_no", "nurse", "NIntc_richness", "NIntc_cover", "NIntc_shannon", 
                  "NInta_richness", "NInta_cover", "NInta_shannon")

for (n in 1:length(sum_list)) {
  sum_cou <- get(sum_list[n])
  sitelist <- unique(sum_cou$site_ID)
  plotlist <- unique(sum_cou$plot)
  maxreplist <- unique(sum_cou$replicate_no)
  assign(paste("result", countrynames[n], sep= "_"), #create an empty results table for each country
         data.frame(matrix(nrow = length(sitelist)*length(plotlist)*length(maxreplist), ncol = length(column_names))))
  result_cou <- (get(result_list[n])) #assign this empty table to result_cou
  colnames(result_cou) = column_names
  l = 1
  
  #Subset per replicate in a plot in a site
  for (j in sitelist) {
    for (k in plotlist) {
      sub  <- sum_cou[which(sum_cou$site_ID == j), ]
      sub2 <- sub[which(sub$plot == k),]
      replicatelist <- unique(sub2$replicate_no)
        for (i in replicatelist) {
        sub3 <- sub2[which(sub2$replicate_no == i),]
        
        #populate the results table with the variables corresponding to each plot
        result_cou[l,1] <- countrynames[n]
        result_cou[l,2] <- sub3$ID[1]
        result_cou[l,3] <- sub3$site_ID[1]
        result_cou[l,4] <- sub3$plot[1]
        result_cou[l,5] <- sub3$graz[1]
        result_cou[l,6] <- sub3$aridity[1]
        result_cou[l,7] <- sub3$replicate_no[1]
        result_cou[l,8] <- sub3[which(sub3$microsite == 2) , ]$microsite_ID #add the name of the nurse
        
        
        ##NIntc
        
        #Calculate NIntc_richness using species richness
        sprichness_bare <- sub3[which(sub3$microsite == 1), ]$sprichness
        sprichness_nurse <- sub3[which(sub3$microsite == 2), ]$sprichness
        NIntc_richness <- 2*((sprichness_nurse - sprichness_bare)/((sprichness_nurse + sprichness_bare) + abs(sprichness_nurse - sprichness_bare)))
        result_cou[l, which(colnames(result_cou) == "NIntc_richness")] <- NIntc_richness
        
        #Calculate NIntc_cover using veg cover
        cover_bare <- sub3[which(sub3$microsite == 1), ]$totalcover
        cover_nurse <- sub3[which(sub3$microsite == 2), ]$totalcover
        NIntc_cover <- 2*((cover_nurse - cover_bare)/((cover_nurse + cover_bare) + abs(cover_nurse - cover_bare)))
        result_cou[l, which(colnames(result_cou) == "NIntc_cover")] <- NIntc_cover
        
        #Calculate NIntc_shannon using shannon diversity 
        shannon_bare <- sub3[which(sub3$microsite == 1), ]$shannondiv
        shannon_nurse <- sub3[which(sub3$microsite == 2), ]$shannondiv
        NIntc_shannon <- 2*((shannon_nurse - shannon_bare)/((shannon_nurse + shannon_bare) + abs(shannon_nurse - shannon_bare)))
        result_cou[l, which(colnames(result_cou) == "NIntc_shannon")] <- NIntc_shannon
        
        ###NInta
        
        #Calculate NInta_richness using species richness
        NInta_richness <- 2*((sprichness_nurse - sprichness_bare)/((sprichness_bare) + abs(sprichness_nurse - sprichness_bare)))
        result_cou[l, which(colnames(result_cou) == "NInta_richness")] <- NInta_richness
        
        #Calculate NInta_cover using veg cover
        NInta_cover <- 2*((cover_nurse - cover_bare)/((cover_bare) + abs(cover_nurse - cover_bare)))
        result_cou[l, which(colnames(result_cou) == "NInta_cover")] <- NInta_cover
        
        #Calculate NInta_shannon using shannon diversity 
        NInta_shannon <- 2*((shannon_nurse - shannon_bare)/((shannon_bare) + abs(shannon_nurse - shannon_bare)))
        result_cou[l, which(colnames(result_cou) == "NInta_shannon")] <- NInta_shannon
        
        l = l+1
        #NIntc returns NA values when any of the performance variables are zero.
        
        result_cou <- result_cou[which(!is.na(result_cou[,2])) , ] #remove empty rows
        
      }}}
   
   assign(paste("result", countrynames[n], sep= "_"), result_cou) #name the result dataframe according to the country
}#end loop to calculate NIntc results


###Join the results of all the countries together in one big table
all_result <- rbind(result_algeria, result_argentina, result_australia, result_chile, result_chinachong, result_chinaxin, result_iranabedi, result_iranfarzam,
                    result_israel, result_namibiablaum, result_namibiawang, result_southafrica, result_spainmaestre, result_spainrey)
write.csv(all_result, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\NIntc_results_allcountries_26Sep.csv")

#graz = Grazing intensity within the plot (0 ungrazed; 1:low grazing; 2:medium grazing; 3:high grazing)
#plot = Inverse of graz, Original Lab code for grazing intensity within the plot (1:high grazing; 2:medium grazing; 3:low grazing; 4: ungrazed)
#aridity = the ARIDITY_v3 value from the BIODESERT sites information file



