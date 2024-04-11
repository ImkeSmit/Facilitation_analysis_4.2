###Models and other descriptive statistics regarding NIntc across grazing and gradients of MAT and RAI###
library(glmmTMB)
library(car)
library(lsmeans)
library(multcomp)
library(multcompView)
library(MuMIn)
library(dplyr)
library(tidyverse)
library(tidylog)

##Import results of NIntc calculations (from interaction-gradient analysis scripts)
all_result <- read.csv("Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv", row.names = 1)
all_result$site_ID <- as.factor(all_result$site_ID)
all_result$ID <- as.factor(all_result$ID)
##Treat grazing as an unordered factor!
all_result$graz <- as.factor(all_result$graz)

#import siteinfo, so that we can add RAI and AMT
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") 
#select the columns we want to add
siteinfo <- siteinfo[, which(colnames(siteinfo) %in% c("ID", "AMT", "RAI"))]
siteinfo$ID <- as.factor(siteinfo$ID)
#join to all_result
all_result <- all_result |> 
  left_join(siteinfo, by = "ID")

#NIntc is bounded beween -1 and 1, so binomial family is appropriate
#However the function requires that the response be bounded between 0 and 1, so rescale NIntc
#x-min/max- min (here the formula is just already simplified)
all_result$NIntc_richness_binom <- (all_result$NIntc_richness + 1)/2
all_result$NIntc_cover_binom <- (all_result$NIntc_cover + 1)/2
all_result$NIntc_shannon_binom <- (all_result$NIntc_shannon + 1)/2

#x-min/max- min
all_result$NInta_richness_binom <- (all_result$NInta_richness - (-1)) / (2 - (-1))
all_result$NInta_cover_binom <- (all_result$NInta_cover - (-1)) / (2 - (-1))
all_result$NInta_shannon_binom <- (all_result$NInta_shannon - (-1)) / (2 - (-1))




###Correlations####
#Is NInta and NIntc correlated?
plot(all_result$NInta_richness, all_result$NIntc_richness)
plot(all_result$NInta_shannon, all_result$NIntc_shannon)
plot(all_result$NInta_cover, all_result$NIntc_cover)
cordat <- all_result[-which(is.na(all_result$NIntc_cover)) , ] #remove NA values
cor.test(cordat$NInta_cover, cordat$NIntc_cover, method = "spearman")

cordat <- all_result[-which(is.na(all_result$NIntc_richness)) , ] #remove NA values
cor.test(cordat$NInta_richness, cordat$NIntc_richness, method = "spearman")

cordat <- all_result[-which(is.na(all_result$NIntc_shannon)) , ] #remove NA values
cor.test(cordat$NInta_shannon, cordat$NIntc_shannon, method = "spearman")


###Is NIntc richness, cover and diversity correlated
#Richness and shannon
plot(all_result$NIntc_richness, all_result$NIntc_shannon)
cordat <- all_result[-which(is.na(all_result$NIntc_shannon)) , ] #remove NA values
cor.test(cordat$NIntc_richness, cordat$NIntc_shannon, method = "spearman") #0.8483084

#Richness and cover
plot(all_result$NIntc_richness, all_result$NIntc_cover)
cordat <- all_result[-which(is.na(all_result$NIntc_cover)) , ] #remove NA values
cor.test(cordat$NIntc_richness, cordat$NIntc_cover, method = "spearman") #0.7377719

#Shannon and cover
plot(all_result$NIntc_shannon, all_result$NIntc_cover)
cordat <- all_result[-which(is.na(all_result$NIntc_shannon)) , ] #remove NA values
cor.test(cordat$NIntc_shannon, cordat$NIntc_cover, method = "spearman") #0.346839

###Is NInta richness, cover and diversity correlated
#Richness and shannon
plot(all_result$NInta_richness, all_result$NInta_shannon)
cordat <- all_result[-which(is.na(all_result$NInta_shannon)) , ] #remove NA values
cor.test(cordat$NInta_richness, cordat$NInta_shannon, method = "spearman") #0.8483084

#Richness and cover
plot(all_result$NInta_richness, all_result$NInta_cover)
cordat <- all_result[-which(is.na(all_result$NInta_cover)) , ] #remove NA values
cor.test(cordat$NInta_richness, cordat$NInta_cover, method = "spearman") #0.7344117 

#Shannon and cover
plot(all_result$NInta_shannon, all_result$NInta_cover)
cordat <- all_result[-which(is.na(all_result$NInta_shannon)) , ] #remove NA values
cor.test(cordat$NInta_shannon, cordat$NInta_cover, method = "spearman") #0.3414124


###Generalised linear modelling with glmmTMB####
formula_table <- read.csv("Facilitation data\\results\\nint_models_allsubsets_AMT_RAI.csv") |> 
  separate_wider_delim(formula, delim = "~", names = c("response", "predictors")) |> 
  select(predictors) |> 
  distinct(predictors) |> 
  add_row(predictors = "1+(1|site_ID)")  #add the null model


###Loop through the formulas for NIntc####
#Create a table for results
results_table <- data.frame(Response = character(), Model = character(), Chisq = numeric(), 
                            Df = integer(), Pr_value = numeric(), AIC = numeric(), 
                            Warnings = character(), row.names = NULL)

# Initialize warning_msg outside the loop
warning_msg <- ""

##Also loop through response variables
#loop through Nintc first
response_list <- c("NIntc_richness_binom", "NIntc_cover_binom", "NInta_richness_binom", "NInta_cover_binom")
datalist = c("all_result", "all_result", "all_result", "all_result")

##LOOP THROUGH MODELS STARTS HERE##
#Loop through response variables
for(r in 1:length(response_list)) {
  
  response_var <- response_list[r]  
  data = get(datalist[r])
  
  #Loop through response variables
  for (f in 1:nrow(formula_table)) {
    
    predictors <- as.character(formula_table[f, ])
    formula <- as.formula(paste(response_var, "~",  predictors))
    
    # Clear existing warning messages
    warnings()
    
    # Initialize anova_result and AIC_model outside the tryCatch block
    anova_result <- NULL
    AIC_model <- NULL
    
    tryCatch( #tryCatch looks for errors and warinngs in the expression
      expr = {
        model <- glmmTMB(formula, family = binomial, data = data)
        
        # Perform Anova 
        anova_result <- Anova(model, type = 2)
        # Get AIC
        AIC_model <- AIC(model)
        
        warning_messages <- warnings()
        
        ##Do nothing if the warinng is about non integer successes
        # Check for the non-integer #successes warning
        if ("non-integer #successes" %in% warning_messages) {
          # Handle non-integer #successes warning (e.g., print a message)
          message("Ignoring non-integer #successes warning")
        }
        
        #Print the warning message if it is about model fit
        # Check for other warnings, excluding the non-integer #successes warning
        other_warnings <- setdiff(warning_messages, "non-integer #successes")
        if (length(other_warnings) > 0) {
          warning_msg <- paste("warning :", as.character(other_warnings), collapse = "; ")
          message(paste("WARNING_", "r =" , response_var, "f =", f, warning_msg))
        }
      }, 
      
      #Also show me errors
      error = function(e) {
        message(paste("ERROR_", "r =" , response_var, "f =", f, conditionMessage(e)))
        print(e)
      }
    )
    
    # Extract relevant information
    result_row <- data.frame(Response = response_var,
                             Model = paste(response_var, "~",  predictors), 
                             Chisq = ifelse(!is.null(anova_result), anova_result$Chisq[1], NA), 
                             Df = ifelse(!is.null(anova_result), anova_result$"Df"[1], NA), 
                             Pr_value = ifelse(!is.null(anova_result), anova_result$"Pr(>Chisq)"[1], NA), 
                             AIC = ifelse(!is.null(AIC_model), AIC_model, NA),
                             Warnings = warning_msg)
    
    
    results_table <- rbind(results_table, result_row)
  }
}
##if there is no AIC value, the model did not converge
results_table


###Species preference along aridity####
#Does aridity influence how many species grow exclusively in bare, open and both microsites (Pbare and Pdominant analysis)
#We require raw country data
data_files <- list.files("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Countriesv3")
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Countriesv3\\",
                          data_files[i])))
}

##This is a PLOTLEVEL analysis
##Calculate the number of species that prefer each microsite in each plot of each country
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")

preflist <- c("pref_algeria", "pref_argentina", "pref_australia", "pref_chile", "pref_chinachong", "pref_chinaxin", 
              "pref_iranabedi", "pref_iranfarzam", "pref_israel", "pref_namibiablaum", "pref_namibiawang", 
              "pref_southafrica",  "pref_spainmaestre", "pref_spainrey")

column_names <- c("site_ID", "ID", "aridity", "graz", "prop_bare_only", "prop_nurse_only", "prop_both")


for (i in 1:length(countrynames)) {
  cou <- get(countrynames[i]) #working with one country at a time
  assign(paste("pref", countrynames[i], sep= "_"), #create an empty pref table that will have the values for each site in a country
         data.frame(matrix(nrow = length(unique(as.factor(cou$ID))), ncol = length(column_names))))
  pref_cou <- (get(preflist[i])) #assign this empty table to result_cou
  colnames(pref_cou) = column_names
  
  plotlist <- c(unique(cou$ID))
  l = 1
  
  for (f in 1:length(plotlist)) {
    plot <- cou[which(cou$ID == plotlist[f]) , ] #select a plot in a country
    plot <- plot[which(!is.na(plot$Species.within.quadrat)) , ] #remove rows that had no species in a microsite
    
    all_sp <- unique(plot$Species.within.quadrat) #all species at the plot
    
    bare <- plot[which(plot$Microsite == 1) , ] #select only bare microsites
    bare_sp <- unique(bare$Species.within.quadrat) #species in bare microsites
    
    nurse <- plot[which(plot$Microsite == 2) , ] #select only nurse microsites
    nurse_sp <- unique(nurse$Species.within.quadrat) #species in nurse microsites
    
    bare_match <- match(nurse_sp, bare_sp) #the indexes in bare_sp that correspond to the names in nurse_sp
    #eg the second name in nurse_sp corresponds to the 4th name in bare sp. Thus the match is 4
    
    nurse_match <- match(bare_sp, nurse_sp) #the indexes in nurse_sp that correspond to the names in bare_sp
    
    #no of species occuring in bare and nurse microsites
    nboth <- length(nurse_match[which(!is.na(nurse_match))]) 
    prop_both <- (nboth/length(all_sp)) #proportion of sp that grow in both microsites
    
    #number of species that only grow in nurse/bare microsites
    if (nboth == 0) {
      nnurse_only = length(nurse_sp)
      nbare_only = length(bare_sp)
      
    }else {
      nnurse_only <- length(nurse_sp[-nurse_match[which(!is.na(nurse_match))]])
      nbare_only <- length(bare_sp[-bare_match[which(!is.na(bare_match))]])
    }
    
    prop_nurse <- (nnurse_only/length(all_sp)) #proportion of sp that only grow in nurse microsites
    
    prop_bare <- (nbare_only/length(all_sp)) #proportion of sp that only grow in bare microsites
    
    
    pref_cou[l,1] <- plot$SITE_ID[1]
    pref_cou[l,2] <- plotlist[f]
    pref_cou[l,3] <- plot$ARIDITY.v3[1] #aridity of the plot
    pref_cou[l,4] <- plot$GRAZ[1] #grazing pressure of the plot
    pref_cou[l,5] <- prop_bare
    pref_cou[l,6] <- prop_nurse
    pref_cou[l,7] <- prop_both
    
    l = l+1
  }
  assign(paste("pref", countrynames[i], sep= "_"), pref_cou) #name the dataframe according to the country
  #Now moving on to the next country to make a separate pref table for that country
}

#BInd all the pref tables together
sp_preference <- rbind(pref_algeria, pref_argentina, pref_australia, pref_chile, pref_chinachong, pref_chinaxin, 
                       pref_iranabedi, pref_iranfarzam, pref_israel, pref_namibiablaum, pref_namibiawang, 
                       pref_southafrica,  pref_spainmaestre, pref_spainrey)
#Order ID according to aridity
sp_preference <- sp_preference[order(sp_preference$aridity),]
sp_preference$ID <- as.factor(sp_preference$ID)
sp_preference$site_ID <- as.factor(sp_preference$site_ID)
sp_preference$graz <- as.factor(sp_preference$graz)
#Add a quadratic aridity term
sp_preference$arid_sq <- sp_preference$aridity^2

##Write to a .csv file to use in graphing
#write.csv(sp_preference, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\results\\plotlevels_sp_preference_6Feb2024.csv")
sp_preference <- read.csv("Facilitation data\\results\\plotlevels_sp_preference_6Feb2024.csv", row.names = 1)
sp_preference$ID <- as.factor(sp_preference$ID)
sp_preference$site_ID <- as.factor(sp_preference$site_ID)
sp_preference$graz <- as.factor(sp_preference$graz)


#join AMT and RAI to sp_preference
#import siteinfo
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") 
#select the columns we want to add
siteinfo <- siteinfo[, which(colnames(siteinfo) %in% c("ID", "AMT", "RAI"))]
siteinfo$ID <- as.factor(siteinfo$ID)
#join to all_result
sp_preference <- sp_preference |> 
  left_join(siteinfo, by = "ID")

#In how many of the plots did the majority of sp prefer nurse microsites?
length(sp_preference[which(sp_preference$prop_nurse_only > sp_preference$prop_both) , ]) #8

#In how many of the sites did the majority of sp prefer open microsites?
length(sp_preference[which(sp_preference$prop_bare_only > sp_preference$prop_both) , ]) #8

#On average, how many species ocurred in both nurse and bare microsites
avg_both <- sum(sp_preference$prop_both)/nrow(sp_preference)


###Linear modelling: Does the proportion of NURSE only sp change along grazing and aridity?
##in text the proportion of nurse only species = Pdominant
#Create a table for results
prefmod_results_table <- data.frame(Response = character(), Model = character(), Chisq = numeric(), 
                            Df = integer(), Pr_value = numeric(), AIC = numeric(), 
                            Warnings = character(), row.names = NULL)

# Initialize warning_msg outside the loop
warning_msg <- ""

##Also loop through response variables
response_list <- c("prop_nurse_only", "prop_bare_only", "prop_both")
datalist = c("sp_preference", "sp_preference", "sp_preference")

##LOOP THROUGH MODELS STARTS HERE##
#Loop through response variables
for(r in 1:length(response_list)) {
  
  response_var <- response_list[r]  
  data = get(datalist[r])
  
  #Loop through response variables
  for (f in 1:nrow(formula_table)) {
    
    predictors <- as.character(formula_table[f, ])
    formula <- as.formula(paste(response_var, "~",  predictors))
    
    # Clear existing warning messages
    warnings()
    
    # Initialize anova_result and AIC_model outside the tryCatch block
    anova_result <- NULL
    AIC_model <- NULL
    
    tryCatch( #tryCatch looks for errors and warinngs in the expression
      expr = {
        model <- glmmTMB(formula, family = binomial, data = data)
        
        # Perform Anova 
        anova_result <- Anova(model, type = 2)
        # Get AIC
        AIC_model <- AIC(model)
        
        warning_messages <- warnings()
        
        ##Do nothing if the warinng is about non integer successes
        # Check for the non-integer #successes warning
        if ("non-integer #successes" %in% warning_messages) {
          # Handle non-integer #successes warning (e.g., print a message)
          message("Ignoring non-integer #successes warning")
        }
        
        #Print the warning message if it is about model fit
        # Check for other warnings, excluding the non-integer #successes warning
        other_warnings <- setdiff(warning_messages, "non-integer #successes")
        if (length(other_warnings) > 0) {
          warning_msg <- paste("warning :", as.character(other_warnings), collapse = "; ")
          message(paste("WARNING_", "r =" , response_var, "f =", f, warning_msg))
        }
      }, 
      
      #Also show me errors
      error = function(e) {
        message(paste("ERROR_", "r =" , response_var, "f =", f, conditionMessage(e)))
        print(e)
      }
    )
    
    # Extract relevant information
    result_row <- data.frame(Response = response_var,
                             Model = paste(response_var, "~",  predictors), 
                             Chisq = ifelse(!is.null(anova_result), anova_result$Chisq[1], NA), 
                             Df = ifelse(!is.null(anova_result), anova_result$"Df"[1], NA), 
                             Pr_value = ifelse(!is.null(anova_result), anova_result$"Pr(>Chisq)"[1], NA), 
                             AIC = ifelse(!is.null(AIC_model), AIC_model, NA),
                             Warnings = warning_msg)
    
    
    prefmod_results_table <- rbind(prefmod_results_table, result_row)
  }
}
##if there is no AIC value, the model did not converge
prefmod_results_table


###CHisq tests of species association with nurse or bare microsites####
###First we need to get the number times a species is present/absent in each microsite
#Import the country_v3 data
data_files <- list.files("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Countriesv3")
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\Countriesv3\\",
                          data_files[i])))
}

vars <- c("SITE_ID", "ID", "GRAZ", "ARIDITY.v3", "Microsite", "ID_Microsite", "Number.of.replicate", "Species.within.quadrat" )
data <- rbind(algeria[ , which(colnames(algeria) %in% vars)], argentina[ , which(colnames(argentina) %in% vars)], australia[ , which(colnames(australia) %in% vars)], chile[ , which(colnames(chile) %in% vars)], 
              chinachong[ , which(colnames(chinachong) %in% vars)], chinaxin[ , which(colnames(chinaxin) %in% vars)], iranabedi[ , which(colnames(iranabedi) %in% vars)], iranfarzam[ , which(colnames(iranfarzam) %in% vars)], 
              israel[ , which(colnames(israel) %in% vars)], namibiablaum[ , which(colnames(namibiablaum) %in% vars)], namibiawang[ , which(colnames(namibiawang) %in% vars)], southafrica[ , which(colnames(southafrica) %in% vars)],  
              spainmaestre[ , which(colnames(spainmaestre) %in% vars)], spainrey[ , which(colnames(spainrey) %in% vars)])

column_names <- c("site_ID", "ID", "graz", "aridity", "species", "nurse_p", "nurse_a", "bare_p", "bare_a")
#nurse_p is the number of nurse microsites in which a species is present
#nurse_a is the total number of replicates minus the number of nurse presences
#same goes for the bare microsites

plotlist <- unique(data$ID)

for (f in 1:length(plotlist)) {
  
  plot <- data[which(data$ID == plotlist[f]) , ]
  plot_species <- c(unique(plot$Species.within.quadrat))
  plot_species <- plot_species[which(!is.na(plot_species))]
  
  #subset for each species in a plot
  for(k in 1:length(plot_species)) {
    
    focus_sp <- plot[which(plot$Species.within.quadrat == plot_species[k]) , ]
    
    nurse_p <- nrow(focus_sp[which(focus_sp$Microsite == 2) , ])
    nurse_a <- length(unique(plot$Number.of.replicate)) - nurse_p
    
    bare_p <- nrow(focus_sp[which(focus_sp$Microsite == 1) , ])
    bare_a <- length(unique(plot$Number.of.replicate)) - bare_p
    
    if(f+k == 2) { #only make a table for the first result, afterwards we will just rbind to it
      #create an empty table to put the results in 
      sp_PrAb <- data.frame(matrix(nrow = 1, ncol = length(column_names)))
      colnames(sp_PrAb) <- column_names
      
      sp_PrAb$site_ID <- plot$SITE_ID[1]
      sp_PrAb$ID <- plotlist[f]
      sp_PrAb$graz <- plot$GRAZ[1]
      sp_PrAb$aridity <- plot$ARIDITY.v3[1]
      sp_PrAb$species <- plot_species[k]
      sp_PrAb$nurse_p <- nurse_p
      sp_PrAb$nurse_a <- nurse_a
      sp_PrAb$bare_p <- bare_p
      sp_PrAb$bare_a <- bare_a
      
    } else { #now we just rbind to the table we made in the above loop
      temp <- cbind(plot$SITE_ID[1], plotlist[f], plot$GRAZ[1], plot$ARIDITY.v3[1],  plot_species[k] , nurse_p, nurse_a, 
                    bare_p, bare_a)
      colnames(temp) <- column_names
      sp_PrAb <- rbind(sp_PrAb, temp)
    }
  }
}


###MAke a contingency table for each species in sp_PrAb

sp_PrAb$nurse_p <- as.numeric(sp_PrAb$nurse_p)
sp_PrAb$nurse_a <- as.numeric(sp_PrAb$nurse_a)
sp_PrAb$bare_a <- as.numeric(sp_PrAb$bare_a)
sp_PrAb$bare_p <- as.numeric(sp_PrAb$bare_p)

# Create an empty list to store contingency tables
contingency_tables <- list()

# Iterate through each row in the data frame
for (i in 1:nrow(sp_PrAb)) {
  row <- sp_PrAb[i, ]  # Extract the current row
  
  # Extract values from the row
  nurse_p <- row[['nurse_p']]
  nurse_a <- row[['nurse_a']]
  bare_p <- row[['bare_p']]
  bare_a <- row[['bare_a']]
  
  # Create a contingency table
  contingency_table <- matrix(c(nurse_p, nurse_a, bare_p, bare_a), nrow = 2, byrow = TRUE)
  colnames(contingency_table) <- c('nurse', 'bare')
  rownames(contingency_table) <- c('p', 'a')
  
  # Convert to a table object
  contingency_table <- as.table(contingency_table)
  
  # Add the contingency table to the list
  contingency_tables[[i]] <- contingency_table
}

# Now, contingency_tables is a list of contingency tables, one for each row in sp_PrAb, so one for each species and ID combination
# You can access individual contingency tables using indexing, e.g., contingency_tables[[1]], contingency_tables[[2]], etc.


###Now we need to do the Chisquared tests on each contingency table
# Create an empty data frame to store Chi-squared test results
Chisq_results <- data.frame(table_no = numeric(0), p_value = numeric(0), exp_nurse_p= numeric(0), exp_nurse_a= numeric(0), 
                            exp_bare_p= numeric(0), exp_bare_a= numeric(0), res_nurse_p = numeric(0), res_nurse_a = numeric(0), 
                            res_bare_p = numeric(0), res_bare_a = numeric(0))

# Iterate through each contingency table
for (i in 1:length(contingency_tables)) {
  focus_table <- contingency_tables[[i]]
  
  # Perform the Chi-squared test
  chi_squared_result <- chisq.test(focus_table)
  
  # Extract the p-value from the test result
  p_value <- chi_squared_result$p.value
  
  #extract the expected results 
  exp <- chi_squared_result$expected
  
  #extract the standard residuals
  res <- chi_squared_result$residuals
  
  # Add the row number, p-value, expected values and std residuals to the data frame
  Chisq_results <- rbind(Chisq_results, data.frame(table_no = i, p_value = p_value, 
                                                   exp_nurse_p = exp[1,1], exp_nurse_a = exp[2,1],
                                                   exp_bare_p = exp[1,2], exp_bare_a = exp[2,2], 
                                                   res_nurse_p = res[1,1], res_nurse_a = res[2,1],
                                                   res_bare_p = res[1,2], res_bare_a = res[2,2] ))
}##There are warnings that it the results may not be accurate??
#In chisq.test(focus_table) : Chi-squared approximation may be incorrect

#Now we can merge the ID and species names from sp_PrAb to Chisq results
sp_PrAb$table_no <- rownames(sp_PrAb)
Chisq_results <- merge(Chisq_results, sp_PrAb, by = "table_no")

###Classify entries whether they have neutral, nurse or bare association, or whether their results are unreliable
##Add a variable to chisq_filtered that shows whether the species is significantly associated with the nurse/bare microsite or if it shows no association
Chisq_results$association <- NA

for(i in 1:nrow(Chisq_results)) { 
  #If any of the expected values is lower than 5, the Chisq approximation may not be correct
  #These species are too rare to trust their results
  if(Chisq_results[i , ]$exp_nurse_p <5 | Chisq_results[i , ]$exp_nurse_a <5 | Chisq_results[i , ]$exp_bare_p <5 | Chisq_results[i , ]$exp_bare_a <5) {
    Chisq_results[i , ]$association <- "too_rare" 
  } else { 
    if(Chisq_results[i , ]$p_value > 0.05) { #if p>0.05 there is no significant association to any microsite
      Chisq_results[i , ]$association <- "neutral"
    } else {
      if(Chisq_results[i , ]$res_nurse_p > 0) { #if p<0.05 and res_nurse_p>0 then there is an attraction between the species presence and the nurse microsite
        Chisq_results[i , ]$association <- "nurse"
      } else {
        if(Chisq_results[i , ]$res_bare_p > 0) { #if p<0.05 and res_bare_p>0 then there is an attraction between the species presence and the bare microsite
          Chisq_results[i , ]$association <- "bare"
        }
      }
    }
  }
}


#write to csv file
#write.csv(Chisq_results, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\results\\Chisq_results_6Feb2024.csv")
Chisq_results <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\results\\Chisq_results_6Feb2024.csv", row.names = 1)

#How many sp significantly associated with the nurse?
Chisq_results |> 
  filter(association == "nurse") |> 
  distinct(species) |> 
  summarise(nsp = n()) #85

#How many sp significantly associated with the bare microsite?
Chisq_results |> 
  filter(association == "bare") |> 
  distinct(species) |> 
  summarise(nsp = n()) #38

#How many sp show neutral association?
Chisq_results |> 
  filter(association == "neutral") |> 
  distinct(species) |> 
  summarise(nsp = n()) #241

#How many sp are adequately sampled?
Chisq_results |> 
  filter(!association == "too_rare") |> 
  distinct(species) |> 
  summarise(nsp = n()) #305

#How many species in total
Chisq_results |> 
  select(species) |> 
  distinct(species) |> 
  summarise(nsp = n()) #720

####Is species association influenced by aridity and graz?###
##First, get the proportion of species in a plot that show a certain association
chisq_reduced <- Chisq_results[-which(Chisq_results$association == "too_rare") , ] #remove the rare sp

prop_chisq_reduced <- chisq_reduced %>%
  group_by(ID, association) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(ID) %>%
  mutate(Proportion = Count / sum(Count))
prop_chisq_reduced <- as.data.frame(prop_chisq_reduced)
prop_chisq_reduced$percentage <- prop_chisq_reduced$Proportion*100
#Add aridity to prop_chisq
ymerge <- Chisq_results[-which(duplicated(Chisq_results$ID) == TRUE) , which(colnames(Chisq_results) %in% c("site_ID", "ID", "graz", "aridity"))]
prop_chisq_reduced <- merge(prop_chisq_reduced, ymerge, 
                            by = "ID", all.x = FALSE, all.y = FALSE, no.dups = TRUE)
#add aridity squared
prop_chisq_reduced$arid_sq <- (prop_chisq_reduced$aridity)^2
prop_chisq_reduced$site_ID <- as.factor(prop_chisq_reduced$site_ID)
prop_chisq_reduced$ID <- as.factor(prop_chisq_reduced$ID)
prop_chisq_reduced$graz <- as.factor(prop_chisq_reduced$graz)

###Now we can make the models
##PROPORTION OF BARE ASSOCIATIONS##
baredat <- prop_chisq_reduced[which(prop_chisq_reduced$association == "bare") , ]

nullmod_bare <- glmmTMB(Proportion ~ 1 +(1|site_ID),  
                        family = binomial, data = baredat)

##graz
bare_mod1 <- glmmTMB(Proportion ~ graz +(1|site_ID),  
                     family = binomial, data = baredat)
summary(bare_mod1)
Anova(bare_mod1)
anova(nullmod_bare, bare_mod1) 
lsmeans(bare_mod1, specs = "graz")
cld(lsmeans(bare_mod1, specs = "graz"), Letters = "abcdefg")

r.squaredGLMM(bare_mod1) #take the theoretical

##aridity
bare_mod2 <- glmmTMB(Proportion ~ aridity +(1|site_ID),  
                     family = binomial, data = baredat)

##aridity + arid_sq
bare_mod3 <- glmmTMB(Proportion ~ aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = baredat)

##graz + aridity
bare_mod4 <- glmmTMB(Proportion ~ graz +aridity + (1|site_ID),  
                     family = binomial, data = baredat)

##graz + aridity + arid_sq
bare_mod5 <- glmmTMB(Proportion ~ graz +aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = baredat)

##graz*aridity
bare_mod6 <- glmmTMB(Proportion ~ graz*aridity + (1|site_ID),  
                     family = binomial, data = baredat)
summary(bare_mod6)

##Graz*aridity + graz*arid_sq
bare_mod7 <- glmmTMB(Proportion ~ graz*aridity + graz*arid_sq + (1|site_ID),  
                     family = binomial, data = baredat) 


AIC(nullmod_bare, bare_mod1, bare_mod2, bare_mod3, bare_mod4, bare_mod5, bare_mod6, bare_mod7)
##nullmodel has the lowest AIC



##PROPORTION OF NURSE ASSOCIATIONS##
nursedat <- prop_chisq_reduced[which(prop_chisq_reduced$association == "nurse") , ]

nullmod_nurse <- glmmTMB(Proportion ~ 1 +(1|site_ID),  
                         family = binomial, data = nursedat)

##graz
nurse_mod1 <- glmmTMB(Proportion ~ graz +(1|site_ID),  
                      family = binomial, data = nursedat)
summary(nurse_mod1)
Anova(nurse_mod1)
anova(nullmod_nurse, nurse_mod1) 
lsmeans(nurse_mod1, specs = "graz")
cld(lsmeans(nurse_mod1, specs = "graz"), Letters = "abcdefg")

r.squaredGLMM(nurse_mod1) #take the theoretical

##aridity
nurse_mod2 <- glmmTMB(Proportion ~ aridity +(1|site_ID),  
                      family = binomial, data = nursedat)

##aridity + arid_sq
nurse_mod3 <- glmmTMB(Proportion ~ aridity + arid_sq +(1|site_ID),  
                      family = binomial, data = nursedat)

##graz + aridity
nurse_mod4 <- glmmTMB(Proportion ~ graz +aridity + (1|site_ID),  
                      family = binomial, data = nursedat)

##graz + aridity + arid_sq
nurse_mod5 <- glmmTMB(Proportion ~ graz +aridity + arid_sq +(1|site_ID),  
                      family = binomial, data = nursedat)

##graz*aridity
nurse_mod6 <- glmmTMB(Proportion ~ graz*aridity + (1|site_ID),  
                      family = binomial, data = nursedat)
summary(nurse_mod6)

##Graz*aridity + graz*arid_sq
nurse_mod7 <- glmmTMB(Proportion ~ graz*aridity + graz*arid_sq + (1|site_ID),  
                      family = binomial, data = nursedat) ##model convergence problem


AIC(nullmod_nurse, nurse_mod1, nurse_mod2, nurse_mod3, nurse_mod4, nurse_mod5, nurse_mod6, nurse_mod7)
##nullmodel has the lowest AIC



###DESCRIPTIVE STATISTICS####
#How many plots and sites
length(unique(all_result$ID))#97
length(unique(all_result$site_ID))#29

#how many dominant bare pairs
nrow(all_result)

#How many species in total?
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for (k in 1:length(countrynames)) {
  country <- get(countrynames[k])
  
  if(k == 1){
    target_taxa <- country |> 
      select(Species.within.quadrat) |> 
      distinct(Species.within.quadrat) |> 
      filter(!is.na(Species.within.quadrat))
    
    nurse_taxa <- country |> 
      filter(Microsite == 2) |> 
      select(ID_Microsite) |> 
      distinct(ID_Microsite)
  } else {
    temp_target_taxa <- country |> 
      select(Species.within.quadrat) |> 
      distinct(Species.within.quadrat)
    
    temp_nurse_taxa <- country |> 
      filter(Microsite == 2) |> 
      select(ID_Microsite) |> 
      distinct(ID_Microsite)
    
    target_taxa <- target_taxa |> 
      bind_rows(temp_target_taxa) |> 
      distinct(Species.within.quadrat)
    
    nurse_taxa <- nurse_taxa |> 
      bind_rows(temp_nurse_taxa) |> 
      distinct(ID_Microsite)
  }
}

#number of target species
nrow(target_taxa) #721
#number of nurse species
nrow(nurse_taxa) #90


#What is the mean NIntc over all plots? 
richdat <- #remove NA
  all_result[-which(is.na(all_result$NIntc_richness)) , which(colnames(all_result) == "NIntc_richness")]
avg_NIntc_rich <- mean(richdat) #0.1293802
SE_NIntc_rich <- sd(richdat)/sqrt(length((richdat))) #std error
t.test(richdat) #one sample t test to test if sign different from 0


covdat <- all_result[-which(is.na(all_result$NIntc_cover)) , which(colnames(all_result) == "NIntc_cover")]
avg_NIntc_cov <- mean(covdat) #0.1480586
SE_NIntc_cov <- sd(covdat)/sqrt(length((covdat))) #std error
t.test(covdat)


shandat <- all_result[-which(is.na(all_result$NIntc_shannon)) , which(colnames(all_result) == "NIntc_shannon")]
avg_NIntc_shan <- mean(shandat) #mean of NIntc cover wit NA removed
#0.1192385
t.test(shandat)

#What is the mean NInta over all plots?
ad_richdat <- #remove NA
  all_result[-which(is.na(all_result$NInta_richness)) , which(colnames(all_result) == "NInta_richness")]
avg_NInta_rich <- mean(ad_richdat) #0.3843493
SE_NInta_rich <- sd(ad_richdat)/sqrt(length((ad_richdat))) #std error #0.01493847
t.test(ad_richdat) #one sample t test to test if sign different from 0


ad_covdat <- all_result[-which(is.na(all_result$NInta_cover)) , which(colnames(all_result) == "NInta_cover")]
avg_NInta_cov <- mean(ad_covdat) #0.4940919
SE_NInta_cov <- sd(ad_covdat)/sqrt(length((ad_covdat))) #0.01798452
t.test(ad_covdat)


