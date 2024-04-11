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


