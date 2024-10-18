###Models and other descriptive statistics regarding NIntc across grazing and gradients of MAT, Aridity, RASE, ph and sand content
library(glmmTMB)
library(DHARMa)
library(car)
library(lsmeans)
#library(multcomp)
#library(multcompView)
library(MuMIn)
library(dplyr)
library(tidyverse)
library(tidylog)
library(corrplot)

##Import results of NIntc calculations (from interaction-gradient analysis scripts)
all_result <- read.csv("Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv", row.names = 1)
all_result$site_ID <- as.factor(all_result$site_ID)
all_result$ID <- as.factor(all_result$ID)
##Treat grazing as an unordered factor!
all_result$graz <- as.factor(all_result$graz)

#import siteinfo, we will use this to add ID to drypop
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") |> 
  mutate(plotref = str_c(SITE, PLOT, sep = "_")) |> 
  select(ID, plotref) |> 
  distinct() |> 
  na.omit()

#import drypop, so which contains the env covariates
drypop <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis clone\\Functional trait data\\Raw data\\drypop_20MAy.csv") |> 
  mutate(plotref = str_c(Site, Plot, sep = "_")) |> #create a variable to identify each plot
  select(plotref, AMT, RAI, RASE, pH.b, SAC.b) |> 
  distinct() |> 
  left_join(siteinfo, by = "plotref") |> 
  select(!plotref)
drypop$ID <- as.factor(drypop$ID)

#join the env covariates to the facilitation data
all_result <- all_result |> 
  inner_join(drypop, by = "ID") |> 
  rename(pH = "pH.b", SAC = "SAC.b") |> 
  mutate(aridity2 = aridity^2, 
         AMT2 = AMT^2)

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

#make sure variables are correctly classified
all_result$site_ID <- as.factor(all_result$site_ID)
all_result$ID <- as.factor(all_result$ID)
##Treat grazing as an unordered factor!
all_result$graz <- as.factor(all_result$graz)


###Correlations####
#env variables
cordata <- all_result |> 
  select(aridity, AMT, RAI, RASE, pH, SAC) |> 
  na.omit()
cormat <- cor(cordata, method = "pearson")

setwd("Figures")
png("environmental_variables_correlation.png")
corrplot(cormat, method = "number", type = "lower")
dev.off()

#only RAI and aridity is strongly correlated
#in the models we will use graz+aridity+AMT+RASE+ph.b+SAc.b

#Interaction indexes
cordata2 <- all_result |> 
  select(contains("NInt")) |>
  select(!contains("binom")) |> 
  na.omit()
cormat2 <- cor(cordata2, method = "pearson")
cortest <- cor.mtest(cordata2) #test for significance, all correlations are significant

setwd("Figures")
png("nint_correlation.png")
corrplot(cormat2, p.mat = cortest$p, sig.level = 0.05, method = 'circle', type = 'lower', insig = "blank",
         addCoef.col ="white", number.cex = 1, order = 'alphabet',diag = FALSE, tl.col = 'black', tl.srt = 45)
dev.off()




###Get the model formulas####
predictors <- c("graz", "aridity", "aridity2", "AMT", "AMT2", "RASE", "pH", "SAC", 
                "graz:aridity", "graz:RASE", "graz:AMT", "graz:pH", "graz:SAC", "RASE:AMT", "RASE:aridity", "AMT:aridity")

#how many combinations are possible?
n_possible_models = 2^length(predictors) -1

modlist <- data.frame(formula = character(length = n_possible_models))
l = 1
for(counter1 in 1:length(predictors)) {
  combos <- as.matrix(combn(predictors, counter1))
  
  for(counter2 in 1:ncol(combos)) {
    mod <- paste(c(combos[, counter2]), collapse = "+")
    
    modlist[l, 1] <- mod
    l = l+1
}}

# Function to check if a model is valid
is_valid_model <- function(model) {
  terms <- unlist(strsplit(model, "\\+"))
  
  # Define main effects and their corresponding interaction/squared terms
  interactions <- list("graz" = c("graz:aridity", "graz:RASE", "graz:AMT", "graz:pH", "graz:SAC"),
                       "aridity" = c("graz:aridity", "RASE:aridity", "AMT:aridity"),
                       "RASE" = c("graz:RASE", "RASE:AMT", "RASE:aridity"),
                       "AMT" = c("graz:AMT", "RASE:AMT", "AMT:aridity"),
                       "pH" = "graz:pH",
                       "SAC" = "graz:SAC")
  
  squared_terms <- list("aridity" = "aridity2", "AMT" = "AMT2")
  
  # Check for interaction terms without main effects
  for (main_effect in names(interactions)) {
    if (any(interactions[[main_effect]] %in% terms) && !(main_effect %in% terms)) {
      return(FALSE)
    }
  }
  
  # Check for squared terms without main effects
  for (main_effect in names(squared_terms)) {
    if ((squared_terms[[main_effect]] %in% terms) && !(main_effect %in% terms)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#run modlist through the function to see which models are valid
validity = c()
for(m in 1:nrow(modlist)) {
  validity[m] <- is_valid_model(modlist[m, 1])
}
#subset modlist to keep only valid models
valid_modlist <- data.frame(predictors = modlist[c(which(validity == TRUE)), ])
write.csv(valid_modlist, "Facilitation data\\results\\nint_clim_soil_model_formulas_22Jun2024.csv")


###Generalised linear modelling with glmmTMB : NINt ~ AMT + RASE + aridity + GRAZ####
formula_table <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\nint_clim_soil_model_formulas_22Jun2024.csv", row.names = 1) |>
  mutate(predictors = paste(predictors, "(1|site_ID/ID)", sep = "+")) |> #add the random effect to all formulas
  add_row(predictors = "1+(1|site_ID/ID)")  #add the null model

#Initialise output file for results
output_file <- "Facilitation data\\results\\nint_clim_soil_nestedRE_model_results_13Aug2024.csv"

# Initialize the output file
write.csv(data.frame(Response = character(), Model = character(), AIC = numeric(), BIC = numeric(), 
                     Warnings = character()), output_file, row.names = FALSE)

# Initialize warning_msg outside the loop
warning_msg <- ""

##Also loop through response variables
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
    
    # Initialize AIC_model outside the tryCatch block
    AIC_model <- NULL
    BIC_model <- NULL
    
    tryCatch( #tryCatch looks for errors and warinngs in the expression
      expr = {
        model <- glmmTMB(formula, family = binomial, data = data)
        
        # Get AIC
        AIC_model <- AIC(model)
        BIC_model <- BIC(model)
        
        warning_messages <- warnings()
        
        ##Do nothing if the warinng is about non integer successes
        # Check for the non-integer #successes warning
        #if ("non-integer #successes" %in% warning_messages) {
        # Handle non-integer #successes warning (e.g., print a message)
        # message("Ignoring non-integer #successes warning")
        #}
        
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
                             AIC = ifelse(!is.null(AIC_model), AIC_model, NA),
                             BIC = ifelse(!is.null(BIC_model), BIC_model, NA),
                             Warnings = warning_msg)
    
    
    write.table(result_row, output_file, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE) #append the new model to the existing file
  }
}

#start 17:25 on 13 Aug
#end 23:30 on 13 Aug

#find the model with the lowest AIC in post hoc tests script


###SPECIES PREFERENCE ANALYSIS####
#Does aridity influence how many species grow exclusively in bare, open and both microsites (Pbare and Pdominant analysis)
#We require raw country data
data_files <- list.files("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\Countriesv3")
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
sp_preference <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\plotlevels_sp_preference_6Feb2024.csv", row.names = 1)
sp_preference$ID <- as.factor(sp_preference$ID)
sp_preference$site_ID <- as.factor(sp_preference$site_ID)
sp_preference$graz <- as.factor(sp_preference$graz)

#import siteinfo, we will use this to add ID to drypop
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") |> 
  mutate(plotref = str_c(SITE, PLOT, sep = "_")) |> 
  select(ID, plotref) |> 
  distinct() |> 
  na.omit()

#import drypop, so which contains the env covariates
drypop <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis clone\\Functional trait data\\Raw data\\drypop_20MAy.csv") |> 
  mutate(plotref = str_c(Site, Plot, sep = "_")) |> #create a variable to identify each plot
  select(plotref, AMT, RAI, RASE, pH.b, SAC.b) |> 
  distinct() |> 
  left_join(siteinfo, by = "plotref") |> 
  select(!plotref)
drypop$ID <- as.factor(drypop$ID)

#join the env covariates to the sp preference data
sp_preference <- sp_preference |> 
  inner_join(drypop, by = "ID") |> 
  rename(pH = "pH.b", SAC = "SAC.b") |> 
  mutate(AMT2 = AMT^2) |>
  mutate(aridity2 = aridity^2)

#In how many of the plots did the majority of sp prefer nurse microsites?
length(sp_preference[which(sp_preference$prop_nurse_only > sp_preference$prop_both) , ]) #15

#In how many of the sites did the majority of sp prefer open microsites?
length(sp_preference[which(sp_preference$prop_bare_only > sp_preference$prop_both) , ]) #15

#On average, how many species ocurred in both nurse and bare microsites
avg_both <- sum(sp_preference$prop_both)/nrow(sp_preference) #0.57


###Generalised linear modelling with glmmTMB: P ~ graz + AMT+ aridity + rase + pH +SAC####
##in text the proportion of nurse only species = Pdominant

#import model formulas
formula_table <- read.csv("Facilitation data\\results\\nint_clim_soil_model_formulas_22Jun2024.csv", row.names = 1) |>
  mutate(predictors = paste(predictors, "(1|site_ID)", sep = "+")) |> #add the random effect to all formulas
  add_row(predictors = "1+(1|site_ID)")  #add the null model

#Create a table for results
results_table <- data.frame(Response = character(), Model = character(), AIC = numeric(), BIC = numeric(), 
                            Warnings = character(), row.names = NULL)

# Initialize warning_msg outside the loop
warning_msg <- ""

##Also loop through response variables
response_list <- c("prop_nurse_only", "prop_bare_only")
datalist = c("sp_preference", "sp_preference")

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
    
    # Initialize AIC_model outside the tryCatch block
    AIC_model <- NULL
    BIC_model <- NULL
    
    tryCatch( #tryCatch looks for errors and warinngs in the expression
      expr = {
        model <- glmmTMB(formula, family = binomial, data = data)
        
        # Get AIC
        AIC_model <- AIC(model)
        BIC_model <- BIC(model)
        
        warning_messages <- warnings()
        
        ##Do nothing if the warinng is about non integer successes
        # Check for the non-integer #successes warning
        #if ("non-integer #successes" %in% warning_messages) {
        # Handle non-integer #successes warning (e.g., print a message)
        #message("Ignoring non-integer #successes warning")
        #}
        
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
                             AIC = ifelse(!is.null(AIC_model), AIC_model, NA),
                             BIC = ifelse(!is.null(BIC_model), BIC_model, NA), 
                             Warnings = warning_msg)
    
    results_table <- rbind(results_table, result_row)
  }
}
##if there is no AIC value, the model did not converge
results_table

#save results
write.csv(results_table, "Facilitation data\\results\\sp_preference_clim_soil_model_results_23Jun2024.csv")

#find model with lowest BIC
prefmod_results_table <- read.csv("Facilitation data\\results\\sp_preference_clim_soil_model_results_23Jun2024.csv", row.names = 1)|> 
  group_by(Response) |> 
  filter(!is.na(BIC)) |> 
  filter(BIC == min(BIC))


###CHisq tests of species association with nurse or bare microsites####
###First we need to get the number times a species is present/absent in each microsite
#Import the country_v3 data
data_files <- list.files("Facilitation analysis\\Facilitation data\\Countriesv3")
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
###Import Chisq results####
Chisq_results <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\Chisq_results_6Feb2024.csv", row.names = 1)
Chisq_results$ID <- as.factor(Chisq_results$ID)

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
  summarise(nsp = n()) #243

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


###get the proportion of species in a plot that show a certain association
#import siteinfo, we will use this to add ID to drypop
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") |> 
  mutate(plotref = str_c(SITE, PLOT, sep = "_")) |> 
  select(ID, SITE_ID, plotref, GRAZ, ARIDITY.v3) |> 
  rename(graz = "GRAZ", aridity = "ARIDITY.v3", site_ID = "SITE_ID") |> 
  distinct() |> 
  na.omit()

#import drypop, so which contains the env covariates
drypop <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis clone\\Functional trait data\\Raw data\\drypop_20MAy.csv") |> 
  mutate(plotref = str_c(Site, Plot, sep = "_")) |> #create a variable to identify each plot
  select(plotref, AMT, RAI, RASE, pH.b, SAC.b) |> 
  distinct() |> 
  left_join(siteinfo, by = "plotref") |> 
  select(!plotref)
drypop$ID <- as.factor(drypop$ID)

#calculate proportions and add siteinfo
prop_chisq_reduced <- Chisq_results |> 
  filter(!association == "too_rare") |> #remove sp that are too rare to do chisq test and do not take them into account for proportion calculation
  group_by(ID, association) |> 
  summarize(Count = n()) |> 
  ungroup() |> 
  group_by(ID) |> 
  mutate(Proportion = Count / sum(Count)) |> 
  ungroup() |> 
  mutate(percentage = Proportion*100) |> 
  left_join(drypop, by = "ID") |> 
  rename(pH = "pH.b", SAC = "SAC.b") |> 
  mutate(AMT2 = AMT^2, aridity2 = aridity^2)

#make sure classifications are correct
prop_chisq_reduced$site_ID <- as.factor(prop_chisq_reduced$site_ID)
prop_chisq_reduced$ID <- as.factor(prop_chisq_reduced$ID)
prop_chisq_reduced$graz <- as.factor(prop_chisq_reduced$graz)

#make separate dataframes for the proportion of species associated with bare and nurse microsites
baredat <- prop_chisq_reduced |> 
  filter(association == "bare") |> 
  rename(prop_bare_association = Proportion)

nursedat <- prop_chisq_reduced |> 
  filter(association == "nurse") |> 
  rename(prop_nurse_association = Proportion)


###Generalised linear modelling with glmmTMB: prop_association ~ graz + AMT + RAI####
#import model formulas
formula_table <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\nint_clim_soil_model_formulas_22Jun2024.csv", row.names = 1) |>
  mutate(predictors = paste(predictors, "(1|site_ID/ID)", sep = "+")) |> #add the random effect to all formulas
  add_row(predictors = "1+(1|site_ID/ID)")  #add the null model

#Initialise output file for results
output_file <- "Facilitation data\\results\\association_clim_soil_nestedRE_model_results_13Aug2024.csv"

# Initialize the output file
write.csv(data.frame(Response = character(), Model = character(), AIC = numeric(), BIC = numeric(), 
                     Warnings = character()), output_file, row.names = FALSE)

# Initialize warning_msg outside the loop
warning_msg <- ""

##Also loop through response variables
response_list <- c("prop_bare_association", "prop_nurse_association")
datalist = c("baredat", "nursedat")

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
    
    # Initialize AIC_model outside the tryCatch block
    AIC_model <- NULL
    BIC_model <- NULL
    
    tryCatch( #tryCatch looks for errors and warinngs in the expression
      expr = {
        model <- glmmTMB(formula, family = binomial, data = data)
        
        # Get AIC
        AIC_model <- AIC(model)
        BIC_model <- BIC(model)
        
        warning_messages <- warnings()
        
        ##Do nothing if the warinng is about non integer successes
        # Check for the non-integer #successes warning
        #if ("non-integer #successes" %in% warning_messages) {
        # Handle non-integer #successes warning (e.g., print a message)
        # message("Ignoring non-integer #successes warning")
        #}
        
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
                             AIC = ifelse(!is.null(AIC_model), AIC_model, NA),
                             BIC = ifelse(!is.null(BIC_model), BIC_model, NA),
                             Warnings = warning_msg)
    
    
    write.table(result_row, output_file, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE) #append the new model to the existing file
  }
}

#start:09:06 20 Aug
#end 9:45 Aug

###DESCRIPTIVE STATISTICS####
range(all_result$aridity)
range(all_result$AMT)
range(all_result$RASE)
range(all_result$SAC)
range(all_result$pH)
range(all_result$RAI)

#which country has lowest rainfall
all_result |> filter(RAI == min(all_result$RAI))
#which country has highest rainfall
all_result |> filter(RAI == max(all_result$RAI))

#which country has lowest temp
all_result |> filter(AMT == min(all_result$AMT))
#which country has highest temp
all_result |> filter(AMT == max(all_result$AMT))

#number of dominant-bare pairs
all_result |> 
  filter(!is.na(NIntc_richness)) |> #remove those records that have no nintc richness
  summarise(nreps = n())

#How many plots and sites
length(unique(all_result$ID))#97
length(unique(all_result$site_ID))#29

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