###Post hoc tests on best subset models###
library(tidyverse)
library(tidylog)
library(glmmTMB)
library(car)
library(MuMIn)


###NInt models####
#import nint results
all_result <- read.csv("Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv", row.names = 1)
all_result$site_ID <- as.factor(all_result$site_ID)
all_result$ID <- as.factor(all_result$ID)
##Treat grazing as an unordered factor!
all_result$graz <- as.factor(all_result$graz)

#import siteinfo, so that we can add RAI and AMT
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") |> 
  select(ID, RAI, AMT) |> 
  mutate(RAI2 = RAI^2, 
         AMT2 = AMT^2)
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

###import the modelling result:
nint_model_results <- read.csv("Facilitation data//results//nint_model_results_11Apr2024.csv", row.names = 1)

#Find the lowest AIC model for each response variable
bestmods <- nint_model_results |> 
  filter(!is.na(AIC))|> #remove models with convergence errors
  group_by(Response) |> 
  filter(AIC == min(AIC))

###Make all of the above models and get p values and R squared
##NINtc richness
null_nintc_richmod <- glmmTMB(NIntc_richness_binom ~ 1+(1|site_ID), family = binomial, data = all_result)
summary(null_nintc_richmod)


##NIntc cover
#null model
null_nintc_covmod <- glmmTMB(NIntc_cover_binom ~ 1+(1|site_ID), family = binomial, data = all_result)
#best subset model
best_nintc_covmod <- glmmTMB(NIntc_cover_binom ~ graz+AMT+RAI+AMT2+AMT:RAI+RAI:AMT2+(1|site_ID), 
                             family = binomial, data = all_result)
summary(best_nintc_covmod)
anova(null_nintc_covmod, best_nintc_covmod) #p = 0.01212, full model is better than null model
Anova(best_nintc_covmod)
r.squaredGLMM(best_nintc_covmod) #take the theoretical

##Ninta richness
#nullmodel
null_ninta_richmod <- glmmTMB(NIntc_richness_binom ~ 1+(1|site_ID), data = all_result, family = binomial)
#bets model
best_ninta_richmod <- glmmTMB(NInta_richness_binom ~ AMT+(1|site_ID), data = all_result, family = binomial)

summary(best_ninta_richmod)
anova(null_ninta_richmod, best_ninta_richmod) #p = 0.00635
Anova(best_ninta_richmod)
r.squaredGLMM(best_ninta_richmod) #take the theoretical


##NInta cover
#null model
null_ninta_covmod <- glmmTMB(NInta_cover_binom ~ 1+(1|site_ID), data = all_result, family = binomial)
#best model
best_ninta_covmod <- glmmTMB(NInta_cover_binom ~ graz+AMT+RAI+AMT2+AMT:RAI+RAI:AMT2+(1|site_ID), 
                            data = all_result, family = binomial)

summary(best_ninta_covmod)
anova(null_ninta_covmod, best_ninta_covmod) #p = 0.00202
Anova(best_ninta_covmod)
r.squaredGLMM(best_ninta_covmod) #take the theoretical


####sp preference models####
##Import sp preference data
sp_preference <- read.csv("Facilitation data\\results\\plotlevels_sp_preference_6Feb2024.csv", row.names = 1)
sp_preference$ID <- as.factor(sp_preference$ID)
sp_preference$site_ID <- as.factor(sp_preference$site_ID)
sp_preference$graz <- as.factor(sp_preference$graz)

#import siteinfo
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") |> 
  select(ID, RAI, AMT) |> 
  mutate(RAI2 = RAI^2, 
         AMT2 = AMT^2)
siteinfo$ID <- as.factor(siteinfo$ID)
#join to all_result
sp_preference <- sp_preference |> 
  left_join(siteinfo, by = "ID")

##Import the modelling result
pref_model_results <- read.csv("Facilitation data\\results\\sp_preference_model_results_11Apr2024.csv", row.names = 1)

#Find the lowest AIC model for each response variable
pref_bestmods <- pref_model_results |> 
  filter(!is.na(AIC))|> #remove models with convergence errors
  group_by(Response) |> 
  filter(AIC == min(AIC))
#Null models selected for both response variables
#!!!80% of the models have convergenec errors. Is this a problem?


###Species association models####
#import species associations (from Chi2 tests)
Chisq_results <- read.csv("Facilitation data\\results\\Chisq_results_6Feb2024.csv", row.names = 1)
Chisq_results$ID <- as.factor(Chisq_results$ID)
Chisq_results$site_ID <- as.factor(Chisq_results$site_ID)
Chisq_results$graz <- as.factor(Chisq_results$graz)

#import s#import siteinfo
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") |> 
  select(ID, RAI, AMT) |> 
  mutate(RAI2 = RAI^2, 
         AMT2 = AMT^2)
siteinfo$ID <- as.factor(siteinfo$ID)

chisq
