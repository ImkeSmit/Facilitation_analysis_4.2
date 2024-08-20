###Post hoc tests on best subset models###
library(tidyverse)
library(tidylog)
library(glmmTMB)
library(car)
library(MuMIn)
library(emmeans)
library(ggplot2)
library(ggpubr)
library(DHARMa)

###NInt models####
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

##How many reps?
all_result |> 
  filter(!is.na(NIntc_richness)) |> 
  summarise(n = n()) #3789

all_result |> 
  filter(!is.na(NIntc_cover)) |> 
  summarise(n = n()) #3736

###import the modelling result:
#models without the nested RE
#nint_model_results <- read.csv("Facilitation data//results//nint_clim_soil_model_results_22Jun2024.csv", row.names = 1)
#latest model with the nested RE
nint_model_results <- read.csv("Facilitation data//results//nint_clim_soil_nestedRE_model_results_13Aug2024.csv")

#Find the lowest AIC model for each response variable
bestmods <- nint_model_results |> 
  filter(!is.na(AIC))|> #remove models with convergence errors
  group_by(Response) |> 
  filter(AIC == min(AIC))

###Make all of the above models and get p values and R squared
##NINtc richness
nintc_rich_bestmod <- glmmTMB(NIntc_richness_binom ~ graz+ SAC+ graz:SAC+ (1|site_ID/ID), family = binomial, data = all_result)
null_nintc_richmod <- glmmTMB(NIntc_richness_binom ~ 1+(1|site_ID), family = binomial, data = all_result)

summary(nintc_rich_bestmod)
anova(null_nintc_richmod, nintc_rich_bestmod)
emmeans(nintc_rich_bestmod, specs = 'graz')

r.squaredGLMM(nintc_rich_bestmod)
plot(simulateResiduals(nintc_rich_bestmod)) #underdispersed


##NIntc cover
#null model
null_nintc_covmod <- glmmTMB(NIntc_cover_binom ~ 1+(1|site_ID), family = binomial, data = all_result)
#best subset model
best_nintc_covmod <- glmmTMB(NIntc_cover_binom ~ pH+ (1|site_ID/ID), 
                             family = binomial, data = all_result)

test <- glmmTMB(NIntc_cover ~ graz + SAC + graz:SAC+ (1|site_ID/ID), 
         data = all_result)

summary(best_nintc_covmod)
emmeans(best_nintc_covmod, specs = "graz")
anova(null_nintc_covmod, best_nintc_covmod) 
Anova(best_nintc_covmod)
r.squaredGLMM(best_nintc_covmod) #take the theoretical

interaction.plot(x.factor = all_result$SAC, trace.factor = all_result$graz, response = all_result$NIntc_cover_binom)


##Ninta richness
#nullmodel
null_ninta_richmod <- glmmTMB(NIntc_richness_binom ~ 1+(1|site_ID/ID), data = all_result, family = binomial)
#bets model
best_ninta_richmod <- glmmTMB(NInta_richness_binom ~ graz+AMT+SAC+graz:SAC+(1|site_ID/ID), data = all_result, family = binomial)

summary(best_ninta_richmod)
anova(null_ninta_richmod, best_ninta_richmod) #
Anova(best_ninta_richmod)
r.squaredGLMM(best_ninta_richmod) #take the theoretical
plot(simulateResiduals(best_ninta_richmod)) #underdispersed
emmeans(best_ninta_richmod, specs = "graz")

##NInta cover
#null model
null_ninta_covmod <- glmmTMB(NInta_cover_binom ~ 1+(1|site_ID/ID), data = all_result, family = binomial)
#best model
best_ninta_covmod <- glmmTMB(NInta_cover_binom ~ pH+(1|site_ID/ID), 
                            data = all_result, family = binomial)

summary(best_ninta_covmod)
anova(null_ninta_covmod, best_ninta_covmod) 
Anova(best_ninta_covmod)
r.squaredGLMM(best_ninta_covmod) #take the theoretical
plot(simulateResiduals(best_ninta_covmod)) #underdispersed
emmeans(best_ninta_covmod, specs = "graz")

####sp preference models####
##Import sp preference data
sp_preference <- read.csv("Facilitation data\\results\\plotlevels_sp_preference_6Feb2024.csv", row.names = 1) |> 
  rename(aridity2 = arid_sq)
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
  mutate(AMT2 = AMT^2)

##Import the modelling result
#find model with lowest AIC
prefmod_results_table <- read.csv("Facilitation data\\results\\sp_preference_clim_soil_model_results_23Jun2024.csv", row.names = 1)|> 
  group_by(Response) |> 
  filter(!is.na(AIC)) |> 
  filter(AIC == min(AIC)) 

#How many reps?



###Species association models####
#import species associations (from Chi2 tests)
Chisq_results <- read.csv("Facilitation data\\results\\Chisq_results_6Feb2024.csv", row.names = 1)
Chisq_results$ID <- as.factor(Chisq_results$ID)
Chisq_results$site_ID <- as.factor(Chisq_results$site_ID)
Chisq_results$graz <- as.factor(Chisq_results$graz)

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
prop_chisq_reduced$ID <- as.factor(prop_chisq_reduced$ID)
prop_chisq_reduced$site_ID <- as.factor(prop_chisq_reduced$site_ID)
prop_chisq_reduced$graz <- as.factor(prop_chisq_reduced$graz)

#import the modelling results
ass_model_results <- read.csv("Facilitation data\\results\\association_clim_soil_nestedRE_model_results_13Aug2024.csv")

ass_bestmods <- ass_model_results |> 
  filter(!is.na(AIC))|> #remove models with convergence errors
  group_by(Response) |> 
  filter(AIC == min(AIC))

#how many reps?
#prop_bare:
prop_chisq_reduced |> 
  filter(association == "bare") |> 
  summarise(n = n()) #37

#prop_nurse:
prop_chisq_reduced |> 
  filter(association == "nurse") |> 
  summarise(n = n()) #47
