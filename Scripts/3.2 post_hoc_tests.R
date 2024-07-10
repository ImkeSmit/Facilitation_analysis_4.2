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

###import the modelling result:
nint_model_results <- read.csv("Facilitation data//results//nint_clim_soil_model_results_22Jun2024.csv", row.names = 1)

#Find the lowest AIC model for each response variable
bestmods <- nint_model_results |> 
  filter(!is.na(AIC))|> #remove models with convergence errors
  group_by(Response) |> 
  filter(AIC == min(AIC))

###Make all of the above models and get p values and R squared
##NINtc richness
nintc_rich_bestmod <- glmmTMB(NIntc_richness_binom ~ graz+SAC+graz:SAC +(1|site_ID), family = binomial, data = all_result)
null_nintc_richmod <- glmmTMB(NIntc_richness_binom ~ 1+(1|site_ID), family = binomial, data = all_result)

summary(nintc_rich_bestmod)
anova(null_nintc_richmod, nintc_rich_bestmod)
emmeans(nintc_rich_bestmod, specs = 'graz')

interaction.plot(x.factor = all_result$SAC, trace.factor = all_result$graz, response = all_result$NIntc_richness_binom)

r.squaredGLMM(nintc_rich_bestmod)
plot(simulateResiduals(nintc_rich_bestmod)) #underdispersed


##NIntc cover
#null model
null_nintc_covmod <- glmmTMB(NIntc_cover_binom ~ 1+(1|site_ID), family = binomial, data = all_result)
#best subset model
best_nintc_covmod <- glmmTMB(NIntc_cover_binom ~ graz+pH+SAC+graz:SAC+(1|site_ID), 
                             family = binomial, data = all_result)
summary(best_nintc_covmod)
emmeans(best_nintc_covmod, specs = "graz")
anova(null_nintc_covmod, best_nintc_covmod) 
Anova(best_nintc_covmod)
r.squaredGLMM(best_nintc_covmod) #take the theoretical

interaction.plot(x.factor = all_result$SAC, trace.factor = all_result$graz, response = all_result$NIntc_cover_binom)

##Some basic plots###
#get model predictions of nintc richness over SAC for different graz
pred_data <- all_result |>
  select(ID, SAC, pH, graz, site_ID) |> 
  distinct() |> 
  mutate(pH = mean(pH))#get the mean pH so that we can keep it constant in the nintc cover prediction

##NIntc richness##
graphmod <- glmmTMB(NIntc_richness_binom ~ graz+SAC+graz:SAC, family = binomial, data = all_result)#remove random effect because otherwise it makes jagged lines

pred_data$NIntc_richness_binom_prediction <- predict(graphmod, newdata = pred_data, type = "response") #get nintc_binom predictions
pred_data$NIntc_richness_prediction <- pred_data$NIntc_richness_binom_prediction*2 -1

nintc_richness_sac <- ggplot(all_result, aes(y = NIntc_richness, x = SAC)) +
  geom_jitter(height = 0.01, width = 2, color = "darkslategrey", alpha = 0.6, size = 1) +
  geom_line(data = pred_data, aes(x = SAC, y = NIntc_richness_prediction, color = graz), lwd = 1) +
  scale_color_manual(labels = c("ungrazed", "low", "medium", "high"),
                     values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" ))+
  labs(color = "Grazing pressure", y = expression(NInt[C]~richness), x = "Sand content (%)") +
  theme_classic() 

##Nintc cover##
graphmod2 <- glmmTMB(NIntc_cover_binom ~ graz+pH+SAC+graz:SAC, 
                     family = binomial, data = all_result)#remove random effect because otherwise it makes jagged lines

pred_data$NIntc_cover_binom_prediction <- predict(graphmod2, newdata = pred_data, type = "response", se.fit = T)$fit #get nintc_binom predictions
pred_data$NIntc_cover_prediction <- pred_data$NIntc_cover_binom_prediction*2 -1

pred_data$NIntc_cover_binom_se <-predict(graphmod2, newdata = pred_data, type = "response", se.fit = T)$se.fit
pred_data$NIntc_cover_se <- pred_data$NIntc_cover_binom_se*2 -1

nintc_cover_sac <- ggplot(all_result, aes(y = NIntc_cover, x = SAC)) +
  geom_jitter(height = 0.01, width = 2, color = "darkslategrey", alpha = 0.6, size = 1) +
  geom_line(data = pred_data, aes(x = SAC, y = NIntc_cover_prediction, color = graz), lwd = 1) +
  scale_color_manual(labels = c("ungrazed", "low", "medium", "high"),
                     values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" ))+
  labs(color = "Grazing pressure", y = expression(NInt[C]~cover), x = "Sand content (%)") +
  theme_classic() 

nint_sac_combo <- ggarrange(nintc_richness_sac, nintc_cover_sac, ncol = 2, nrow = 1, common.legend = T, 
                            legend = "bottom", labels = c("a", "b"))
ggsave("nint_sac_scatter.png", nint_sac_combo, path = "Figures", height = 700, width = 1250, units = "px")

##Nintc cover over pH##
pred_data2 <- all_result |>
  select(ID, SAC, pH, graz, site_ID) |> 
  distinct() |> 
  mutate(SAC = mean(SAC))

graphmod2 <- glmmTMB(NIntc_cover_binom ~ graz+pH+SAC+graz:SAC, 
                     family = binomial, data = all_result)#remove random effect because otherwise it makes jagged lines

pred_data2$NIntc_cover_binom_prediction <- predict(graphmod2, newdata = pred_data2, type = "response", se.fit = T)$fit #get nintc_binom predictions
pred_data2$NIntc_cover_prediction <- pred_data2$NIntc_cover_binom_prediction*2 -1

nintc_cover_ph <- ggplot(all_result, aes(y = NIntc_cover, x = pH)) +
  geom_jitter(height = 0.01, width = 0.1, color = "darkslategrey", alpha = 0.6, size = 1.5) +
  geom_line(data = pred_data2, aes(x = pH, y = NIntc_cover_prediction, color = graz), lwd = 1) +
  scale_color_manual(labels = c("ungrazed", "low", "medium", "high"),
                     values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" ))+
  labs(color = "Grazing pressure", y = expression(NInt[C]~cover), x = "pH") +
  theme_classic() +
  theme(legend.position = "right")
ggsave("nint_ph_scatter.png", nintc_cover_ph, path = "Figures", height = 700, width = 1400, units = 'px')


##Ninta richness
#nullmodel
null_ninta_richmod <- glmmTMB(NIntc_richness_binom ~ 1+(1|site_ID), data = all_result, family = binomial)
#bets model
best_ninta_richmod <- glmmTMB(NInta_richness_binom ~ graz+AMT+SAC+graz:SAC +(1|site_ID), data = all_result, family = binomial)

summary(best_ninta_richmod)
anova(null_ninta_richmod, best_ninta_richmod) #
Anova(best_ninta_richmod)
r.squaredGLMM(best_ninta_richmod) #take the theoretical
plot(simulateResiduals(best_ninta_richmod)) #underdispersed


##NInta cover
#null model
null_ninta_covmod <- glmmTMB(NInta_cover_binom ~ 1+(1|site_ID), data = all_result, family = binomial)
#best model
best_ninta_covmod <- glmmTMB(NInta_cover_binom ~ graz+aridity+AMT+SAC+graz:SAC+(1|site_ID), 
                            data = all_result, family = binomial)

summary(best_ninta_covmod)
anova(null_ninta_covmod, best_ninta_covmod) 
Anova(best_ninta_covmod)
r.squaredGLMM(best_ninta_covmod) #take the theoretical
plot(simulateResiduals(best_ninta_covmod)) #underdispersed


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
ass_model_results <- read.csv("Facilitation data\\results\\association_clim_soil_model_results_23Jun2024.csv", row.names = 1)

ass_bestmods <- ass_model_results |> 
  filter(!is.na(AIC))|> #remove models with convergence errors
  group_by(Response) |> 
  filter(AIC == min(AIC))