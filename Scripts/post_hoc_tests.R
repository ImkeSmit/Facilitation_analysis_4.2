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

emmeans(best_nintc_covmod, specs = "graz")

#is it a problem to model with the binom variable and graph with the untransformed variable?

##Some basic plots
###NIntc cover ~ graz
covdat <- all_result[-which(is.na(all_result$NIntc_cover)) , ]
#does graz affect NINtc cover
covdat$NIntc_cover_binom <- (covdat$NIntc_cover + 1)/2
cov_mod1 <- glmmTMB(NIntc_cover_binom ~ graz +(1|site_ID),  
                    family = binomial, data = covdat)
summary(cov_mod1)
lsmeans(cov_mod1, specs = "graz")
lsmeans <- c(0.376,0.146,0.356,0.427)#these are the means of each grazlevel predicted by the model. 
SE <- c(0.146, 0.127, 0.126, 0.130) #these are the SE of the mean estimates
cld(glht(model = cov_mod1, mcp(graz = "Tukey"))) ##these are the significance letters showing significance between the predicted means
lsmeans_letters <- c("ab", "a", "ab", "b")

cov_grazlevel_lsmeans <- data.frame(lsmean = lsmeans, std_error = SE, 
                                    graz = c(0,1,2,3), sign_letters = lsmeans_letters, ycoord = c(0.6, 0.6, 0.6, 0.6))
cov_grazlevel_lsmeans$ymin <- cov_grazlevel_lsmeans$lsmean - cov_grazlevel_lsmeans$std_error
cov_grazlevel_lsmeans$ymax <- cov_grazlevel_lsmeans$lsmean + cov_grazlevel_lsmeans$std_error
cov_grazlevel_lsmeans$graz <- as.factor(cov_grazlevel_lsmeans$graz)

lsmean_cov_graz_bar <- ggplot() +
  geom_bar(data = cov_grazlevel_lsmeans, aes(x = graz, y = lsmean, fill = graz), stat = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  geom_errorbar(data = cov_grazlevel_lsmeans, aes(x = graz, ymin  = ymin, ymax = ymax), colour="black", width = 0.5)+
  ylim(0, 0.62) +
  geom_text(data = cov_grazlevel_lsmeans, aes(x = graz, y = ycoord), label = c(cov_grazlevel_lsmeans$sign_letters))+
  xlab("Grazing pressure") +
  ylab(expression(Estimated~mean~NInt[C]~cover))+
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  theme_classic()+
  theme(legend.position = "none")
lsmean_cov_graz_bar


###Arithmetic means
###get the mean and se of NINtc at each grazing level. Use covdat because NA has been removed
arith_means <- data.frame(tapply(covdat$NIntc_cover, covdat$graz, FUN = "mean"))
colnames(arith_means) <- "mean_NIntc_cover"
arith_means$graz = rownames(arith_means)
rownames(arith_means) <- c(1:nrow(arith_means))

n_replicates <- data.frame(tapply(covdat$NIntc_cover, covdat$graz, FUN = "length"))
colnames(n_replicates) <- "n_replicates"
n_replicates$graz = rownames(n_replicates)
rownames(n_replicates) <- c(1:nrow(n_replicates))

sd <- data.frame(tapply(covdat$NIntc_cover, covdat$graz, FUN = "sd"))
colnames(sd) <- "std_dev"
sd$graz = rownames(sd)
rownames(sd) <- c(1:nrow(sd))

se <- data.frame(std_error = sd$std_dev/sqrt(n_replicates$n_replicates))

cov_grazlevel_stats <- cbind(arith_means, se)
#get mean - std error
cov_grazlevel_stats$ymin <- cov_grazlevel_stats$mean_NIntc_cover - cov_grazlevel_stats$std_error
#get mean + std error
cov_grazlevel_stats$ymax <- cov_grazlevel_stats$mean_NIntc_cover + cov_grazlevel_stats$std_error

##Now we need to do one sample t tests to see if the means are significantly differnt from 0
#graz0
t.test(covdat[which(covdat$graz == 0) , ]$NIntc_cover)
#p-value = 0.004373

#graz1
t.test(covdat[which(covdat$graz == 1) , ]$NIntc_cover)
# p-value = 1.474e-07

#graz2
t.test(covdat[which(covdat$graz == 2) , ]$NIntc_cover)
#p-value < 2.2e-16

#graz3
t.test(covdat[which(covdat$graz == 3) , ]$NIntc_cover)
#p-value < 2.2e-16
##SO ALL are differnt from 0


#create a dataframe so that we can add asterisks to the plot
cov_aster <- data.frame(graz = c("0", "1", "2", "3"), ycoord = c(0.12, 0.14, 0.19, 0.23))

arith_cov_graz_bar <- ggplot() +
  geom_bar(data = cov_grazlevel_stats, aes(x = graz, y = mean_NIntc_cover, fill = graz), stat = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  geom_errorbar(data = cov_grazlevel_stats, aes(x = graz, ymin  = ymin, ymax = ymax), colour="black", width = 0.5)+
  ylim(0, 0.62) +
  geom_text(data = cov_aster, aes(x = graz, y = ycoord), label = "*", size = 6) +
  xlab("Grazing pressure") +
  ylab(expression(Arithmetic~mean~NInt[C]~cover))+
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  theme_classic()+
  theme(legend.position = "none")
arith_cov_graz_bar

##arrange the arithmetic and lsmean plots on the same pane
cov_grazlevel_arith_and_lsmeans <- ggarrange(lsmean_cov_graz_bar, arith_cov_graz_bar, ncol = 2, nrow = 1, labels = c("a", "b"))


###NIntc cover ~ AMT
#get model predictions
AMT_mod <- glmmTMB(NIntc_cover_binom ~ AMT, data = all_result, family = binomial)
#values to predict over
pred_df <- data.frame(AMT = c(unique(all_result$AMT)))
#get predicted vals and add them to dataframe
pred_df$AMT_mod_predictions <- c(predict(AMT_mod, pred_df))


nintc_cov_AMT <- ggplot(all_result, aes(x = AMT, y = NIntc_cover)) +
  geom_jitter(shape = 21, size = 2, fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 1, height = 0.1) +
  ylab("NIntc cover") +
  geom_line(data = pred_df, aes(x = AMT, y = AMT_mod_predictions), color = "red", lwd = 1) +
  theme_classic() 

##NINtc cover ~ RAI
#get model predictions
RAI_mod <- glmmTMB(NIntc_cover_binom ~ RAI, data = all_result, family = binomial)
#values to predict over
RAI_pred_df <- data.frame(RAI = c(unique(all_result$RAI)))
#get predicted vals and add them to dataframe
RAI_pred_df$RAI_mod_predictions <- c(predict(RAI_mod, RAI_pred_df))


nintc_cov_RAI <- ggplot(all_result, aes(x = RAI, y = NIntc_cover)) +
  geom_jitter(shape = 21, size = 2, fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 10, height = 0.1) +
  ylab("NIntc cover") +
  geom_line(data = RAI_pred_df, aes(x = RAI, y = RAI_mod_predictions), color = "blue", lwd = 1) +
  theme_classic() 



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
  left_join(siteinfo, by = "plotref") |> 
  select(!plotref)
drypop$ID <- as.factor(drypop$ID)

#join the env covariates to the sp preference data
sp_preference <- sp_preference |> 
  inner_join(drypop, by = "ID") |> 
  rename(pH = "pH.b", SAC = "SAC.b") |> 
  mutate(AMT2 = AMT^2)

##Import the modelling result
#find model with lowest BIC
prefmod_results_table <- read.csv("Facilitation data\\results\\sp_preference_clim_soil_model_results_23Jun2024.csv", row.names = 1)|> 
  group_by(Response) |> 
  filter(!is.na(BIC)) |> 
  filter(BIC == min(BIC)) 
#null model selected for prop_bare_only
#graz selected for prop_nurse_only

nurse_only_bestmod <- glmmTMB(prop_nurse_only ~ graz+(1|site_ID), data = sp_preference, family = binomial)
nurse_only_nullmod <- glmmTMB(prop_nurse_only ~ 1+(1|site_ID), data = sp_preference, family = binomial)
summary(nurse_only_bestmod)
anova(nurse_only_nullmod, nurse_only_bestmod)
emmeans(nurse_only_bestmod, specs = "graz")
r.squaredGLMM(nurse_only_bestmod)
plot(simulateResiduals(nurse_only_bestmod)) #underdispersed, HOV violated


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
  filter(!is.na(BIC))|> #remove models with convergence errors
  group_by(Response) |> 
  filter(BIC == min(BIC))
#null model selected for bare ass
#pH selected for nurse ass


##best model for prop_nurse_ass###
#subset for the species that are bare associated
nursedat <- prop_chisq_reduced |> 
  filter(association == "nurse") |> 
  rename(prop_nurse_association = Proportion)

nurse_ass_bestmod <- glmmTMB(prop_nurse_association ~ pH +(1|site_ID), family = binomial, data = nursedat)

nurse_ass_nullmod <- glmmTMB(prop_nurse_association ~ 1+(1|site_ID), family = binomial, data = nursedat)

summary(nurse_ass_bestmod)
anova(nurse_ass_nullmod, nurse_ass_bestmod)
r.squaredGLMM(nurse_ass_bestmod)

plot(simulateResiduals(nurse_ass_bestmod))#underdispersed...

