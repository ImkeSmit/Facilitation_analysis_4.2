###Models and other descriptive statistics regarding NIntc accross grazing and aridity gradients

library(glmmTMB)
library(car)
#library(glmmADMB)
library(lsmeans)
library(multcomp)
library(multcompView)
library(MuMIn)
library(dplyr)

##Import results of NIntc calculations (from interaction-gradient analysis scripts)
all_result <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\NIntc_results_allcountries_26Sep.csv", row.names = 1)
all_result$site_ID <- as.factor(all_result$site_ID)
all_result$ID <- as.factor(all_result$ID)
##Treat grazing as an unordered factor!
all_result$graz <- as.factor(all_result$graz)

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

#Square aridity to look fro quadratic relationships
all_result$arid_sq <- all_result$aridity^2


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
#we will use a model buidling approach where we sequentially add variables
###NIntc_richness####
dat <- all_result[-which(is.na(all_result$NIntc_richness_binom)) , ] #remove rows with NA

#NULL model 
nullmod_rich <- glmmTMB(NIntc_richness_binom ~ 1 +(1|site_ID),  
                        family = binomial, data = dat)

##graz
rich_mod1 <- glmmTMB(NIntc_richness_binom ~ graz +(1|site_ID),  
                     family = binomial, data = dat)
summary(rich_mod1)
Anova(rich_mod1)
anova(nullmod_rich, rich_mod1) #p=0.3691
lsmeans(rich_mod1, specs = "graz")
cld(lsmeans(rich_mod1, specs = "graz"), Letters = "abcdefg")

#aridity
rich_mod2 <- glmmTMB(NIntc_richness_binom ~ aridity + (1|site_ID),  
                     family = binomial, data = dat)
summary(rich_mod2)
Anova(rich_mod2)
anova(nullmod_rich, rich_mod2) #p=0.8024

#aridity + arid_sq
rich_mod3 <- glmmTMB(NIntc_richness_binom ~ aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = dat)
summary(rich_mod3)
Anova(rich_mod3)
anova(nullmod_rich, rich_mod3) #p=0.7875

##graz + aridity
rich_mod4 <- glmmTMB(NIntc_richness_binom ~ graz + aridity +(1|site_ID),  
                     family = binomial, data = dat)
summary(rich_mod4)
Anova(rich_mod4)
anova(nullmod_rich, rich_mod4) #0.5228

##graz + aridity + arid_sq
rich_mod5 <- glmmTMB(NIntc_richness_binom ~ graz + aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = dat)
summary(rich_mod5)
Anova(rich_mod5)
anova( nullmod_rich, rich_mod5) # p = 0.599

#graz*aridity
rich_mod6 <- glmmTMB(NIntc_richness_binom ~ graz*aridity + (1|site_ID),  
                     family = binomial, data = dat)
summary(rich_mod6)
Anova(rich_mod6)
anova(nullmod_rich,rich_mod6) # 0.7563

#graz*aridity + graz*arid_sq
rich_mod7 <- glmmTMB(NIntc_richness_binom ~ graz*aridity + graz*arid_sq +(1|site_ID),  
        family = binomial, data = dat)
summary(rich_mod7)
Anova(rich_mod7)
anova(nullmod_rich, rich_mod7) # 0.4029

##Compare the AIC values
AIC(nullmod_rich, rich_mod1, rich_mod2, rich_mod3, rich_mod4, rich_mod5, rich_mod6, rich_mod7)
#null model has the lowest AIC, so the predictors have no significant effect


###Get the model predictions
#create a dataframe with values that the model must predict for
#make site_ID NA (the predict function needs the variable to make predictions)
#graz0:
pred_data_0 <- data.frame(aridity = seq(min(dat[which(dat$graz == 0) , ]$aridity), max(dat[which(dat$graz == 0) , ]$aridity), 0.01), 
                          graz = 0, colour = "darkgreen", site_ID = NA)
pred_data_0$arid_sq <- pred_data_0$aridity^2

#graz1:
pred_data_1 <- data.frame(aridity = seq(min(dat[which(dat$graz == 1) , ]$aridity), max(dat[which(dat$graz == 1) , ]$aridity), 0.01), 
                          graz = 1, colour = "chartreuse2", site_ID = NA)
pred_data_1$arid_sq <- pred_data_1$aridity^2

#graz2:
pred_data_2 <- data.frame(aridity = seq(min(dat[which(dat$graz == 2) , ]$aridity), max(dat[which(dat$graz == 2) , ]$aridity), 0.01), 
                          graz = 2, colour = "darkolivegreen3", site_ID = NA)
pred_data_2$arid_sq <- pred_data_2$aridity^2

#graz3:
pred_data_3 <- data.frame(aridity = seq(min(dat[which(dat$graz == 3) , ]$aridity), max(dat[which(dat$graz == 3) , ]$aridity), 0.01), 
                          graz = 3, colour = "darkgoldenrod4", site_ID = NA)
pred_data_3$arid_sq <- pred_data_3$aridity^2


#rbind all the grazlevels:
pred_data <- rbind(pred_data_0, pred_data_1, pred_data_2, pred_data_3)
pred_data$graz <- as.factor(pred_data$graz)

plot(predict(rich_mod2, newdata = pred_data, type = "response") ~ 
       pred_data$aridity, ylab = "predicted NINtc richness binom", xlab = "Aridity", col = pred_data$colour)

#export so that you can use it in the graphs and figures script
#predictions of mod2
model2_prediction <- data.frame(predicted_NIntc_binom = predict(rich_mod2, newdata = pred_data, type = "response"))
model2_prediction <- cbind(model2_prediction, pred_data)
write.csv(model2_prediction, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\rich_mod2_prediction_tmb_11Sept.csv")

#predictions of mod4
model4_prediction <- data.frame(predicted_NIntc_binom = predict(rich_mod4, newdata = pred_data, type = "response"))
model4_prediction <- cbind(model4_prediction, pred_data)
write.csv(model4_prediction, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\rich_mod4_prediction_tmb_11Sept.csv")


##NIntc cover####
covdat <- all_result[-which(is.na(all_result$NIntc_cover_binom)) , ] #remove rows with NA

nullmod_cov <- glmmTMB(NIntc_cover_binom ~ 1 +(1|site_ID),  
                          family = binomial, data = covdat)

##graz
cov_mod1 <- glmmTMB(NIntc_cover_binom ~ graz +(1|site_ID),  
                     family = binomial, data = covdat)
summary(cov_mod1)
Anova(cov_mod1)
anova(nullmod_cov, cov_mod1) #p = 0.03662
lsmeans(cov_mod1, specs = "graz")
cld(lsmeans(cov_mod1, specs = "graz"), Letters = "abcdefg")



r.squaredGLMM(cov_mod1) #take the theoretical


#aridity
cov_mod2 <- glmmTMB(NIntc_cover_binom ~ aridity + (1|site_ID),  
                     family = binomial, data = covdat)
summary(cov_mod2)
Anova(cov_mod2)
anova(nullmod_cov, cov_mod2) #p = 0.4678

#aridity + arid_sq
cov_mod3 <- glmmTMB(NIntc_cover_binom ~ aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = covdat)
summary(cov_mod3)
Anova(cov_mod3)
anova(nullmod_cov, cov_mod3) #p = 0.4761

##graz + aridity
cov_mod4 <- glmmTMB(NIntc_cover_binom ~ graz + aridity +(1|site_ID),  
                     family = binomial, data = covdat)
summary(cov_mod4)
Anova(cov_mod4)
anova(nullmod_cov, cov_mod4) #p = 0.06037

##graz + aridity + arid_sq
cov_mod5 <- glmmTMB(NIntc_cover_binom ~ graz + aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = covdat)
summary(cov_mod5)
Anova(cov_mod5)
anova(nullmod_cov, cov_mod5) #p = 0.06963

#graz*aridity
cov_mod6 <- glmmTMB(NIntc_cover_binom ~ graz*aridity + (1|site_ID),  
                     family = binomial, data = covdat)
summary(cov_mod6)
Anova(cov_mod6)
anova(nullmod_cov, cov_mod6) #0.2036

#graz*aridity + graz*arid_sq
cov_mod7 <- glmmTMB(NIntc_cover_binom ~ graz*aridity + graz*arid_sq +(1|site_ID),  
                     family = binomial, data = covdat)
summary(cov_mod7)
Anova(cov_mod7)
anova(nullmod_cov, cov_mod7) #0.1968

AIC(nullmod_cov, cov_mod1, cov_mod2, cov_mod3, cov_mod4, cov_mod5, cov_mod6, cov_mod7)
#cov_mod1 has the lowest AIC



##NInta richness####
ad_richdat <- all_result[-which(is.na(all_result$NInta_richness_binom)) , ] #remove rows with NA

#nullmodel
additive_null_richmod <- glmmTMB(NInta_richness_binom ~ 1 +(1|site_ID),  
                        family = binomial, data = ad_richdat)

##graz
ad_rich_mod1 <- glmmTMB(NInta_richness_binom ~ graz +(1|site_ID),  
                    family = binomial, data = ad_richdat)
summary(ad_rich_mod1)
Anova(ad_rich_mod1)
lsmeans(ad_rich_mod1, specs = "graz")
cld(lsmeans(ad_rich_mod1, specs = "graz"), Letters = "abcdefg")

#aridity
ad_rich_mod2 <- glmmTMB(NInta_richness_binom ~ aridity + (1|site_ID),  
                    family = binomial, data = ad_richdat)
summary(ad_rich_mod2)
Anova(ad_rich_mod2)

#aridity + arid_sq
ad_rich_mod3 <- glmmTMB(NInta_richness_binom ~ aridity + arid_sq +(1|site_ID),  
                    family = binomial, data = ad_richdat)
summary(ad_rich_mod3)
Anova(ad_rich_mod3)

##graz + aridity
ad_rich_mod4 <- glmmTMB(NInta_richness_binom ~ graz + aridity +(1|site_ID),  
                    family = binomial, data = ad_richdat)
summary(ad_rich_mod4)
Anova(ad_rich_mod4)

##graz + aridity + arid_sq
ad_rich_mod5 <- glmmTMB(NInta_richness_binom ~ graz + aridity + arid_sq +(1|site_ID),  
                    family = binomial, data = ad_richdat)
summary(ad_rich_mod5)
Anova(ad_rich_mod5)

#graz*aridity
ad_rich_mod6 <- glmmTMB(NInta_richness_binom ~ graz*aridity + (1|site_ID),  
                    family = binomial, data = ad_richdat)
summary(ad_rich_mod6)
Anova(ad_rich_mod6)

#graz*aridity + graz*arid_sq
ad_rich_mod7 <- glmmTMB(NInta_richness_binom ~ graz*aridity + graz*arid_sq +(1|site_ID),  
                    family = binomial, data = ad_richdat)
summary(ad_rich_mod7)
Anova(ad_rich_mod7)

AIC(additive_null_richmod, ad_rich_mod1, ad_rich_mod2, ad_rich_mod3, ad_rich_mod4, ad_rich_mod5, ad_rich_mod6, ad_rich_mod7)
#null model has the lowest AIC

##NInta cover####
ad_covdat <- all_result[-which(is.na(all_result$NInta_cover_binom)) , ] #remove rows with NA

#nullmod
additive_null_covmod <- glmmTMB(NInta_cover_binom ~ 1 +(1|site_ID),  
                                family = binomial, data = ad_covdat)

##graz
ad_cov_mod1 <- glmmTMB(NInta_cover_binom ~ graz +(1|site_ID),  
                        family = binomial, data = ad_covdat)
summary(ad_cov_mod1)
Anova(ad_cov_mod1)
lsmeans(ad_cov_mod1, specs = "graz")
cld(lsmeans(ad_cov_mod1, specs = "graz"), Letters = "abcdefg")

r.squaredGLMM(ad_cov_mod1) #take the theoretical

#aridity
ad_cov_mod2 <- glmmTMB(NInta_cover_binom ~ aridity + (1|site_ID),  
                        family = binomial, data = ad_covdat)
summary(ad_cov_mod2)
Anova(ad_cov_mod2)

#aridity + arid_sq
ad_cov_mod3 <- glmmTMB(NInta_cover_binom ~ aridity + arid_sq +(1|site_ID),  
                        family = binomial, data = ad_covdat)
summary(ad_cov_mod3)
Anova(ad_cov_mod3)

##graz + aridity
ad_cov_mod4 <- glmmTMB(NInta_cover_binom ~ graz + aridity +(1|site_ID),  
                        family = binomial, data = ad_covdat)
summary(ad_cov_mod4)
Anova(ad_cov_mod4)

##graz + aridity + arid_sq
ad_cov_mod5 <- glmmTMB(NInta_cover_binom ~ graz + aridity + arid_sq +(1|site_ID),  
                        family = binomial, data = ad_covdat)
summary(ad_cov_mod5)
Anova(ad_cov_mod5)

#graz*aridity
ad_cov_mod6 <- glmmTMB(NInta_cover_binom ~ graz*aridity + (1|site_ID),  
                        family = binomial, data = ad_covdat)
summary(ad_cov_mod6)
Anova(ad_cov_mod6)

#graz*aridity + graz*arid_sq
ad_cov_mod7 <- glmmTMB(NInta_cover_binom ~ graz*aridity + graz*arid_sq +(1|site_ID),  
                        family = binomial, data = ad_covdat)
summary(ad_cov_mod7)
Anova(ad_cov_mod7)

AIC(additive_null_covmod, ad_cov_mod1, ad_cov_mod2, ad_cov_mod3, ad_cov_mod4, ad_cov_mod5, ad_cov_mod6, ad_cov_mod7)
#ad_cov_mod1 has the lowest AIC



###Proportion of facilitation at each plot####
#This is the proportion of facilitative, compettitve and neutral interactions according to NIntc richness at each plot
propdat <- data.frame(matrix(ncol = 8, nrow = length(unique(all_result$ID))))
colnames(propdat) <- c("ID", "country", "site_ID", "aridity", "graz", "prop_facilitation", "prop_competition", "prop_neutral")
IDlist <- c(unique(all_result$ID))

l = 1
for(p in IDlist) {
  subs <- all_result[which(all_result$ID == p) , ]
  n <- nrow(subs)
  fac <- nrow(subs[which(subs$NIntc_richness > 0) , ] )
  comp <- nrow(subs[which(subs$NIntc_richness < 0) , ] )
  neutral <- nrow(subs[which(subs$NIntc_richness == 0) , ] )
  
  propdat[l,1] <- subs$ID[1]
  propdat[l,2] <- subs$country[1]
  propdat[l,3] <- subs$site_ID[1]
  propdat[l,4] <- subs$aridity[1]
  propdat[l,5] <- subs$graz[1]
  propdat[l,6] <- fac/n
  propdat[l,7] <- comp/n
  propdat[l,8] <- neutral/n
  
  l = l+1
}

plot(propdat$prop_facilitation ~ propdat$aridity)
plot(propdat$prop_competition ~ propdat$aridity)
hist(propdat$prop_facilitation)

propdat$site_ID <- as.factor(propdat$site_ID)
propdat$graz <- as.factor(propdat$graz)
propmod <- glmmADMB::glmmadmb(formula = prop_facilitation ~ graz * aridity + (1|site_ID),
                              data = propdat, family = "nbinom2")
summary(propmod)

null_propmod <- glmmADMB::glmmadmb(formula = prop_facilitation ~ 1 + (1|site_ID),
                                   data = propdat, family = "nbinom2")

anova(null_propmod , propmod)
Anova(propmod)






###DESCRIPTIVE STATISTICS####
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


###Species position along aridity####
#Does aridity influence how many species grow in bare, open and both microsites
#We require raw country data
wd <- "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2"
data_files <- list.files(wd)
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2\\",
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
#write.csv(sp_preference, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\plotlevels_sp_preference_26Sept.csv")
sp_preference <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\plotlevels_sp_preference_26Sept.csv", row.names = 1)
sp_preference$ID <- as.factor(sp_preference$ID)
sp_preference$site_ID <- as.factor(sp_preference$site_ID)
sp_preference$graz <- as.factor(sp_preference$graz)

#In how many of the plots did the majority of sp prefer nurse microsites?
length(sp_preference[which(sp_preference$prop_nurse_only > sp_preference$prop_both) , ]) #8

#In how many of the sites did the majority of sp prefer open microsites?
length(sp_preference[which(sp_preference$prop_bare_only > sp_preference$prop_both) , ]) #8

#On average, how many species ocurred in both nurse and bare microsites
avg_both <- sum(sp_preference$prop_both)/nrow(sp_preference)



###Does the proportion of NURSE only sp change along grazing and aridity?
#nullmod
null_prefmod_nurse <- glmmTMB(prop_nurse_only ~ 1 +(1|site_ID),  
                     family = binomial, data = sp_preference)

#graz
pref_mod1 <- glmmTMB(prop_nurse_only ~ graz +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(pref_mod1)
Anova(pref_mod1)
anova(null_prefmod_nurse, pref_mod1)#0.7507

#aridity
pref_mod2 <- glmmTMB(prop_nurse_only ~ aridity + (1|site_ID),  
                     family = binomial, data = sp_preference)
summary(pref_mod2)
Anova(pref_mod2)
anova(null_prefmod_nurse, pref_mod2) #0.8587

#aridity + arid_sq
pref_mod3 <- glmmTMB(prop_nurse_only ~ aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(pref_mod3)
Anova(pref_mod3) 
anova(null_prefmod_nurse, pref_mod3)#0.9342

##graz + aridity
pref_mod4 <- glmmTMB(prop_nurse_only ~ graz + aridity +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(pref_mod4)
Anova(pref_mod4)
anova(null_prefmod_nurse, pref_mod4) #0.8715

##graz + aridity + arid_sq
pref_mod5 <- glmmTMB(prop_nurse_only ~ graz + aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(pref_mod5)
Anova(pref_mod5)
anova(null_prefmod_nurse, pref_mod5) #0.93

#graz*aridity
pref_mod6 <- glmmTMB(prop_nurse_only ~ graz*aridity + (1|site_ID),  
                     family = binomial, data = sp_preference)
summary(pref_mod6)
Anova(pref_mod6)
anova(null_prefmod_nurse, pref_mod6) #0.9743

#graz*aridity + graz*arid_sq
pref_mod7 <- glmmTMB(prop_nurse_only ~ graz*aridity + graz*arid_sq +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(pref_mod7)
Anova(pref_mod7)
anova(null_prefmod_nurse, pref_mod7)#0.9987

lsmeans(pref_mod7, specs = "graz")

AIC(null_prefmod_nurse, pref_mod1, pref_mod2, pref_mod3, pref_mod4, pref_mod5, pref_mod6, pref_mod7)
#null model has the lowest AIC




###Does the proportion of BARE only sp change along grazing and aridity?
#nullmodel
null_prefmod_bare <- glmmTMB(prop_nurse_only ~ 1 +(1|site_ID),  
                     family = binomial, data = sp_preference)

#graz
bare_mod1 <- glmmTMB(prop_bare_only ~ graz +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(bare_mod1)
Anova(bare_mod1)
anova(null_prefmod_bare, bare_mod1) #7.395e-07 ***

#aridity
bare_mod2 <- glmmTMB(prop_bare_only ~ aridity + (1|site_ID),  
                     family = binomial, data = sp_preference)
summary(bare_mod2)
Anova(bare_mod2)
plot(sp_preference$prop_bare_only ~ sp_preference$aridity)
#anova(null_prefmod_bare, bare_mod2) #2.698e-08 ***
r.squaredGLMM(bare_mod2)##use the theoretical R2 for binomial family

#aridity + arid_sq
bare_mod3 <- glmmTMB(prop_bare_only ~ aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(bare_mod3)
Anova(bare_mod3)
anova(null_prefmod_bare, bare_mod3) #1.933e-07 ***

##graz + aridity
bare_mod4 <- glmmTMB(prop_bare_only ~ graz + aridity +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(bare_mod4)
Anova(bare_mod4)
anova(null_prefmod_bare, bare_mod4) #2.669e-06 ***

##graz + aridity + arid_sq
bare_mod5 <- glmmTMB(prop_bare_only ~ graz + aridity + arid_sq +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(bare_mod5)
Anova(bare_mod5) 
anova(null_prefmod_bare, bare_mod5) #8.183e-06 ***

#graz*aridity
bare_mod6 <- glmmTMB(prop_bare_only ~ graz*aridity + (1|site_ID),  
                     family = binomial, data = sp_preference)
summary(bare_mod6)
Anova(bare_mod6)
anova(null_prefmod_bare, bare_mod6) #3.707e-05 ***

#graz*aridity + graz*arid_sq
bare_mod7 <- glmmTMB(prop_bare_only ~ graz*aridity + graz*arid_sq +(1|site_ID),  
                     family = binomial, data = sp_preference)
summary(bare_mod7)
Anova(bare_mod7) ##model convergence problem
#anova(null_prefmod_bare, bare_mod7)

AIC(null_prefmod_bare, bare_mod1, bare_mod2, bare_mod3, bare_mod4, bare_mod5, bare_mod6, bare_mod7)
#bare_mod2 has the lowest AIC

###CHisq tests of species association with nurse or bare microsites####
###First we need to get the number times a species is present/absent in each microsite
#We require raw country data
wd <- "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2"
data_files <- list.files(wd)
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2\\",
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
write.csv(Chisq_results, "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\Chisq_results_27Sep.csv")
Chisq_results <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\Chisq_results_27Sep.csv", row.names = 1)

#How many sp significantly associated with the nurse?
nrow(Chisq_results[which(Chisq_results$association == "nurse") , ]) #124
#How many sp significantly associated with the bare microsite?
nrow(Chisq_results[which(Chisq_results$association == "bare") , ]) #52
#How many sp show neutral association?
nrow(Chisq_results[which(Chisq_results$association == "neutral") , ]) #431
#How many sp are too rare
nrow(Chisq_results[which(Chisq_results$association == "too_rare") , ]) #1269
nrow(Chisq_results) #1876


##Isolate too-rare species
rare <- Chisq_results[which(Chisq_results$association == "too_rare") , ]
rest <- Chisq_results[-which(Chisq_results$association == "too_rare") , ]

max(rare$nurse_p + rare$bare_p)
min(rare$nurse_p + rare$bare_p)

max(rest$nurse_p + rest$bare_p)
min(rest$nurse_p + rest$bare_p)

test <- filter(Chisq_results, Chisq_results$nurse_p + Chisq_results$bare_p > 9)

####Is sspecies association influenced by aridity and graz?###
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