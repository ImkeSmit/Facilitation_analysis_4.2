##This is a test with supplied dummy data to understand why model predictions are so much lower than the mean. 
##I will build a similar model with the owls data and also plot model predictions

library(glmmADMB)
library(car)
library(ggplot2)
library(glmmTMB)

owldat <- Owls
owldat$Nest <- as.factor(owldat$Nest)
owldat$FoodTreatment <- as.factor(owldat$FoodTreatment)
owldat$Brood_sq <- owldat$logBroodSize^2

mod1 <- glmmADMB::glmmadmb(formula = SiblingNegotiation ~ 
                     logBroodSize + Brood_sq +FoodTreatment + logBroodSize:FoodTreatment 
                     + Brood_sq:FoodTreatment +(1|Nest),
                   data = owldat, family = "poisson")
summary(mod1)
Anova(mod1)

ggplot(owldat, aes(x = logBroodSize, y = SiblingNegotiation)) + 
  geom_jitter(shape = 21, size = 2, fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  geom_smooth(method = "loess", span = 1, aes(color = FoodTreatment, fill = FoodTreatment)) +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue" )) +
  facet_wrap(~FoodTreatment) +
  theme(legend.position = "none")


#Make a dtaframe to get model predictions
#Deprived
pred_data_0 <- data.frame(logBroodSize = seq(min(owldat[which(owldat$FoodTreatment == "Deprived") , ]$logBroodSize), 
                                             max(owldat[which(owldat$FoodTreatment == "Deprived") , ]$logBroodSize), 0.01))
pred_data_0$Brood_sq <- pred_data_0$logBroodSize^2
pred_data_0$FoodTreatment<- "Deprived"
pred_data_0$colour <- "red"

#Satiated
pred_data_1 <- data.frame(logBroodSize = seq(min(owldat[which(owldat$FoodTreatment == "Satiated") , ]$logBroodSize), 
                                             max(owldat[which(owldat$FoodTreatment == "Satiated") , ]$logBroodSize), 0.01))
pred_data_1$Brood_sq <- pred_data_1$logBroodSize^2
pred_data_1$FoodTreatment<- "Satiated"
pred_data_1$colour <- "blue"

pred_data <- rbind(pred_data_0, pred_data_1)

#Get model predictions
model_prediction <- data.frame(predicted_sibneg = predict(mod1, newdata = pred_data, type = "response"))
model_prediction <- cbind(model_prediction, pred_data)

##Now make a graph showing means and predictions

ggplot() + 
  geom_jitter(data = owldat, aes(x = logBroodSize, y = SiblingNegotiation), shape = 21, size = 2, 
              fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  geom_smooth(data = owldat, method = "loess", span = 1, aes(x = logBroodSize, y = SiblingNegotiation, color = FoodTreatment, fill = FoodTreatment)) +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue" )) +
  facet_wrap(~FoodTreatment) +
  geom_line(data = model_prediction, aes(x = logBroodSize, y = predicted_sibneg, color = FoodTreatment), lty = 2, lwd = 1) +
  theme(legend.position = "none")

####NOw create a dataset similar to mine with realistic NIntc values####
##Force a positive relationship between NIntc and grazing pressure
##There won't be a relationship between aridity and NIntc
#make aridity values to use in every grazing level
#arid_vals <- c(runif(n = 100, min = 0.5, max = 0.94))

##Graz0
dum_graz0 <- data.frame(NIntc = c(runif(n = 100, min = -1, max = -0.5)), 
                        graz = "0", aridity = c(runif(n = 100, min = 0.5, max = 0.94)))
#Graz1
dum_graz1 <- data.frame(NIntc = c(runif(n = 100, min = -0.7, max = 1)), 
                        graz = "1", aridity = c(runif(n = 100, min = 0.5, max = 0.94)))
#Graz2
dum_graz2 <- data.frame(NIntc = c(runif(n = 100, min = -0.2, max = 0.7)), 
                        graz = "2", aridity = c(runif(n = 100, min = 0.5, max = 0.94)))
#Graz3
dum_graz3 <- data.frame(NIntc = c(runif(n = 100, min = 0.4, max = 1)), 
                        graz = "3", aridity = c(runif(n = 100, min = 0.5, max = 0.94)))

#rbind all the grazing levels together
dumdat <- rbind(dum_graz0, dum_graz1, dum_graz2, dum_graz3)

#add a random effect
dumdat$rando <- c("1", "2", "3", "4")
dumdat$rando <- as.factor(dumdat$rando)

#make graz and ordered factor
#dumdat$graz <- as.factor(dumdat$graz)
#dumdat$graz <- ordered(dumdat$graz, levels = c("0", "1", "2", "3"))
dumdat$graz <- factor(dumdat$graz, ordered = TRUE, levels = c("0", "1", "2", "3"))
#dumdat$graz <- as.numeric(dumdat$graz)
#add aridity squared
dumdat$arid_sq <- dumdat$aridity^2
#transform NIntc to be between 0 and 1
dumdat$NIntc_binom <- (dumdat$NIntc + 1)/2

#Lets look at the data
#Nintc against grazing pressure
ggplot(dumdat, aes(x = graz, y = NIntc, fill = graz, alpha = 0.5)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  xlab("Grazing pressure") +
  ylab(expression(NInt[C]))+
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  theme_classic() +
  theme(legend.position = "none")


ggplot(dumdat, aes(x = graz, y = aridity, fill = graz, alpha = 0.5)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  xlab("Grazing pressure") +
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  theme_classic() +
  theme(legend.position = "none")

#NIntc against aridity
ggplot(dumdat, aes(x = aridity, y = NIntc, colour = graz)) +
  geom_point() +
  scale_colour_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  xlab("aridity") +
  ylab(expression(NInt[C]))+
  theme_classic()


ggplot(dumdat, aes(x = aridity, y = NIntc_binom, colour = graz)) +
  geom_point() +
  scale_colour_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  xlab("aridity") +
  ylab(expression(NInt[C]~binom))+
  theme_classic()

##generalised linear model
mod2 <- glmmADMB::glmmadmb(formula = NIntc_binom ~ 
                             graz + aridity + arid_sq + graz:aridity + graz:arid_sq + (1|rando), 
                           data = dumdat, family = "binomial")

summary(mod2)
Anova(mod2)

mod3 <- glmmADMB::glmmadmb(formula = NIntc_binom ~ 
                             graz  + (1|rando),
                           data = dumdat, family = "binomial")
summary(mod3)

mod4 <- glm(NIntc_binom ~ graz*aridity, 
            data = dumdat, family = "binomial")
summary(mod4)


mod5 <- glmmADMB::glmmadmb(formula = NIntc_binom ~ 
                             graz,
                           data = dumdat, family = "binomial")
summary(mod5)


##Now let's get the model predictions.
#first create values to predict over
pred_0 <- data.frame(aridity = seq(min(dumdat[which(dumdat$graz == 0) , ]$aridity), max(dumdat[which(dumdat$graz == 0) , ]$aridity), 0.01), 
                     graz = "0", colour = "darkgreen")
pred_0$arid_sq <- pred_0$aridity^2

pred_1 <- data.frame(aridity = seq(min(dumdat[which(dumdat$graz == 1) , ]$aridity), max(dumdat[which(dumdat$graz == 1) , ]$aridity), 0.01), 
                     graz = "1", colour = "chartreuse2")
pred_1$arid_sq <- pred_1$aridity^2

pred_2 <- data.frame(aridity = seq(min(dumdat[which(dumdat$graz == 2) , ]$aridity), max(dumdat[which(dumdat$graz == 2) , ]$aridity), 0.01), 
                     graz = "2", colour = "darkolivegreen3")
pred_2$arid_sq <- pred_2$aridity^2

pred_3 <- data.frame(aridity = seq(min(dumdat[which(dumdat$graz == 3) , ]$aridity), max(dumdat[which(dumdat$graz == 3) , ]$aridity), 0.01), 
                     graz = "3", colour = "darkgoldenrod4")
pred_3$arid_sq <- pred_3$aridity^2


mod2_pred_data <- rbind(pred_0, pred_1, pred_2, pred_3)
mod2_pred_data$graz <- ordered(mod2_pred_data$graz, levels = c("0", "1", "2", "3"))


mod2_prediction <- data.frame(predicted_NIntc_binom = predict(mod2, newdata = mod2_pred_data, type = "response"))
mod2_prediction <- cbind(mod2_prediction, mod2_pred_data)
mod2_prediction$predicted_NIntc <- (mod2_prediction$predicted_NIntc_binom*2)-1

##Now graph the predictions
pred_plot <- ggplot() + 
  geom_jitter(data = dumdat, aes(x = aridity, y = NIntc), shape = 21, size = 2, 
              fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  geom_smooth(data = dumdat, method = "loess", span = 1, aes(x = aridity, y = NIntc, color = graz, fill = graz)) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  facet_wrap(~graz, labeller = as_labeller(c("0" = "Ungrazed", "1" = "Low grazing pressure", "2" = "Medium grazing pressure", "3" = " High grazing pressure"))) +
  geom_line(data = mod2_prediction, aes(x = aridity, y = predicted_NIntc, color = graz), lty = 2, lwd = 1) +
  xlab("Aridity") +
  ylab(expression(NInt[C])) +
  theme(legend.position = "none")
pred_plot


###Now lets try it with glmmTMB####
#build models with all possible variable combinations

tmod1 <- glmmTMB(NIntc_binom ~ graz + (1|rando), data = dumdat, family = binomial)
summary(tmod1)

tmod2 <- glmmTMB(NIntc_binom ~ aridity + (1|rando), data = dumdat, family = binomial)
summary(tmod2)

tmod3 <- glmmTMB(NIntc_binom ~ aridity + arid_sq + (1|rando), data = dumdat, family = binomial)
summary(tmod3)

tmod4 <- glmmTMB(NIntc_binom ~ graz + aridity + (1|rando), data = dumdat, family = binomial)
summary(tmod4)

tmod5 <- glmmTMB(NIntc_binom ~ graz + aridity + arid_sq + (1|rando), data = dumdat, family = binomial)
summary(tmod5)

tmod6 <- glmmTMB(NIntc_binom ~ graz*aridity + (1|rando), data = dumdat, family = binomial)
summary(tmod6)

tmod7 <- glmmTMB(NIntc_binom ~ graz*aridity + graz*arid_sq + (1|rando), data = dumdat, family = binomial)
summary(tmod7)


##Now lets look at the model predictions
#Use mod2 pred data created previously, and tmod7 to make predictions

predict(tmod7, newdata = mod2_pred_data, type = "response")

tmod_prediction <- data.frame(predicted_NIntc_binom = predict(tmod7, newdata = mod2_pred_data, type = "response"))
tmod_prediction <- cbind(tmod_prediction, mod2_pred_data)
tmod_prediction$predicted_NIntc <- (tmod_prediction$predicted_NIntc_binom*2)-1

##Now graph the predictions
pred_plot <- ggplot() + 
  geom_jitter(data = dumdat, aes(x = aridity, y = NIntc), shape = 21, size = 2, 
              fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  geom_smooth(data = dumdat, method = "loess", span = 1, aes(x = aridity, y = NIntc, color = graz, fill = graz)) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  facet_wrap(~graz, labeller = as_labeller(c("0" = "Ungrazed", "1" = "Low grazing pressure", "2" = "Medium grazing pressure", "3" = " High grazing pressure"))) +
  geom_line(data = tmod_prediction, aes(x = aridity, y = predicted_NIntc), lty = 2, lwd = 1) +
  xlab("Aridity") +
  ylab(expression(NInt[C])) +
  theme(legend.position = "none")
pred_plot

