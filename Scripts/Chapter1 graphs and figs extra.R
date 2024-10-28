###CHAPTER 1 GRAPHS AND FIGURES###
library(data.table)
library(ggplot2)
library(hexbin)
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(forcats)
library(tidyr)
library(lsmeans)
#library(multcomp)
#library(multcompView)
library(glmmTMB)

IDlist <- unique(all_result$ID)
res <- data.frame(matrix(nrow = length(IDlist), ncol = 3))
colnames(res) <- c("ID", "max_reps", "n_nurses")
l =1

for (i in 1:length(IDlist)) {
  sub <- all_result[which(all_result$ID == IDlist[i]) , ]
  
  res[l, 1] <- IDlist[i]
  res[l,2] <- max(sub$replicate_no)
  res[l, 3] <- length(unique(sub$nurse))
  
   l = l+1
}

res$product <- res$max_reps*res$n_nurses



all_result <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv", row.names = 1)
all_result$site_ID <- as.factor(all_result$site_ID)
all_result$graz <- as.factor(all_result$graz)
all_result$ID <- as.factor(all_result$ID)

###facet wrap of Nintc richness accross aridity####
#all data with transparent bubbles
bubble_facets <- ggplot(all_result, aes(x = aridity, y = NIntc_richness)) + 
  geom_jitter(shape = 21, size = 2, fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  geom_smooth(method = "loess", span = 1, aes(color = graz, fill = graz)) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  facet_wrap(~graz, labeller = as_labeller(c("0" = "Ungrazed", "1" = "Low grazing pressure", "2" = "Medium grazing pressure", "3" = " High grazing pressure"))) +
  xlab("Aridity") +
  ylab(expression(NInt[C]~richness)) +
  theme(legend.position = "none")
bubble_facets
ggsave("rich_grazing facets bubble.png", bubble_facets, width = 1400, height = 1500, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Figures")


#import model predictions
rich_pred <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\rich_mod4_prediction_tmb_11Sept.csv", row.names = 1)
#backtransform NIntc richness binom to range between -1 and 1 again
rich_pred$predicted_NIntc <- (2*rich_pred$predicted_NIntc_binom)-1
rich_pred$graz <- as.factor(rich_pred$graz)


pred_plot <- ggplot() + 
  geom_jitter(data = all_result, aes(x = aridity, y = NIntc_richness), shape = 21, size = 2, 
              fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  geom_smooth(data = all_result, method = "loess", span = 1, aes(x = aridity, y = NIntc_richness, color = graz, fill = graz)) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  facet_wrap(~graz, labeller = as_labeller(c("0" = "Ungrazed", "1" = "Low grazing pressure", "2" = "Medium grazing pressure", "3" = " High grazing pressure"))) +
  geom_line(data = rich_pred, aes(x = aridity, y = predicted_NIntc), lty = 2, lwd = 1) +
  xlab("Aridity") +
  ylab(expression(NInt[C]~richness)) +
  theme(legend.position = "none")
pred_plot
ggsave("rich_grazing_facets_mod4_predict.png", pred_plot, width = 1400, height = 1500, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")






###facet wrap of Nintc cover accross aridity####
#All data

#import model predictions
#model_predictions_richness_factorgraz are with graz as an ordered factor
cov_pred <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\model_prediction_cover_factorgraz.csv", row.names = 1)
#backtransform NIntc richness binom to range between -1 and 1 again
cov_pred$predicted_NIntc <- (2*cov_pred$predicted_NIntc_cover_binom)-1
cov_pred$graz <- as.factor(cov_pred$graz)


cov_pred_plot <- ggplot() + 
  geom_jitter(data = all_result, aes(x = aridity, y = NIntc_cover), shape = 21, size = 2, 
              fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  geom_smooth(data = all_result, method = "loess", span = 1, aes(x = aridity, y = NIntc_cover, color = graz, fill = graz)) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  facet_wrap(~graz, labeller = as_labeller(c("0" = "Ungrazed", "1" = "Low grazing pressure", "2" = "Medium grazing pressure", "3" = " High grazing pressure"))) +
  geom_line(data = cov_pred, aes(x = aridity, y = predicted_NIntc, color = graz), lty = 2, lwd = 1) +
  xlab("Aridity") +
  ylab(expression(NInt[C]~cover)) +
  theme(legend.position = "none")
cov_pred_plot
ggsave("cov_grazing facets predict.png", pred_plot, width = 1400, height = 1500, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")



###Barplot of NINtc richness at different grazing levels####
###Are the means different from each other?
modeldat <- all_result[-which(is.na(all_result$NIntc_richness)) , ]
modeldat$NIntc_richness_binom <- (modeldat$NIntc_richness + 1)/2

rich_mod1 <- glmmTMB(NIntc_richness_binom ~ graz +(1|site_ID),  
                    family = binomial, data = modeldat)
summary(rich_mod1)
lsmeans(rich_mod1, specs = "graz")
rich_letters <- cld(lsmeans(rich_mod1, specs = "graz"), Letters = "abcdefg")
#none are significantly different from each other
#another way:
glht.mod <- glht(model = rich_mod1, linfct = mcp(graz = "Tukey"))
cld(glht.mod)

###get the mean and se of NINtc at each grazing level. Use modeldat because NA has been removed
means <- data.frame(tapply(modeldat$NIntc_richness, modeldat$graz, FUN = "mean"))
colnames(means) <- "mean_NIntc_richness"
means$graz = rownames(means)
rownames(means) <- c(1:nrow(means))

n_replicates <- data.frame(tapply(modeldat$NIntc_richness, modeldat$graz, FUN = "length"))
colnames(n_replicates) <- "n_replicates"
n_replicates$graz = rownames(n_replicates)
rownames(n_replicates) <- c(1:nrow(n_replicates))

sd <- data.frame(tapply(modeldat$NIntc_richness, modeldat$graz, FUN = "sd"))
colnames(sd) <- "std_dev"
sd$graz = rownames(sd)
rownames(sd) <- c(1:nrow(sd))

se <- data.frame(std_error = sd$std_dev/sqrt(n_replicates$n_replicates))

grazlevel_stats <- cbind(means, se)
#get mean - std error
grazlevel_stats$ymin <- grazlevel_stats$mean_NIntc_richness - grazlevel_stats$std_error
#get mean + std error
grazlevel_stats$ymax <- grazlevel_stats$mean_NIntc_richness + grazlevel_stats$std_error

##Now we need to do one sample t tests to see if the means are significantly differnt from 0
#graz0
t.test(modeldat[which(modeldat$graz == 0) , ]$NIntc_richness)
#p-value = 2.123e-05

#graz1
t.test(modeldat[which(modeldat$graz == 1) , ]$NIntc_richness)
#p-value = 5.173e-08

#graz2
t.test(modeldat[which(modeldat$graz == 2) , ]$NIntc_richness)
#p-value < 2.2e-16

#graz3
t.test(modeldat[which(modeldat$graz == 3) , ]$NIntc_richness)
#p-value = 1.144e-15
##SO ALL are differnt from 0
#none are sign differnt from each other

#create a dataframe so that we can add asterisks to the plot
rich_aster <- data.frame(graz = c("0", "1", "2", "3"), ycoord = c(0.15, 0.13, 0.18, 0.18))
rich_letters <- data.frame(graz = c("0", "1", "2", "3"), ycoord = c(0.24, 0.24, 0.24, 0.24))

rich_graz_bar <- ggplot() +
  geom_bar(data = grazlevel_stats, aes(x = graz, y = mean_NIntc_richness, fill = graz), stat = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  geom_errorbar(data = grazlevel_stats, aes(x = graz, ymin  = ymin, ymax = ymax), colour="black", width = 0.5)+
  ylim(0, 0.25) +
  geom_text(data = rich_aster, aes(x = graz, y = ycoord), label = "*", size = 6) +
  geom_text(data = rich_letters, aes(x= graz, y = ycoord), label = c("a", "a", "a", "a"))+
  xlab("Grazing pressure") +
  ylab(expression(mean~NInt[C]~richness))+
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  theme_classic()+
  theme(legend.position = "none")
rich_graz_bar
ggsave("rich_grazlevel_bar.png", rich_graz_bar, 
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")
  

###Barplot of NINtc  cover at different grazing levels####
#we will do a plot of the arithmetic means, and also the lsmeans with significance letters.

###lsmeans
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
ggsave("lsmean_cov_grazlevel_bar.png", lsmean_cov_graz_bar, width = 1300, height = 800, unit = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")



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

cov_grazlevel_stats <- cbind(aridth_means, se)
#get mean - std error
cov_grazlevel_stats$ymin <- cov_grazlevel_stats$mean_NIntc_cover - cov_grazlevel_stats$std_error
#get mean + std error
cov_grazlevel_stats$ymax <- cov_grazlevel_stats$mean_NIntc_cover + cov_grazlevel_stats$std_error

##Now we need to do one sample t tests to see if the means are significantly differnt from 0
#graz0
t.test(covdat[which(covdat$graz == 0) , ]$NIntc_cover)
#p-value = 0.004373v

#graz1
t.test(covdat[which(covdat$graz == 1) , ]$NIntc_cover)
# p-value = 4.88e-07

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
ggsave("arithmetic_cov_grazlevel_bar.png", arith_cov_graz_bar, width = 1300, height = 800, unit = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")


##arrange the arithmetic and lsmean plots on the same pane
cov_grazlevel_arith_and_lsmeans <- ggarrange(lsmean_cov_graz_bar, arith_cov_graz_bar, ncol = 2, nrow = 1, labels = c("a", "b"))
ggsave("combo_arithmetic_lsmean_cov_grazlevel_bar.png", cov_grazlevel_arith_and_lsmeans, width = 1900, height = 1000, unit = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")



###Barplot of NINta  cover at different grazing levels####
#we will do a plot of the arithmetic means, and also the lsmeans with significance letters.
ad_covdat <- all_result[-which(is.na(all_result$NInta_cover)) , ]

##lsmeans
#does graz affect NINtc cover
ad_covdat$NInta_cover_binom <- (ad_covdat$NInta_cover - (-1)) / (2 - (-1))
ad_cov_mod1 <- glmmTMB(NInta_cover_binom ~ graz +(1|site_ID),  
                    family = binomial, data = ad_covdat)
summary(ad_cov_mod1)
lsmeans(ad_cov_mod1, specs = 'graz')
ad_lsmeans <- c(0.0300, -0.1895, 0.0377, 0.1569) #estimated lsmeans
ad_SE <- c(0.158, 0.141, 0.139, 0.143)
cld(glht(model = ad_cov_mod1, mcp(graz = "Tukey")))
ad_lsmeans_letters <- c("ab", "a", "ab", "b")

ad_cov_grazlevel_lsmeans <- data.frame(lsmean = ad_lsmeans, std_error = ad_SE, 
                                    graz = c(0,1,2,3), sign_letters = ad_lsmeans_letters, ycoord = c(0.38, 0.38, 0.38, 0.38))
ad_cov_grazlevel_lsmeans$ymin <- ad_cov_grazlevel_lsmeans$lsmean - ad_cov_grazlevel_lsmeans$std_error
ad_cov_grazlevel_lsmeans$ymax <- ad_cov_grazlevel_lsmeans$lsmean + ad_cov_grazlevel_lsmeans$std_error
ad_cov_grazlevel_lsmeans$graz <- as.factor(ad_cov_grazlevel_lsmeans$graz)

#now make the lsmean graph
ad_cov_graz_bar_lsmean <- ggplot() +
  geom_bar(data = ad_cov_grazlevel_lsmeans, aes(x = graz, y = lsmean, fill = graz), stat = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  geom_errorbar(data = ad_cov_grazlevel_lsmeans, aes(x = graz, ymin  = ymin, ymax = ymax), colour="black", width = 0.5)+
  ylim(-0.4, 0.7) +
  geom_text(data = ad_cov_grazlevel_lsmeans, aes(x = graz, y = ycoord), 
            label = c(ad_cov_grazlevel_lsmeans$sign_letters))+
  xlab("Grazing pressure") +
  ylab(expression(Estimated~mean~NInt[A]~cover))+
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  geom_hline(yintercept = 0) +
  theme_classic()+
  theme(legend.position = "none")
ad_cov_graz_bar_lsmean
ggsave("NInta_lsmean_cov_grazlevel_bar.png", ad_cov_graz_bar_lsmean,width = 1300, height = 800, unit = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")



##arithmetic means
###get the mean and se of NINtc at each grazing level. Use modeldat because NA has been removed
arith_means <- data.frame(tapply(ad_covdat$NInta_cover, ad_covdat$graz, FUN = "mean"))
colnames(arith_means) <- "mean_NInta_cover"
arith_means$graz = rownames(arith_means)
rownames(arith_means) <- c(1:nrow(arith_means))

n_replicates <- data.frame(tapply(ad_covdat$NInta_cover, ad_covdat$graz, FUN = "length"))
colnames(n_replicates) <- "n_replicates"
n_replicates$graz = rownames(n_replicates)
rownames(n_replicates) <- c(1:nrow(n_replicates))

sd <- data.frame(tapply(ad_covdat$NInta_cover, ad_covdat$graz, FUN = "sd"))
colnames(sd) <- "std_dev"
sd$graz = rownames(sd)
rownames(sd) <- c(1:nrow(sd))

se <- data.frame(std_error = sd$std_dev/sqrt(n_replicates$n_replicates))

ad_cov_grazlevel_stats <- cbind(arith_means, se)
#get mean - std error
ad_cov_grazlevel_stats$ymin <- ad_cov_grazlevel_stats$mean_NInta_cover - ad_cov_grazlevel_stats$std_error
#get mean + std error
ad_cov_grazlevel_stats$ymax <- ad_cov_grazlevel_stats$mean_NInta_cover + ad_cov_grazlevel_stats$std_error

##Now we need to do one sample t tests to see if the means are significantly differnt from 0
#graz0
t.test(ad_covdat[which(ad_covdat$graz == 0) , ]$NInta_cover)
#p-value =5.38e-15

#graz1
t.test(ad_covdat[which(ad_covdat$graz == 1) , ]$NInta_cover)
# p-value =2.2e-16

#graz2
t.test(ad_covdat[which(ad_covdat$graz == 2) , ]$NInta_cover)
#p-value < 2.2e-16

#graz3
t.test(ad_covdat[which(ad_covdat$graz == 3) , ]$NInta_cover)
#p-value < 2.2e-16
##SO ALL are differnt from 0


#create a dataframe so that we can add asterisks to the plot
ad_cov_aster <- data.frame(graz = c("0", "1", "2", "3"), ycoord = c(0.44, 0.48, 0.55, 0.64))

ad_cov_graz_bar_arithmetic <- ggplot() +
  geom_bar(data = ad_cov_grazlevel_stats, aes(x = graz, y = mean_NInta_cover, fill = graz), stat = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  geom_errorbar(data = ad_cov_grazlevel_stats, aes(x = graz, ymin  = ymin, ymax = ymax), colour="black", width = 0.5)+
  ylim(-0.4, 0.70) +
  geom_text(data = ad_cov_aster, aes(x = graz, y = ycoord), label = "*", size = 6) +
  xlab("Grazing pressure") +
  ylab(expression(Arithmetic~mean~NInt[A]~cover))+
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  geom_hline(yintercept = 0) +
  theme_classic()+
  theme(legend.position = "none")
ad_cov_graz_bar_arithmetic
ggsave("NInta_arithmetic_cov_grazlevel_bar.png", ad_cov_graz_bar_arithmetic, width = 1300, height = 800, unit = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")


##Put the graphs of arithmetic and lsmean NInta cover on the same figure
NInta_cov_grazlevel_arith_and_lsmeans <- ggarrange(ad_cov_graz_bar_lsmean, ad_cov_graz_bar_arithmetic, ncol = 2, nrow = 1, labels = c("a", "b"))
ggsave("combo_NINta_arithmetic_lsmean_cov_grazlevel_bar.png", NInta_cov_grazlevel_arith_and_lsmeans, width = 1900, height = 1000, unit = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")


###Distribution of NIntc at different graz and aridity####
#First we have to bin aridity, make 3 bins for simplicity
sort(unique(all_result$aridity))
mult <- (0.9446 - 0.7474)/2 
densplot <- all_result %>% mutate(arid_bin = cut(aridity, breaks = c(0, 0.6720, (0.7474 + mult), (0.7474 + 2*mult)))) 
#n <- (max(all_result$aridity) - min(all_result$aridity))/3
#min <- min(all_result$aridity)
#densplot <- all_result %>% mutate(arid_bin = cut(aridity, breaks = c(0, min+n, min+n*2, min+n*3)))

density <- ggplot(densplot, aes(x = NIntc_richness, group = graz, fill = graz)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(labels = c("Ungrazed", "Low grazing", "Medium grazing", "High grazing"), 
                    values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  facet_wrap(~ arid_bin, labeller = as_labeller(c("(0,0.672]" = "Low aridity", "(0.672,0.846]" = "Medium aridity", "(0.846,0.945]" = "High aridity"))) +
  theme_classic() +
  xlab("NIntc richness") +
  ylab("Density") +
  theme(legend.title= element_blank(), legend.position = "bottom")
density
ggsave("density facets2.png", density, width = 1500, height = 865, units = "px",
       path = "C:\\Users\\user\\OneDrive\\Documents\\Msc Projek\\Methods and results\\Figures")



###NIntc along Aridity binned####
##Make 5 bins this time beacuse the two lowest bins have few data points and are far apart
sort(unique(all_result$aridity))
#first bin encompasses: 0.4952 0.5028 0.5150
#second bin encompasses: 0.6631 0.6719
 #the upper limit of the next 3 bins are created by adding mult to the third lowest ardidty value
mult <- (0.9446 - 0.7474)/3
forplot <- all_result %>% mutate(arid_bin = cut(aridity, breaks = c(0, 0.5151, 0.6720, (0.7474 + mult), (0.7474 + 2*mult), (0.7474 + 3*mult)))) 

#remove rows with NA in NIntc_richness
forplot <- forplot[-which(is.na(forplot$NIntc_richness)) , ]

binplota <- ggline(forplot, y = "NIntc_richness", x = "arid_bin",  
                    add = c("mean_se"), position = position_dodge(0.15), color = "graz", size = 0.5) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  theme_classic() +
  xlab("Aridity") +
  ylab("Mean of NIntc richness") +
  labs(color = "Grazing pressure") +
  theme(legend.position = "bottom", legend.text = element_text(size = 7), legend.title = element_text(size = 8, face = "bold"))
binplota
#save the plot
ggsave("binned line.png", binplota, path = "C:\\Users\\user\\OneDrive\\Documents\\Msc Projek\\Methods and results\\Figures")



###Plot richness and cover of plots along aridity gradient####
#To get this data we have to import the raw data again
wd <- "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2"
###read in the facilitation data for each country
data_files <- list.files(wd)
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\Countriesv2\\",
                          data_files[i])))
}



###loop to calculate the sprichness and total veg cover of each plot
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey") 

column_names <- c("country", "ID", "site_ID", "graz", "aridity", "plotlevel_sprichness", "plotlevel_vegcover")

plotsum_list <- c("plotlvl_algeria", "plotlvl_argentina", "plotlvl_australia", "plotlvl_chile", "plotlvl_chinachong", "plotlvl_chinaxin", "plotlvl_iranabedi", "plotlvl_iranfarzam", 
              "plotlvl_israel", "plotlvl_namibiablaum", "plotlvl_namibiawang", "plotlvl_southafrica",  "plotlvl_spainmaestre", "plotlvl_spainrey")

for (i in 1:length(countrynames)) {
  cou <- get(countrynames[i]) #working with one country at a time
  assign(paste("plotlvl", countrynames[i], sep= "_"), #create an empty sum table that will have the values for each plot ID
         data.frame(matrix(nrow = length(unique(as.factor(cou$ID))), ncol = length(column_names))))
  plotlvl_cou <- (get(plotsum_list[i])) #assign this empty table to result_cou
  colnames(plotlvl_cou) = column_names
  l = 1
  
  IDlist <- c(unique(as.factor(cou$ID)))
  for (k in IDlist) {
    sub <- cou[which(cou$ID == k) , ] #subset one ID out of the country dataset
    sub2 <- sub[which(!is.na(sub$Species.within.quadrat)) , ] #remove rows with NA values in Species within quadrat, they have no species so do not contribute to richness or totalcover
    
    #plot level species richness
    plot_sprich <- length(unique(sub2$Species.within.quadrat)) 
    
    #plot level vegetation cover
    totalcover <- sum(sub2$Cover)
    #to get the percentage cover we must control for the area sampled
    #so we divide by the highest possible percentage cover, which is the number of replicates times 200, because the max cover of each rep is 200% (bare and nurse microsite is 100% each)
    percentcover <- (totalcover/((length(unique(sub$Number.of.replicate)))*200))*100 
    
    plotlvl_cou[l,1] <- sub2$COU[1]
    plotlvl_cou[l,2] <- sub2$ID[1]
    plotlvl_cou[l,3] <- sub2$SITE_ID[1]
    plotlvl_cou[l,4] <- sub2$GRAZ[1]
    plotlvl_cou[l,5] <- sub2$ARIDITY.v3[1]
    plotlvl_cou[l,6] <- plot_sprich
    plotlvl_cou[l,7] <- percentcover
    
    l = l+1
  }
  
  assign(paste("plotlvl", countrynames[i], sep= "_"), plotlvl_cou) #name the result dataframe according to the country
}#if there are NA in the plotlevel vegcover column, it means that they didn't note cover for all the species in a plot


#bind all plotlvl results together
all_plotlvl <- rbind(plotlvl_algeria, plotlvl_argentina, plotlvl_australia, plotlvl_chile, plotlvl_chinachong, plotlvl_chinaxin, plotlvl_iranabedi, plotlvl_iranfarzam, 
                     plotlvl_israel, plotlvl_namibiablaum, plotlvl_namibiawang, plotlvl_southafrica,  plotlvl_spainmaestre, plotlvl_spainrey)
all_plotlvl$ID <- as.factor(all_plotlvl$ID)
all_plotlvl$graz <- as.factor(all_plotlvl$graz)

#3get the mean aridity at each site
site_arid <- data.frame(mean_aridity_of_site = tapply(all_plotlvl$aridity, all_plotlvl$site_ID, FUN = mean))
site_arid$site_ID <- rownames(site_arid)
rownames(site_arid) <- 1:nrow(site_arid)
##merge the mean aridity of each site with all_plotlvl
all_plotlvl <- merge(all_plotlvl, site_arid, by = "site_ID")
##define the order of grazing levels
all_plotlvl$graz <- factor(all_plotlvl$graz, levels = c("0", "1", "2", "3"))

##plot species richness in plots ordered according to mean aridity of site, then by graz colour by graz
bygraz_sprich_barplot <- 
  ggplot(all_plotlvl, aes(x = ID, y = plotlevel_sprichness, fill = graz)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(labels = c("Ungrazed", "Low grazing", "Medium grazing", "High grazing"), 
                    values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  theme_classic() +
  ylab("Perennial plant species richness") +
  xlab("") +
  scale_x_discrete(limits = all_plotlvl[order(all_plotlvl$mean_aridity_of_site, all_plotlvl$graz), "ID"], #first order by mean aridity then by graz
                   labels = c("85" = "0.50", "84" = "", "83" = "", 
                              "296" = "0.64", "295" = "", "294" = "", "293" = "",
                              "158" = "0.67", "157" = "", "156" = "", "155" = "", 
                              "274" = "0.75", "273" = "", "272" = "",
                              "103" = "0.75", "102" = "", "101" = "", "100" = "", 
                              "143" = "0.76", "142" = "", "141" = "", "140" = "", 
                              "50" = "0.76", "49" = "", "48" = "",
                              "99" = "0.77", "98" = "", "97" = "", 
                              "154" = "0.79", "153" = "", "152" = "", "151" = "",
                              "299" = "0.79", "298" = "", "297" = "", 
                              "146" = "0.80", "145" = "", "144" = "", 
                              "201" = "0.80", "200" = "", "199" = "", 
                              "47" = "0.81", "46" = "", "45" = "", "44" = "", 
                              "96" = "0.81", "95" = "", "94" = "", "93" = "", 
                              "139" = "0.82", "138" = "", "137" = "", 
                              "18" = "0.82", "17" = "", "16" = "", 
                              "136" = "0.83", "135" = "", "134" = "", 
                              "3" = "0.83", "2" = "", "1" = "", 
                              "198" = "0.84", "197" = "", "196" = "", 
                              "252" = "0.85", "251" = "", "250" = "", 
                              "292" = "0.85", "291" = "", "290" = "", "289" = "", 
                              "21" = "0.87", "20" = "", "19" = "", 
                              "150" = "0.87", "149" = "", "148" = "0.88", "147" = "", 
                              "249" = "0.88", "248" = "", "247" = "", 
                              "162" = "0.89", "161" = "", "160" = "", "159" = "", 
                              "195" = "0.90", "194" = "", "193" = "", 
                              "43" = "0.90", "42" = "", "41" = "", "40" = "", 
                              "216" = "0.91", "215" = "", "214" = "", 
                              "115" = "0.94", "114" = "")) +
  theme(axis.text.x = element_text(angle = 90), legend.title = element_blank(), legend.position = "none")
bygraz_sprich_barplot
ggsave("sprichness_barplot_bygraz.png", bygraz_sprich_barplot, width = 1950, height = 1000, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")



##Plot vegetation cover in plots ordered according to mean aridity of site then by graz, colour by graz
bygraz_cov_barplot <- 
  ggplot(all_plotlvl, aes(x = ID, y = plotlevel_vegcover, fill = graz)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(labels = c("Ungrazed", "Low grazing", "Medium grazing", "High grazing"), 
                    values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  theme_classic() +
  ylab("Perennial vegetation cover (%)") +
  xlab("Aridity") +
  scale_x_discrete(limits = all_plotlvl[order(all_plotlvl$mean_aridity_of_site, all_plotlvl$graz), "ID"], 
                   labels = c("85" = "0.50", "84" = "", "83" = "", 
                              "296" = "0.64", "295" = "", "294" = "", "293" = "",
                              "158" = "0.67", "157" = "", "156" = "", "155" = "", 
                              "274" = "0.75", "273" = "", "272" = "",
                              "103" = "0.75", "102" = "", "101" = "", "100" = "", 
                              "143" = "0.76", "142" = "", "141" = "", "140" = "", 
                              "50" = "0.76", "49" = "", "48" = "",
                              "99" = "0.77", "98" = "", "97" = "", 
                              "154" = "0.79", "153" = "", "152" = "", "151" = "",
                              "299" = "0.79", "298" = "", "297" = "", 
                              "146" = "0.80", "145" = "", "144" = "", 
                              "201" = "0.80", "200" = "", "199" = "", 
                              "47" = "0.81", "46" = "", "45" = "", "44" = "", 
                              "96" = "0.81", "95" = "", "94" = "", "93" = "", 
                              "139" = "0.82", "138" = "", "137" = "", 
                              "18" = "0.82", "17" = "", "16" = "", 
                              "136" = "0.83", "135" = "", "134" = "", 
                              "3" = "0.83", "2" = "", "1" = "", 
                              "198" = "0.84", "197" = "", "196" = "", 
                              "252" = "0.85", "251" = "", "250" = "", 
                              "292" = "0.85", "291" = "", "290" = "", "289" = "", 
                              "21" = "0.87", "20" = "", "19" = "", 
                              "150" = "0.87", "149" = "", "148" = "0.88", "147" = "", 
                              "249" = "0.88", "248" = "", "247" = "", 
                              "162" = "0.89", "161" = "", "160" = "", "159" = "", 
                              "195" = "0.90", "194" = "", "193" = "", 
                              "43" = "0.90", "42" = "", "41" = "", "40" = "", 
                              "216" = "0.91", "215" = "", "214" = "", 
                              "115" = "0.94", "114" = "")) +
  theme(axis.text.x = element_text(angle = 90), legend.title = element_blank(), legend.position = "bottom")
bygraz_cov_barplot
#remember there will be missing bars if they did not note cover for all species encountered
ggsave("vegcover_barplot_bygraz.png", bygraz_cov_barplot, width = 1950, height = 1000, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")


#Arrange the cover and sprich bygraz on the same plot
combo_barplot <- ggarrange(bygraz_sprich_barplot, bygraz_cov_barplot, ncol = 1, nrow = 2, labels = "auto", 
                           label.x = 0.1, label.y = 1)
combo_barplot
ggsave("combo_barplot.png", combo_barplot, width = 1500, height = 2100, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")


###Heatmap####
###Make a heatmap of NIntc richness, with the sites on the one axis (arranged from least to most arid) and the graz on the other
#First, we have to get the average NIntc per plot in a site:

#the sites must be ordered according to increasing aridity
result_sort <- all_result[order(all_result$aridity),]
sitelist <- c(unique(result_sort$site_ID))
grazlevels <- c("0" , "1", "2", "3")

##Average NIntc Richness
#matrix that will hold the average NIntc richness per plot
avg_rich_mat <- matrix(nrow = length(sitelist), ncol = 4, dimnames = list(sitelist , c("graz0", "graz1", "graz2", "graz3")))
for (n in 1:length(sitelist)) {
  
  #select only one site
  sub <- result_sort[which(result_sort$site_ID == sitelist[n]) , ]
  
  #select only one grazing level in a site
  for (t in 1:length(grazlevels)) {
  
    sub2 <- sub[which(sub$graz == grazlevels[t]) , ]
    sub3 <- sub2[which(!is.na(sub2$NIntc_richness)) , ] #remove rows with NA in NIntc richness
    avg_NIntc <- mean(sub3$NIntc_richness)
    
    #add the average Nintc to row n and column t of the matrix
    avg_rich_mat[n,t] <- avg_NIntc
  }
}#end of loop


##Average NINtc shannon
#matrix that will hold the average NIntc richness per plot
avg_shan_mat <- matrix(nrow = length(sitelist), ncol = 4, dimnames = list(sitelist , c("graz0", "graz1", "graz2", "graz3")))
for (n in 1:length(sitelist)) {
  
  #select only one site
  sub <- result_sort[which(result_sort$site_ID == sitelist[n]) , ]
  
  #select only one grazing level in a site
  for (t in 1:length(grazlevels)) {
    
    sub2 <- sub[which(sub$graz == grazlevels[t]) , ]
    sub3 <- sub2[which(!is.na(sub2$NIntc_shannon)) , ] #remove rows with NA in NIntc shannon
    avg_NIntc <- mean(sub3$NIntc_shannon)
    
    #add the average Nintc to row n and column t of the matrix
    avg_shan_mat[n,t] <- avg_NIntc
  }
}#end of loop


##Average NIntc cover
#matrix that will hold the average NIntc richness per plot
avg_cov_mat <- matrix(nrow = length(sitelist), ncol = 4, dimnames = list(sitelist , c("graz0", "graz1", "graz2", "graz3")))
for (n in 1:length(sitelist)) {
  
  #select only one site
  sub <- result_sort[which(result_sort$site_ID == sitelist[n]) , ]
  
  #select only one grazing level in a site
  for (t in 1:length(grazlevels)) {
    
    sub2 <- sub[which(sub$graz == grazlevels[t]) , ]
    sub3 <- sub2[which(!is.na(sub2$NIntc_cover)) , ] #remove rows with NA in NIntc cover
    avg_NIntc <- mean(sub3$NIntc_cover)
    
    #add the average Nintc to row n and column t of the matrix
    avg_cov_mat[n,t] <- avg_NIntc
  }
}#end of loop

#Make and export the heatmap of NINtc Richness
setwd("C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")
jpeg(height = 20, width = 35, units = "cm", res = 300, "rich_heatmap.jpg")
par(oma = c(8, 12, .1, .1))
#pick a color pallete for the heatmap
heat_pal <- brewer.pal(n = 4, name = "RdYlBu")
heat_pal2 <- brewer.pal(n = 5, name = "YlOrRd")[2:5]
#Get the average aridity corrseponding to each site, this will be the rowlabels
aridlist <- tapply(result_sort$aridity , result_sort$site_ID, mean)
rowlabels <- format(round(sort(aridlist), 3), nsmall = 3) #reduce decimal places and sort according to increasing aridity

#Now make the heatmap
heatmap(avg_rich_mat, col = heat_pal2, breaks = c(-1, -0.5, 0, 0.5, 1),
        Colv = NA, Rowv = NA,   #Do not reorder rows and columns
        scale = "none",         #Do not normalise NIntc values
        ylab = "Aridity of site", cex = 2,
        labCol = c("ungrazed", "low grazing", "medium grazing", "high grazing"),
        labRow = rowlabels,
        cexCol = 1.5, cexRow = 1.5 #change axis label font size
        )         

legend("bottomright", legend = c("-1 < NIntc < -0.5", "-0.5 < NIntc < 0", 
                                "0 < NIntc < 0.5", "0.5 < NIntc < 1"), 
       fill = heat_pal2, box.lty = 0, cex = 1.3, xjust = 1)

dev.off()


#Make and export the heatmap of NINtc cover
setwd("C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")
jpeg(height = 20, width = 35, units = "cm", res = 300, "cov_heatmap.jpg")
par(oma = c(8, 12, .1, .1))
#pick a color pallete for the heatmap
heat_pal <- brewer.pal(n = 4, name = "RdYlBu")
heat_pal2 <- brewer.pal(n = 5, name = "YlOrRd")[2:5]
#Get the average aridity corrseponding to each site, this will be the rowlabels
aridlist <- tapply(result_sort$aridity , result_sort$site_ID, mean)
rowlabels <- format(round(sort(aridlist), 3), nsmall = 3) #reduce decimal places and sort according to increasing aridity

#Now make the heatmap
heatmap(avg_cov_mat, col = heat_pal2, breaks = c(-1, -0.5, 0, 0.5, 1),
        Colv = NA, Rowv = NA,   #Do not reorder rows and columns
        scale = "none",         #Do not normalise NIntc values
        ylab = "Aridity of site", cex = 2,
        labCol = c("ungrazed", "low grazing", "medium grazing", "high grazing"),
        labRow = rowlabels,
        cexCol = 1.5, cexRow = 1.5 #change axis label font size
)         

legend("bottomright", legend = c("-1 < NIntc < -0.5", "-0.5 < NIntc < 0", 
                                 "0 < NIntc < 0.5", "0.5 < NIntc < 1"), 
       fill = heat_pal2, box.lty = 0, cex = 1.3, xjust = 1)

dev.off()


###Species position along aridity####
###AT THE PLOTLEVEL
#import the plotlevel species preference data that was made in the interaction_gradient models script
plot_sp_pref <- read.csv("Facilitation data\\results\\plotlevels_sp_preference_6Feb2024.csv", row.names = 1)
#make it long format
long_plot_sp_pref <- gather(plot_sp_pref, key = preference, #name of the new column that will be made up of the last 3 columns
                             value = proportion_of_sp, #name of the new column that will hold the proportions
                             prop_bare_only:prop_both, #where to find the values to put in the proportion)of_sp column
                             factor_key=TRUE) #make the key column a factor

#multiply the proportion by 100 to make them percentages
long_plot_sp_pref$proportion_of_sp <- long_plot_sp_pref$proportion_of_sp*100

#we will order plots according to the mean aridity of the site, and then by graz
#get the mean aridity at each site
site_arid <- data.frame(mean_aridity_of_site = tapply(long_plot_sp_pref$aridity, long_plot_sp_pref$site_ID, FUN = mean))
site_arid$site_ID <- rownames(site_arid)
rownames(site_arid) <- 1:nrow(site_arid)
##merge the mean aridity of each site with all_plotlvl
long_plot_sp_pref <- merge(long_plot_sp_pref, site_arid, by = "site_ID")
##define the order of grazing levels
long_plot_sp_pref$graz <- factor(long_plot_sp_pref$graz, levels = c("0", "1", "2", "3"))
long_plot_sp_pref$ID <- as.factor(long_plot_sp_pref$ID)
long_plot_sp_pref$preference <- factor(long_plot_sp_pref$preference, levels = c("prop_bare_only", "prop_both", "prop_nurse_only"))

pal <- brewer.pal(8, "Dark2")[c(7,8,1)]

plotlevel_prefbar <- ggplot(long_plot_sp_pref, aes(x = ID, y = proportion_of_sp)) + 
  geom_bar(aes(fill = preference), position = position_stack(), stat = "identity") +
  scale_fill_manual(values = c(brewer.pal(8, "Dark2")[7], brewer.pal(8,"Pastel2")[8], brewer.pal(8, "Dark2")[1]), 
                    labels = c("Bare microsite only", "Both","Dominant microsite only")) +
  theme_classic() +
  ylab("Percentage of species occurring in a microsite") +
  xlab("Aridity of plot") +
  scale_x_discrete(limits = long_plot_sp_pref[order(long_plot_sp_pref$mean_aridity_of_site, long_plot_sp_pref$graz), "ID"], #first order by mean aridity then by graz
                   labels = c("85" = "0.50", "84" = "", "83" = "", 
                              "296" = "0.64", "295" = "", "294" = "", "293" = "",
                              "158" = "0.67", "157" = "", "156" = "", "155" = "", 
                              "274" = "0.75", "273" = "", "272" = "",
                              "103" = "0.75", "102" = "", "101" = "", "100" = "", 
                              "143" = "0.76", "142" = "", "141" = "", "140" = "", 
                              "50" = "0.76", "49" = "", "48" = "",
                              "99" = "0.77", "98" = "", "97" = "", 
                              "154" = "0.79", "153" = "", "152" = "", "151" = "",
                              "299" = "0.79", "298" = "", "297" = "", 
                              "146" = "0.80", "145" = "", "144" = "", 
                              "201" = "0.80", "200" = "", "199" = "", 
                              "47" = "0.81", "46" = "", "45" = "", "44" = "", 
                              "96" = "0.81", "95" = "", "94" = "", "93" = "", 
                              "139" = "0.82", "138" = "", "137" = "", 
                              "18" = "0.82", "17" = "", "16" = "", 
                              "136" = "0.83", "135" = "", "134" = "", 
                              "3" = "0.83", "2" = "", "1" = "", 
                              "198" = "0.84", "197" = "", "196" = "", 
                              "252" = "0.85", "251" = "", "250" = "", 
                              "292" = "0.85", "291" = "", "290" = "", "289" = "", 
                              "21" = "0.87", "20" = "", "19" = "", 
                              "150" = "0.87", "149" = "", "148" = "", "147" = "", 
                              "249" = "0.88", "248" = "", "247" = "", 
                              "162" = "0.89", "161" = "", "160" = "", "159" = "", 
                              "195" = "0.90", "194" = "", "193" = "", 
                              "43" = "0.90", "42" = "", "41" = "", "40" = "", 
                              "216" = "0.91", "215" = "", "214" = "", 
                              "115" = "0.94", "114" = "")) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "right", legend.title = element_blank(), axis.text.x = element_text(angle = 90))
plotlevel_prefbar




####Graphs of Chisq results####
chisq_results <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data\\results\\Chisq_results_27Sep.csv", row.names = 1)

grey <- brewer.pal(9, "Set1")[9]
blue <- brewer.pal(10, "Set3")[5] 
brown <- brewer.pal(8, "Dark2")[7]
green <- brewer.pal(8, "Dark2")[1]


##WITHOUT RARE SPECIES, ORDER BY MEAN ARIDITY OF SITE, THEN BY GRAZ
#remove the rare species
chisq_reduced <- chisq_results[-which(chisq_results$association == "too_rare") , ]

#recalculate proportions, without taking the rares into account
prop_chisq_reduced <- chisq_reduced %>%
  group_by(ID, association) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(ID) %>%
  mutate(Proportion = Count / sum(Count))
prop_chisq_reduced <- as.data.frame(prop_chisq_reduced)
prop_chisq_reduced$percentage <- prop_chisq_reduced$Proportion*100

#Add aridity and site ID to prop_chisq
ymerge <- chisq_results[-which(duplicated(chisq_results$ID) == TRUE) , which(colnames(chisq_results) %in% c("site_ID", "ID", "graz", "aridity"))]
prop_chisq_reduced <- merge(prop_chisq_reduced, ymerge, 
                    by = "ID", all.x = FALSE, all.y = FALSE, no.dups = TRUE)
prop_chisq_reduced$site_ID <- as.factor(prop_chisq_reduced$site_ID)

#calculate the mean aridity of each site
site_arid <- data.frame(mean_aridity_of_site = tapply(prop_chisq_reduced$aridity, prop_chisq_reduced$site_ID, FUN = mean))
site_arid$site_ID <- rownames(site_arid)
rownames(site_arid) <- 1:nrow(site_arid)
##merge the mean aridity of each site with all_plotlvl
prop_chisq_reduced <- merge(prop_chisq_reduced, site_arid, by = "site_ID")
##define the order of grazing levels
prop_chisq_reduced$graz <- factor(prop_chisq_reduced$graz, levels = c("0", "1", "2", "3"))
prop_chisq_reduced$ID <- as.factor(prop_chisq_reduced$ID)
prop_chisq_reduced$site_ID <- as.factor(prop_chisq_reduced$site_ID)

##which plots had only rare species in them?
lost_plots <- filter(prop_chisq, association == "too_rare" & Proportion == 1)
lost_plots$ID #134 147 155 156 157 158 159 162 44  47


###specify the order of the plots manually(following plotlevel_prefbar), because ordering by site aridity, then graz doesnt work
prop_chisq_bar <- prop_chisq_reduced %>%
  mutate(ID = fct_relevel(ID, 
                            "85","84","83","296","295","294","293",
                            "274","273","272","103","102","101","100",
                            "143", "142","141", "140", "50","49", "48", "99", "98",  
                            "97","154","153","152", "151","299","298","297","146",
                            "145","144","201","200","199","46","45", 
                            "96","95","94","93","139", "138","137","18","17", 
                            "16","136","135","3","2","1","198","197",
                            "196","252","251","250","292","291","290","289","21", 
                            "20","19","150","149","148","249","248","247",
                            "161","160","195","194","193","43","42", 
                            "41","40","216","215","214","115","114")) %>%
  ggplot(aes(x = ID, y = percentage)) + 
  geom_bar(aes(fill = association), position = position_stack(), stat = "identity", width = 0.9) +
  scale_fill_manual(values = c(brown, blue, green) , 
                    labels = c("Associated with bare microsites", "Neutral", "Associated with dominant microsites")) +
  scale_x_discrete(labels = c("85" = "0.50", "84" = "", "83" = "", 
                              "296" = "0.64", "295" = "", "294" = "", "293" = "",
                              "274" = "0.75", "273" = "", "272" = "",
                              "103" = "0.75", "102" = "", "101" = "", "100" = "", 
                              "143" = "0.76", "142" = "", "141" = "", "140" = "", 
                              "50" = "0.76", "49" = "", "48" = "",
                              "99" = "0.77", "98" = "", "97" = "", 
                              "154" = "0.79", "153" = "", "152" = "", "151" = "",
                              "299" = "0.79", "298" = "", "297" = "", 
                              "146" = "0.80", "145" = "", "144" = "", 
                              "201" = "0.80", "200" = "", "199" = "", 
                              "46" = "0.81", "45" = "", 
                              "96" = "0.81", "95" = "", "94" = "", "93" = "", 
                              "139" = "0.82", "138" = "", "137" = "", 
                              "18" = "0.82", "17" = "", "16" = "", 
                              "136" = "0.83", "135" = "", 
                              "3" = "0.83", "2" = "", "1" = "", 
                              "198" = "0.84", "197" = "", "196" = "", 
                              "252" = "0.85", "251" = "", "250" = "", 
                              "292" = "0.85", "291" = "", "290" = "", "289" = "", 
                              "21" = "0.87", "20" = "", "19" = "", 
                              "150" = "0.87", "149" = "", "148" = "", 
                              "249" = "0.88", "248" = "", "247" = "", 
                              "161" = "0.89", "160" = "", 
                              "195" = "0.90", "194" = "", "193" = "", 
                              "43" = "0.90", "42" = "", "41" = "", "40" = "", 
                              "216" = "0.91", "215" = "", "214" = "", 
                              "115" = "0.94", "114" = "")) +
  theme_classic() +
  ylab("Percentage of species showing association") +
  xlab("Aridity of plot") +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "right", legend.title = element_blank(), axis.text.x = element_text(angle = 90))
prop_chisq_bar



###Combine prop_chisq and plotlevel_prefbar in one figure
chisq_combo_pref <- ggarrange(prop_chisq_bar, plotlevel_prefbar, ncol = 1, nrow = 2, labels = c("a", "b"), 
                              hjust = -50)
ggsave("chisq_preference_bar_combo.png",chisq_combo_pref, width = 2000, height = 2200, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")




##ORDERED ONLY BY ARIDITY
#order ID by aridity again
prop_chisq_reduced$ID <- with(prop_chisq_reduced, reorder(ID, aridity, mean))

reduced_prop_chisq_bar <- prop_chisq_reduced %>% 
  ggplot(aes(x = ID, y = percentage)) + 
  geom_bar(aes(fill = association), position = position_stack(), stat = "identity") +
  scale_fill_manual(values = c(brown, blue, green) , 
                    labels = c("Associated with bare microsites", "Neutral", "Associated with dominant microsites")) +
  theme_classic() +
  ylab("Percentage of species") +
  xlab("Aridity of plot") +
  scale_x_discrete(labels = c("83" = "0.50", "84" = "", "85" = "", 
                              "295" = "0.63", "293" = "", "296" = "", 
                              "294" = "",
                              "274" = "0.75", "103" = "", "273" = "", "102" = "", "100" = "", "101" = "", "272" = "",
                              "140" = "0.76", "141" = "", "142" = "", "48" = "", "50" = "", "143" = "", "49" = "",
                              "97" = "0.77", "98" = "", "99" = "", 
                              "153" = "",
                              "299" = "0.79", "298" = "", "152" = "", "151" = "", 
                              "145" = "0.80", "146" = "", "154" = "", "144" = "", "199" = "", "200" = "", "201" = "", 
                              "93" = "0.81", "46" = "", "297" = "", "95" = "", "45" = "", "94" = "", "96" = "", 
                              "137" = "0.82", "138" = "", "139" = "", "16" = "", "17" = "", "18" = "", 
                              "135" = "0.83", "136" = "", "1" = "", "2" = "", "3" = "", 
                              "196" = "",
                              "197" = "0.85", "198" = "", "250" = "", "251" = "", "290" = "", "291" = "", "292" = "", "252" = "", 
                              "289" = "", 
                              "21" = "0.87", "19" = "", "20" = "", "149" = "", "150" = "", 
                              "148" = "0.88", "248" = "", "247" = "", "249" = "", 
                              "160" = "0.89", "161" = "", 
                              "193" = "0.90", "194" = "", "195" = "", "42" = "", "41" = "", "40" = "", "43" = "", 
                              "214" = "0.91", "215" = "", "216" = "", 
                              "115" = "0.94", "114" = "")) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 90))
reduced_prop_chisq_bar
ggsave("prop_chisq_bar_without_rares.png", reduced_prop_chisq_bar, width = 1700, height = 1300, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")







###Some trash plot code that may be useful later####

#Counts of Chisq association categories, with rares
count_chisq_bar <- chisq_results %>% 
  ggplot(aes(x = ID)) + 
  geom_bar(aes(fill = association), position = position_stack(), stat = "count") +
  scale_fill_manual(values = c(brown, blue, green, grey) , 
                    labels = c("Associated with bare microsites", "Neutral", "Associated with dominant microsites", "Rare")) +
  theme_classic() +
  ylab("Number of species") +
  xlab("Aridity of plot") +
  scale_x_discrete(labels = c("83" = "0.50", "84" = "", "85" = "", 
                              "295" = "0.63", "293" = "", "296" = "", 
                              "155" = "0.66", "156" = "", 
                              "294" = "0.67", "157" = "", "158" = "",
                              "274" = "0.75", "103" = "", "273" = "", "102" = "", "100" = "", "101" = "", "272" = "",
                              "140" = "0.76", "141" = "", "142" = "", "48" = "", "50" = "", "143" = "", "49" = "",
                              "97" = "0.77", "98" = "", "99" = "", 
                              "153" = "",
                              "299" = "0.79", "298" = "", "152" = "", "151" = "", 
                              "145" = "0.80", "146" = "", "154" = "", "144" = "", "199" = "", "200" = "", "201" = "", 
                              "93" = "0.81", "44" = "", "47" = "", "46" = "", "297" = "", "95" = "", "45" = "", "94" = "", "96" = "", 
                              "137" = "0.82", "138" = "", "139" = "", "16" = "", "17" = "", "18" = "", 
                              "134" = "0.83", "135" = "", "136" = "", "1" = "", "2" = "", "3" = "", 
                              "196" = "",
                              "197" = "0.85", "198" = "", "250" = "", "251" = "", "290" = "", "291" = "", "292" = "", "252" = "", 
                              "289" = "", 
                              "21" = "0.87", "19" = "", "20" = "", "149" = "", "150" = "", 
                              "148" = "0.88", "248" = "", "247" = "", "249" = "", 
                              "147" = "0.89", "159" = "", "160" = "", "161" = "", "162" = "", 
                              "193" = "0.90", "194" = "", "195" = "", "42" = "", "41" = "", "40" = "", "43" = "", 
                              "214" = "0.91", "215" = "", "216" = "", 
                              "115" = "0.94", "114" = "")) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 90))
count_chisq_bar
ggsave("count_chisq_bar_with_rares.png", count_chisq_bar, width = 1700, height = 1300, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")


###Now make a graph showing the proportion of sp in each category of association
#First calculate the proportion of sp in each plot in each category of association 
prop_chisq <- chisq_results %>%
  group_by(ID, association) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(ID) %>%
  mutate(Proportion = Count / sum(Count))
prop_chisq <- as.data.frame(prop_chisq)
prop_chisq$percentage <- prop_chisq$Proportion*100
#Add aridity to prop_chisq
ymerge <- chisq_results[-which(duplicated(chisq_results$ID) == TRUE) , which(colnames(chisq_results) %in% c("ID", "aridity"))]
prop_chisq <- merge(prop_chisq, ymerge, 
                    by = "ID", all.x = FALSE, all.y = FALSE, no.dups = TRUE)

#sort ID by aridity
prop_chisq$ID <- with(prop_chisq, reorder(ID, aridity, mean))

prop_chisq_bar <- prop_chisq %>% 
  ggplot(aes(x = ID, y = percentage)) + 
  geom_bar(aes(fill = association), position = position_stack(), stat = "identity") +
  scale_fill_manual(values = c(brown, blue, green, grey) , 
                    labels = c("Associated with bare microsites", "Neutral", "Associated with dominant microsites", "Rare")) +
  theme_classic() +
  ylab("Percentage of species of species") +
  xlab("Aridity of plot") +
  scale_x_discrete(labels = c("83" = "0.50", "84" = "", "85" = "", 
                              "295" = "0.63", "293" = "", "296" = "", 
                              "155" = "0.66", "156" = "", 
                              "294" = "0.67", "157" = "", "158" = "",
                              "274" = "0.75", "103" = "", "273" = "", "102" = "", "100" = "", "101" = "", "272" = "",
                              "140" = "0.76", "141" = "", "142" = "", "48" = "", "50" = "", "143" = "", "49" = "",
                              "97" = "0.77", "98" = "", "99" = "", 
                              "153" = "",
                              "299" = "0.79", "298" = "", "152" = "", "151" = "", 
                              "145" = "0.80", "146" = "", "154" = "", "144" = "", "199" = "", "200" = "", "201" = "", 
                              "93" = "0.81", "44" = "", "47" = "", "46" = "", "297" = "", "95" = "", "45" = "", "94" = "", "96" = "", 
                              "137" = "0.82", "138" = "", "139" = "", "16" = "", "17" = "", "18" = "", 
                              "134" = "0.83", "135" = "", "136" = "", "1" = "", "2" = "", "3" = "", 
                              "196" = "",
                              "197" = "0.85", "198" = "", "250" = "", "251" = "", "290" = "", "291" = "", "292" = "", "252" = "", 
                              "289" = "", 
                              "21" = "0.87", "19" = "", "20" = "", "149" = "", "150" = "", 
                              "148" = "0.88", "248" = "", "247" = "", "249" = "", 
                              "147" = "0.89", "159" = "", "160" = "", "161" = "", "162" = "", 
                              "193" = "0.90", "194" = "", "195" = "", "42" = "", "41" = "", "40" = "", "43" = "", 
                              "214" = "0.91", "215" = "", "216" = "", 
                              "115" = "0.94", "114" = "")) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 90))
prop_chisq_bar
ggsave("prop_chisq_bar_with_rares.png", prop_chisq_bar, width = 1700, height = 1300, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")



##plot species richness in plots ordered according to aridity, colour by country
#order ID according to aridity
all_plotlvl$ID = with(all_plotlvl, reorder(ID, aridity, mean))

pal1 <- brewer.pal(8, "Dark2")
pal2 <- brewer.pal(3, "Set1")[1:2]
barpal <- c(pal1, pal2)

sprich_barplot <- all_plotlvl %>%
  ggplot(aes(x = ID, y = plotlevel_sprichness, fill = country)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = barpal) +
  theme_classic() +
  ylab("Species richness") +
  xlab("Aridity of plot") +
  scale_x_discrete(labels = c("83" = "0.50", "84" = "", "85" = "", 
                              "295" = "0.63", "293" = "", "296" = "", 
                              "155" = "0.66", "156" = "", 
                              "294" = "0.67", "157" = "", "158" = "",
                              "274" = "0.75", "103" = "", "273" = "", "102" = "", "100" = "", "101" = "", "272" = "",
                              "140" = "0.76", "141" = "", "142" = "", "48" = "", "50" = "", "143" = "", "49" = "",
                              "97" = "0.77", "98" = "", "99" = "", 
                              "153" = "",
                              "299" = "0.79", "298" = "", "152" = "", "151" = "", 
                              "145" = "0.80", "146" = "", "154" = "", "144" = "", "199" = "", "200" = "", "201" = "", 
                              "93" = "0.81", "44" = "", "47" = "", "46" = "", "297" = "", "95" = "", "45" = "", "94" = "", "96" = "", 
                              "137" = "0.82", "138" = "", "139" = "", "16" = "", "17" = "", "18" = "", 
                              "134" = "0.83", "135" = "", "136" = "", "1" = "", "2" = "", "3" = "", 
                              "196" = "",
                              "197" = "0.85", "198" = "", "250" = "", "251" = "", "290" = "", "291" = "", "292" = "", "252" = "", 
                              "289" = "", 
                              "21" = "0.87", "19" = "", "20" = "", "149" = "", "150" = "", 
                              "148" = "0.88", "248" = "", "247" = "", "249" = "", 
                              "147" = "0.89", "159" = "", "160" = "", "161" = "", "162" = "", 
                              "193" = "0.90", "194" = "", "195" = "", "42" = "", "41" = "", "40" = "", "43" = "", 
                              "214" = "0.91", "215" = "", "216" = "", 
                              "115" = "0.94", "114" = "")) +
  theme(axis.text.x = element_text(angle = 90), legend.title = element_blank(), legend.position = "bottom")
sprich_barplot
ggsave("sprichness_barplot_bycountry.png", sprich_barplot, width = 1950, height = 1000, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Methods and results\\Figures")
###Graph the NIntc per replicate along the aridity gradient###
#using color for different grazing intensities
NIntc_richness_scatter1 <- 
  ggplot(all_result, aes(x = aridity, y = NIntc_richness, label = graz, color = as.factor(graz))) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  theme_classic() +
  xlab("Aridity") +
  ylab("NIntc richness per replicate") +
  labs(color = "grazing pressure") 
NIntc_richness_scatter1

#using transparency for different grazing intensities
NIntc_richness_scatter2 <- 
  ggplot(all_result, aes(x = aridity, y = NIntc_richness, label = graz, alpha = as.factor(graz))) + 
  geom_point(size = 6, color = "darkgreen") +
  theme_classic() +
  xlab("Aridity") +
  ylab("NIntc richness per replicate") +
  labs(alpha = "grazing pressure")
NIntc_richness_scatter2

p <- 
  ggplot(all_result, aes(x = aridity, y = graz, color = NIntc_richness)) + 
  geom_point(size = 3, position = "jitter") +
  theme_classic() +
  xlab("Aridity") +
  ylab("grazing pressure")
p
#look at geom jitter in ggplot

###hexagonal binning - density plot
q <- ggplot(all_result, aes(x = aridity, y = NIntc_richness)) +
  geom_hex(bins = 15, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07")+
  theme_classic()
q




###Graph NIntc over levels of grazing###
NIntc_richness_box <- 
  ggplot(all_result, aes(x = graz, y = NIntc_richness)) +
  geom_boxplot()
NIntc_richness_box  



####MOrgan's plots###
f = ggplot(data = all_result, aes(x = aridity,  y = NIntc_richness)) +  
  geom_point(aes(shape= graz, color = graz), size=4) +  
  geom_smooth(aes(group =graz, colour =graz, size =20 ), method = "lm", se = F, size =2) +
  theme_classic()

f +font("xlab", size = 14, color = "black")+ font("ylab", size = 14, color = "black")+theme(text = element_text(size=12)) 
f + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#Could alos bin aridity and plot like in raath-kruger et al

###Bin aridity, make 4 bins
#The two lowest aridity values are its own bin, because there is a big gap between the second lowest aridity and the third lowest aridity 
sort(unique(all_result$aridity))
#the upper limit of the next bins are created by adding mult to the third lowest ardidty value
mult <- (0.9014 - 0.7474)/3

forplot <- all_result %>% mutate(arid_bin = cut(aridity, breaks = c(0, 0.6720, (0.7474 + mult), (0.7474 + 2*mult), (0.7474 + 3*mult)))) 
#remove rows with NA in NIntc_richness
forplot <- forplot[-which(is.na(forplot$NIntc_richness)) , ]

binplota <- ggline(forplot, y = "NIntc_richness", x = "arid_bin",  add = c("mean_se"), color = "graz", size = 1) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  theme_classic() +
  xlab("Aridity") +
  ylab("Mean of NIntc richness") +
  labs(color = "Grazing pressure")
binplota

##OR##

##Calculate the mean NIntc for each combination of graz and aridity bin
#graz = 0 
graz0 <- forplot[which(forplot$graz == "0") , ]

graz0_mean <- data.frame(matrix(nrow = 4, ncol = 3)) #table for the mean NIntc values
colnames(graz0_mean) = c("arid_bin" , "graz" , "mean_NIntc_richness")
graz0_mean$graz = "0"
graz0_mean$arid_bin <- sort(c(unique(graz0$arid_bin)))
#now get the mean NIntc per bin
graz0_mean$mean_NIntc_richness <- tapply(graz0$NIntc_richness, graz0$arid_bin, mean)

#graz1
graz1 <- forplot[which(forplot$graz == "1") , ]
graz1_mean <- data.frame(matrix(nrow = 4, ncol = 3)) #table for the mean NIntc values
colnames(graz1_mean) = c("arid_bin" , "graz" , "mean_NIntc_richness")
graz1_mean$graz = "1"
graz1_mean$arid_bin <- sort(c(unique(graz1$arid_bin)))
#now get the mean NIntc per bin
graz1_mean$mean_NIntc_richness <- tapply(graz1$NIntc_richness, graz1$arid_bin, mean)

#graz2
graz2 <- forplot[which(forplot$graz == "2") , ]
graz2_mean <- data.frame(matrix(nrow = 4, ncol = 3)) #table for the mean NIntc values
colnames(graz2_mean) = c("arid_bin" , "graz" , "mean_NIntc_richness")
graz2_mean$graz = 2
graz2_mean$arid_bin <- sort(c(unique(graz2$arid_bin)))
#now get the mean NIntc per bin
graz2_mean$mean_NIntc_richness <- tapply(graz2$NIntc_richness, graz2$arid_bin, mean)

#graz3
graz3 <- forplot[which(forplot$graz == "3") , ]
graz3_mean <- data.frame(matrix(nrow = 4, ncol = 3)) #table for the mean NIntc values
colnames(graz3_mean) = c("arid_bin" , "graz" , "mean_NIntc_richness")
graz3_mean$graz = "3"
graz3_mean$arid_bin <- sort(c(unique(graz3$arid_bin)))
#now get the mean NIntc per bin
graz3_mean$mean_NIntc_richness <- tapply(graz3$NIntc_richness, graz3$arid_bin, mean)

#Join all the tables to graph it
bindat <- rbind(graz0_mean, graz1_mean, graz2_mean, graz3_mean)

binplotb <- ggplot(bindat, aes(x = arid_bin, y = mean_NIntc_richness, group = graz, color = graz)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) +
  theme_classic() +
  xlab("Aridity") +
  ylab("Mean of NIntc richness") +
  labs(color = "Grazing pressure")
binplotb


###Graph the average NIntc per plot along the aridity gradient, joining plots at the same site with lines###
##First we must get the average NIntc per plot
all_result$ID <- as.factor(all_result$ID)
all_avg_perplot <- tapply(all_result$NIntc_richness, INDEX = all_result$ID, FUN = mean) #NaN is returned if there is an NA value in NIntc_richness in the plot
#Now create a dataframe with the mean NIntc per plot and the corresponding grazing and aridity
all_avg_perplot <- data.frame(all_avg_perplot) #make it a dataframe
all_avg_perplot$ID <- rownames(all_avg_perplot) #the rownames correspond to the plot ID
colnames(all_avg_perplot) <- c("mean_NIntc", "ID") #rename the columns
#Add site, plot, grazing and aridity
red <- all_result[, 1:6] #we only want to merge the variables in columns 1-6
red$ID <- as.factor(red$ID)
all_avg_perplot$ID <- as.factor(all_avg_perplot$ID)
merged <- merge(x = all_avg_perplot, y = red, by = "ID", all.x = FALSE, all.y = FALSE) #the join adds duplicate rows
#keep only the first row of each ID
#make a second ID column because otherwise the loop doesnt pick it up
merged$ID_duplicate <- as.character(merged$ID)
IDlist <- c(unique(merged$ID_duplicate)) #67 different plot ID's
NIntc_perplot <- data.frame(matrix(nrow = length(IDlist), ncol = 7))
colnames(NIntc_perplot) <- c("ID", "mean_NIntc", "country", "site_ID", "plot", "graz", "aridity")
l = 1
for (a in 1:length(IDlist)) {
  entry <- merged[which(merged$ID %in% IDlist[a])[1] ,]
  NIntc_perplot[l,1 ] <- entry$ID_duplicate
  NIntc_perplot[l,2 ] <- entry$mean_NIntc
  NIntc_perplot[l,3 ] <- entry$country
  NIntc_perplot[l,4 ] <- entry$site_ID
  NIntc_perplot[l,5 ] <- entry$plot
  NIntc_perplot[l,6 ] <- entry$graz
  NIntc_perplot[l,7 ] <- entry$aridity
  
  l= l+1
}
#NIntc_perplot has many NA values because there are replicates in plots for which NIntc richness is NA. 
#If there is one NA value in a plot the plot average will also be NA. Fix this later.

#plot site 42
site42 <- NIntc_perplot[which(NIntc_perplot$site_ID == 42), ]
site42_scatter <- 
  ggplot(site42, aes(x = aridity, y = mean_NIntc, label = graz, color = as.factor(graz))) + #make graz a factor here so that we can assign discrete 
  #colours to different grazing intensities
  geom_point(size = 3) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) + 
  geom_segment(
    aes(xend = c(tail(aridity, n = -1), NA), 
        yend = c(tail(mean_NIntc, n = -1), NA)
    )) +
  theme_classic() +
  xlab("Aridity") +
  ylab("mean NIntc per plot") 
site42_scatter

#plot site 42 and site 30
site42_30 <- NIntc_perplot[which(NIntc_perplot$site_ID %in% c(42, 30)), ]
site42_30_scatter <- 
  ggplot(site42_30, aes(x = aridity, y = mean_NIntc, label = graz, color = as.factor(graz))) + #make graz a factor here so that we can assign discrete 
  #colours to different grazing intensities
  geom_point(size = 3) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) + 
  geom_segment(
    aes(xend = c(tail(aridity, n = -1), NA), 
        yend = c(tail(mean_NIntc, n = -1), NA)
    )) +
  theme_classic() +
  xlab("Aridity") +
  ylab("mean NIntc per plot") 
site42_30_scatter



a <- 
  ggplot(NIntc_perplot, aes(x = aridity, y = mean_NIntc, label = graz, color = as.factor(graz))) + #make graz a factor here so that we can assign discrete 
  #colours to different grazing intensities
  geom_point(size = 3) +
  scale_color_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" )) + 
  geom_segment(
    aes(xend = c(tail(aridity, n = -1), NA), 
        yend = c(tail(mean_NIntc, n = -1), NA)
    )) +
  theme_classic() +
  xlab("Aridity") +
  ylab("mean NIntc per plot") 
a


#species richness according to aridity
#Two y axes
#order ID according to aridity
all_plotlvl$ID = with(all_plotlvl, reorder(ID, aridity, mean))

ylim.prim <- c(0, 100)   # axes limits of sprichness
ylim.sec <- c(0, 1)    # axis limits of aridity

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

colour_brown <- brewer.pal(8, "Dark2")[7]
dark2 <- brewer.pal(8, "Dark2")
set1 <- brewer.pal(3, "Set1")[1:2]
barpal <- c(dark2, set1)

dub_sprich_barplot <- all_plotlvl %>%
  ggplot(aes(x = ID)) + 
  geom_bar(aes(y = plotlevel_sprichness), stat = "identity", fill = colour_brown) +
  geom_point(aes(y = a+aridity*b),  color = "darkslategrey", size = 0.8) +
  scale_y_continuous(name = "Species richness per plot", sec.axis = sec_axis(trans = ~ (. - a)/b, name = "Aridity of each plot")) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  xlab(label = element_blank())
dub_sprich_barplot
ggsave("double_sprichness_barplot.png", dub_sprich_barplot, path = "C:\\Users\\user\\OneDrive\\Documents\\Msc Projek\\Methods and results\\Figures")


###Fig3: Species position along aridity####
###AT THE PLOTLEVEL
#import the plotlevel species preference data that was made in the interaction_gradient models script
plot_sp_pref <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\plotlevels_sp_preference_6Feb2024.csv", row.names = 1)
#make it long format
long_plot_sp_pref <- gather(plot_sp_pref, key = preference, #name of the new column that will be made up of the last 3 columns
                            value = proportion_of_sp, #name of the new column that will hold the proportions
                            prop_bare_only:prop_both, #where to find the values to put in the proportion)of_sp column
                            factor_key=TRUE) #make the key column a factor

#multiply the proportion by 100 to make them percentages
long_plot_sp_pref$proportion_of_sp <- long_plot_sp_pref$proportion_of_sp*100

#we will order plots according to the mean aridity of the site, and then by graz
#get the mean aridity at each site
site_arid <- data.frame(mean_aridity_of_site = tapply(long_plot_sp_pref$aridity, long_plot_sp_pref$site_ID, FUN = mean))
site_arid$site_ID <- rownames(site_arid)
rownames(site_arid) <- 1:nrow(site_arid)
##merge the mean aridity of each site with all_plotlvl
long_plot_sp_pref <- merge(long_plot_sp_pref, site_arid, by = "site_ID")
##define the order of grazing levels
long_plot_sp_pref$graz <- factor(long_plot_sp_pref$graz, levels = c("0", "1", "2", "3"))
long_plot_sp_pref$ID <- as.factor(long_plot_sp_pref$ID)

pal <- brewer.pal(8, "Dark2")[c(7,1,8)]

plotlevel_prefbar <- ggplot(long_plot_sp_pref, aes(x = ID, y = proportion_of_sp)) + 
  geom_bar(aes(fill = preference), position = position_stack(), stat = "identity") +
  scale_fill_manual(values = c(pal) , labels = c("Bare microsite only", "Dominant microsite only", "Both")) +
  theme_classic() +
  ylab("Percentage of species occurring in a microsite") +
  xlab("Aridity of plot") +
  scale_x_discrete(limits = long_plot_sp_pref[order(long_plot_sp_pref$mean_aridity_of_site, long_plot_sp_pref$graz), "ID"], #first order by mean aridity then by graz
                   labels = c("85" = "0.50", "84" = "", "83" = "", 
                              "296" = "0.64", "295" = "", "294" = "", "293" = "",
                              "158" = "0.67", "157" = "", "156" = "", "155" = "", 
                              "274" = "0.75", "273" = "", "272" = "",
                              "103" = "0.75", "102" = "", "101" = "", "100" = "", 
                              "143" = "0.76", "142" = "", "141" = "", "140" = "", 
                              "50" = "0.76", "49" = "", "48" = "",
                              "99" = "0.77", "98" = "", "97" = "", 
                              "154" = "0.79", "153" = "", "152" = "", "151" = "",
                              "299" = "0.79", "298" = "", "297" = "", 
                              "146" = "0.80", "145" = "", "144" = "", 
                              "201" = "0.80", "200" = "", "199" = "", 
                              "47" = "0.81", "46" = "", "45" = "", "44" = "", 
                              "96" = "0.81", "95" = "", "94" = "", "93" = "", 
                              "139" = "0.82", "138" = "", "137" = "", 
                              "18" = "0.82", "17" = "", "16" = "", 
                              "136" = "0.83", "135" = "", "134" = "", 
                              "3" = "0.83", "2" = "", "1" = "", 
                              "198" = "0.84", "197" = "", "196" = "", 
                              "252" = "0.85", "251" = "", "250" = "", 
                              "292" = "0.85", "291" = "", "290" = "", "289" = "", 
                              "21" = "0.87", "20" = "", "19" = "", 
                              "150" = "0.87", "149" = "", "148" = "", "147" = "", 
                              "249" = "0.88", "248" = "", "247" = "", 
                              "162" = "0.89", "161" = "", "160" = "", "159" = "", 
                              "195" = "0.90", "194" = "", "193" = "", 
                              "43" = "0.90", "42" = "", "41" = "", "40" = "", 
                              "216" = "0.91", "215" = "", "214" = "", 
                              "115" = "0.94", "114" = "")) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "right", legend.title = element_blank(), axis.text.x = element_text(angle = 90))
plotlevel_prefbar

###Fig3: Graph of Chisq results####
chisq_results <- read.csv("Facilitation data\\results\\Chisq_results_6Feb2024.csv", row.names = 1)
chisq_results$ID <- as.factor(chisq_results$ID)

grey <- brewer.pal(9, "Set1")[9]
blue <- brewer.pal(10, "Set3")[5] 
brown <- brewer.pal(8, "Dark2")[7]
green <- brewer.pal(8, "Dark2")[1]

##WITHOUT RARE SPECIES, ORDER BY MEAN ARIDITY OF SITE, THEN BY GRAZ
#recalculate proportions, without taking the rares into account
prop_chisq_reduced <- chisq_results |> 
  filter(!association == "too_rare") |> 
  group_by(ID, association) |> 
  mutate(Count = n()) |> 
  distinct(site_ID, ID, aridity, graz, association, Count) |> 
  ungroup() |> 
  group_by(ID) |> 
  mutate(Proportion = Count / sum(Count)) |> 
  mutate(percentage = Proportion*100)
prop_chisq_reduced <- as.data.frame(prop_chisq_reduced)

#calculate the mean aridity of each site
site_arid <- data.frame(mean_aridity_of_site = tapply(prop_chisq_reduced$aridity, prop_chisq_reduced$site_ID, FUN = mean))
site_arid$site_ID <- rownames(site_arid)
rownames(site_arid) <- 1:nrow(site_arid)
##merge the mean aridity of each site with all_plotlvl
prop_chisq_reduced <- merge(prop_chisq_reduced, site_arid, by = "site_ID")
##define the order of grazing levels
prop_chisq_reduced$graz <- factor(prop_chisq_reduced$graz, levels = c("0", "1", "2", "3"))
prop_chisq_reduced$ID <- as.factor(prop_chisq_reduced$ID)
prop_chisq_reduced$site_ID <- as.factor(prop_chisq_reduced$site_ID)

##which plots had only rare species in them?
lost_plots <- anti_join(chisq_results, prop_chisq_reduced, by = "ID") |> 
  distinct(ID)
#134 147 155 156 157 158 159 162 44  47


###specify the order of the plots manually(following plotlevel_prefbar), because ordering by site aridity, then graz doesnt work
prop_chisq_bar <- prop_chisq_reduced %>%
  mutate(ID = fct_relevel(ID, 
                          "85","84","83","296","295","294","293",
                          "274","273","272","103","102","101","100",
                          "143", "142","141", "140", "50","49", "48", "99", "98",  
                          "97","154","153","152", "151","299","298","297","146",
                          "145","144","201","200","199","46","45", 
                          "96","95","94","93","139", "138","137","18","17", 
                          "16","136","135","3","2","1","198","197",
                          "196","252","251","250","292","291","290","289","21", 
                          "20","19","150","149","148","249","248","247",
                          "161","160","195","194","193","43","42", 
                          "41","40","216","215","214","115","114")) %>%
  ggplot(aes(x = ID, y = percentage)) + 
  geom_bar(aes(fill = association), position = position_stack(), stat = "identity", width = 0.9) +
  scale_fill_manual(values = c(brown, blue, green) , 
                    labels = c("Associated with bare microsites", "Neutral", "Associated with dominant microsites")) +
  scale_x_discrete(labels = c("85" = "0.50", "84" = "", "83" = "", 
                              "296" = "0.64", "295" = "", "294" = "", "293" = "",
                              "274" = "0.75", "273" = "", "272" = "",
                              "103" = "0.75", "102" = "", "101" = "", "100" = "", 
                              "143" = "0.76", "142" = "", "141" = "", "140" = "", 
                              "50" = "0.76", "49" = "", "48" = "",
                              "99" = "0.77", "98" = "", "97" = "", 
                              "154" = "0.79", "153" = "", "152" = "", "151" = "",
                              "299" = "0.79", "298" = "", "297" = "", 
                              "146" = "0.80", "145" = "", "144" = "", 
                              "201" = "0.80", "200" = "", "199" = "", 
                              "46" = "0.81", "45" = "", 
                              "96" = "0.81", "95" = "", "94" = "", "93" = "", 
                              "139" = "0.82", "138" = "", "137" = "", 
                              "18" = "0.82", "17" = "", "16" = "", 
                              "136" = "0.83", "135" = "", 
                              "3" = "0.83", "2" = "", "1" = "", 
                              "198" = "0.84", "197" = "", "196" = "", 
                              "252" = "0.85", "251" = "", "250" = "", 
                              "292" = "0.85", "291" = "", "290" = "", "289" = "", 
                              "21" = "0.87", "20" = "", "19" = "", 
                              "150" = "0.87", "149" = "", "148" = "", 
                              "249" = "0.88", "248" = "", "247" = "", 
                              "161" = "0.89", "160" = "", 
                              "195" = "0.90", "194" = "", "193" = "", 
                              "43" = "0.90", "42" = "", "41" = "", "40" = "", 
                              "216" = "0.91", "215" = "", "214" = "", 
                              "115" = "0.94", "114" = "")) +
  theme_classic() +
  ylab("Percentage of species showing association") +
  xlab("Aridity of plot") +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "right", legend.title = element_blank(), axis.text.x = element_text(angle = 90))
prop_chisq_bar



###Combine prop_chisq and plotlevel_prefbar in one figure
chisq_combo_pref <- ggarrange(prop_chisq_bar, plotlevel_prefbar, ncol = 1, nrow = 2, labels = c("a", "b"), 
                              hjust = -50)
ggsave("chisq_preference_bar_combo.png",chisq_combo_pref, width = 2000, height = 2200, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Figures")