###CHAPTER 1 GRAPHS AND FIGURES###
library(data.table)
library(ggplot2)
library(hexbin)
library(RColorBrewer)
library(ggpubr)
library(forcats)
library(lsmeans)
#library(multcomp)
#library(multcompView)
library(glmmTMB)
library(tidyverse)
library(tidylog)

#import nint results
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

  
###Fig2: Plot richness and cover of plots along aridity gradient####
#To get this data we have to import the raw data again
wd <- "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\Countriesv3"
###read in the facilitation data for each country
data_files <- list.files(wd)
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\Countriesv3\\",
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

#some descriptive stats
ggplot(all_plotlvl, aes(x = country, y = plotlevel_sprichness)) +
  geom_boxplot()
min(all_plotlvl$plotlevel_sprichness)
max(all_plotlvl$plotlevel_sprichness)

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


#Arrange the cover and sprich bygraz on the same plot
combo_barplot <- ggarrange(bygraz_sprich_barplot, bygraz_cov_barplot, ncol = 1, nrow = 2, labels = "auto", 
                           label.x = 0.1, label.y = 1)
combo_barplot
ggsave("combo_barplot.png", combo_barplot, width = 1500, height = 2100, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Figures")


###Fig3: Graph of Chisq results####
chisq_results <- read.csv("Facilitation data\\results\\Chisq_results_6Feb2024.csv", row.names = 1)
chisq_results$ID <- as.factor(chisq_results$ID)

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

prop_chisq_reduced$association <- factor(prop_chisq_reduced$association, levels = c("nurse", "bare", "neutral"))

facet_labels <- c("b) Bare", "c) Neutral", "a) Dominant")
names(facet_labels) <- c("bare", "neutral", "nurse")

Chisq_scatter <- ggplot(prop_chisq_reduced, aes(x = aridity, y = percentage)) +
  geom_point(color = "darkslategrey", alpha = 0.5, size = 2) +
  facet_wrap(~ association, labeller = labeller(association = facet_labels)) +
  labs(x = "Aridity of plot", y = "Percentage of species showing association") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

#how many points in each pane?
prop_chisq_reduced |> 
  group_by(association) |> 
  summarise(n = n())

ggsave("Chisq_scatter.png", Chisq_scatter, path = "Figures", width = 1500,height = 1100,units = "px")

###Fig.4: Sactterplot of nintC accross SAC with different graz####
##getdata to predict nint over
pred_data <- all_result |>
  select(ID, SAC, pH, graz, site_ID) |> 
  distinct() |> 
  mutate(pH = mean(pH))#get the mean pH so that we can keep it constant in the nintc cover prediction

##NIntc richness##
graphmod <- glmmTMB(NIntc_richness_binom ~ graz+SAC+graz:SAC, family = binomial, data = all_result)#remove random effect because otherwise it makes jagged lines

pred_data$NIntc_richness_binom_prediction <- predict(graphmod, newdata = pred_data, type = "response") #get nintc_binom predictions
pred_data$NIntc_richness_prediction <- pred_data$NIntc_richness_binom_prediction*2 -1

nintc_richness_sac <- ggplot(all_result, aes(y = NIntc_richness, x = SAC)) +
  geom_jitter(height = 0.01, width = 2, color = "darkslategrey", alpha = 0.4, size = 1.5) +
  geom_line(data = pred_data, aes(x = SAC, y = NIntc_richness_prediction, color = graz), lwd = 1.5) +
  scale_color_manual(labels = c("ungrazed", "low", "medium", "high"),
                     values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" ))+
  labs(color = "Grazing pressure", y = expression(NInt[C]~richness), x = "Sand content (%)") +
  theme_classic() +
  theme(legend.position = "right")
#number of points:
all_result |> 
  filter(!is.na(NIntc_richness)) |> 
  summarise(n = n()) #3789

ggsave("nintc_sac_scatter.png", nintc_richness_sac, path = "Figures", width = 1500, height = 900, units = "px")


###Fig.5: Scatterplot of nintc cover over pH####
#get data to predict over
pred_data2 <- all_result |>
  select(ID, SAC, pH, graz, site_ID) |> 
  distinct() |> 
  mutate(SAC = mean(SAC), graz = 1)
pred_data2$graz <- as.factor(pred_data2$graz)

graphmod2 <- glmmTMB(NIntc_cover_binom ~ pH, 
                     family = binomial, data = all_result)#remove random effect because otherwise it makes jagged lines

pred_data2$NIntc_cover_binom_prediction <- predict(graphmod2, newdata = pred_data2, type = "response", se.fit = T)$fit #get nintc_binom predictions
pred_data2$NIntc_cover_prediction <- pred_data2$NIntc_cover_binom_prediction*2 -1

nintc_cover_ph <- ggplot(all_result, aes(y = NIntc_cover, x = pH)) +
  geom_jitter(height = 0.01, width = 0.1, color = "darkslategrey", alpha = 0.4, size = 1.3) +
  geom_line(data = pred_data2, aes(x = pH, y = NIntc_cover_prediction), lwd = 1, colour = "darkorange") +
  labs(y = expression(NInt[C]~cover), x = "pH") +
  theme_classic() 
ggsave("nint_ph_scatter.png", nintc_cover_ph, path = "Figures", height = 700, width = 800, units = 'px')


###Fig 6: Barplot of NINtc at different grazing levels####
#due to the interactions and random effects, the model mean estimates are different to the boxes, therefore it doesn't make sense to look at the significance letters.
nintc_rich_graz_boxplot <- ggplot(all_result, aes(x = graz, y = NIntc_richness, fill = graz)) +
  geom_boxplot(alpha = 0.6)+
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4")) +
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  xlab("Grazing pressure") +
  ylab(expression(NInt[C]~richness)) +
  theme_classic() +
  theme(legend.position = "none")

nintc_cov_graz_boxplot <- ggplot(all_result, aes(x = graz, y = NIntc_cover, fill = graz)) +
  geom_boxplot(alpha = 0.6)+
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4")) +
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  xlab("Grazing pressure") +
  ylab(expression(NInt[C]~cover)) +
  theme_classic() +
  theme(legend.position = "none")

graz_boxes <- ggarrange(nintc_rich_graz_boxplot, nintc_cov_graz_boxplot, nrow = 1, ncol = 2, labels = c("a", "b"))
ggsave("nint_grazlevel_boxplot.png", graz_boxes, path = "Figures", width = 1500, height = 900, units = "px")


###Appendix Fig S2:Scatterplot of nintA accross SAc with different graz####
##getdata to predict nint over
pred_data <- all_result |>
  select(ID, SAC, AMT, aridity, graz, site_ID) |> 
  distinct() |> 
  mutate(AMT = mean(AMT), aridity = mean(aridity))#get the mean AMT and aridity so that we can keep it constant in the prediction

##NIntA richness##
graphmod <- glmmTMB(NInta_richness_binom ~ graz+AMT+SAC+graz:SAC, data = all_result, family = binomial)#remove random effect because otherwise it makes jagged lines

pred_data$NInta_richness_binom_prediction <- predict(graphmod, newdata = pred_data, type = "response") #get nintc_binom predictions
pred_data$NInta_richness_prediction <- pred_data$NInta_richness_binom_prediction*3 -1

ninta_richness_sac <- ggplot(all_result, aes(y = NInta_richness, x = SAC)) +
  geom_jitter(height = 0.01, width = 2, color = "darkslategrey", alpha = 0.4, size = 1.5) +
  geom_line(data = pred_data, aes(x = SAC, y = NInta_richness_prediction, color = graz), lwd = 2) +
  scale_color_manual(labels = c("ungrazed", "low", "medium", "high"),
                     values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4", "azure4" ))+
  labs(color = "Grazing pressure", y = expression(NInt[A]~richness), x = "Sand content (%)") +
  theme_classic() 

ggsave("ninta_sac_scatter.png", ninta_richness_sac, path = "Figures", width = 1500, height = 900, units = "px")


###Appendix Fig S3: Scatterplot of ninta richness across AMT####
#get data to predict over
pred_data3 <- all_result |>
  select(ID, SAC, aridity, AMT, graz, site_ID) |> 
  distinct() |> 
  mutate(aridity = mean(aridity), graz = 1, SAC = mean(SAC))
pred_data3$graz <- as.factor(pred_data3$graz)

##Ninta richness
graphmod3 <- glmmTMB(NInta_richness_binom ~ graz+AMT+SAC+graz:SAC, data = all_result, family = binomial)#remove random effect because otherwise it makes jagged lines

pred_data3$NInta_richness_binom_prediction <- predict(graphmod3, newdata = pred_data3, type = "response") #get nintc_binom predictions
pred_data3$NInta_richness_prediction <- pred_data3$NInta_richness_binom_prediction*3 -1

ninta_richness_AMT <- ggplot(all_result, aes(y = NInta_richness, x = AMT)) +
  geom_jitter(height = 0.01, width = 0.5, color = "darkslategrey", alpha = 0.4, size = 1.5) +
  geom_line(data = pred_data3, aes(x = AMT, y = NInta_richness_prediction), lwd = 2, colour = "darkorange") +
  labs(y = expression(NInt[A]~richness), x = expression(AMT~(degree*C))) +
  theme_classic() 

ggsave("nintA_richness_AMT_scatter.png", ninta_richness_AMT, path = "Figures")


###Appendix Fig S4: Scatterplot of NIntA cover over pH####
pred_data4 <- all_result |>
  select(ID, pH) |> 
  distinct() 

graphmod4 <- glmmTMB(NInta_cover_binom ~ pH, data = all_result, family = binomial)#remove random effect because otherwise it makes jagged lines

pred_data4$NInta_cover_binom_prediction <- predict(graphmod4, newdata = pred_data4, type = "response") #get nintc_binom predictions
pred_data4$NInta_cover_prediction <- pred_data4$NInta_cover_binom_prediction*3 -1

ninta_cover_pH <- ggplot(all_result, aes(y = NInta_cover, x = pH)) +
  geom_jitter(height = 0.01, width = 0.01, color = "darkslategrey", alpha = 0.4, size = 1.5) +
  geom_line(data = pred_data4, aes(x = pH, y = NInta_cover_prediction), lwd = 2, colour = "darkorange") +
  labs(y = expression(NInt[A]~cover), x = "pH") +
  theme_classic()

ggsave("nintA_cover_pH_scatter.png", ninta_cover_pH, path = "Figures")


###General conclusion figure: scatterplot of NInt~aridity####
nintC_rich_arid_scatter <- ggplot(all_result, aes(x = aridity, y = NIntc_richness)) + 
  geom_jitter(shape = 21, size = 2, fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  xlab("Aridity") +
  scale_x_continuous(breaks = seq(0.4, 1.0, by = 0.1)) +
  ylab(expression(NInt[C]~richness))

nintC_cov_arid_scatter <- ggplot(all_result, aes(x = aridity, y = NIntc_cover)) + 
  geom_jitter(shape = 21, size = 2, fill = "darkslategrey", stroke = 0, alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  xlab("Aridity") +
  scale_x_continuous(breaks = seq(0.4, 1.0, by = 0.1)) +
  ylab(expression(NInt[C]~cover))

nint_arid_combo <- ggarrange(nintC_rich_arid_scatter, nintC_cov_arid_scatter, ncol = 2, nrow = 1, labels = c("a", "b"))
ggsave("nint_aridity_scatterplots.png", nint_arid_combo, height = 900, width = 1600, units = "px", 
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Figures")


###Old stuff:Barplot of NINta  cover at different grazing levels####
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


##Put the graphs of arithmetic and lsmean NInta cover on the same figure
NInta_cov_grazlevel_arith_and_lsmeans <- ggarrange(ad_cov_graz_bar_lsmean, ad_cov_graz_bar_arithmetic, ncol = 2, nrow = 1, labels = c("a", "b"))
ggsave("combo_NINta_arithmetic_lsmean_cov_grazlevel_bar.png", NInta_cov_grazlevel_arith_and_lsmeans, width = 1900, height = 1000, unit = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Figures")


###Extra: aridity ~graz boxplot####
nplots <- all_result |> 
  select(ID, graz) |> 
  distinct(ID, graz) |> 
  group_by(graz) |> 
  summarise(n = n())
nplots <- as.data.frame(nplots)
nplots$ycoord <- c(1,1,1,1)

nplots_graz <- ggplot(all_result, aes(x = graz, y = aridity, fill = graz)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  scale_fill_manual(values = c("darkgreen", "chartreuse2" , "darkolivegreen3", "darkgoldenrod4")) +
  labs(x = "Grazing pressure", y = "Aridity") +
  geom_text(data = nplots, aes(x = graz, y = ycoord), label = c(nplots$n)) +
  theme_classic() +
  theme(legend.position = "none")

ggsave("nplots_graz.png", nplots_graz,
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Figures")



