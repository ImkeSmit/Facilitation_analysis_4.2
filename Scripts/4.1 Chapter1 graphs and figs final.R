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
library(multcomp)
library(multcompView)
library(glmmTMB)

#import nint results
all_result <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv")
all_result$site_ID <- as.factor(all_result$site_ID)
all_result$graz <- as.factor(all_result$graz)
all_result$ID <- as.factor(all_result$ID)

  
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


###Scatterplot of Pbare ~ aridity####
#get the model line to add to the graph
bare_mod2 <- glmmTMB(prop_bare_only ~ aridity ,family = binomial, data = sp_preference)
summary(bare_mod2)
pred_data2 <- data.frame(aridity = c(unique(sp_preference$aridity)))
pred_data2$prop_bare_only_prediction <- predict(bare_mod2, pred_data2, type = "response")

Pbare_scatterplot <- long_plot_sp_pref |> 
  filter(preference == "prop_bare_only") |> 
  ggplot(aes(x = aridity, y = proportion_of_sp)) + 
  geom_point(color = "darkslategrey", alpha = 0.6) +
  geom_line(data = pred_data2, 
            aes(x = aridity, y = prop_bare_only_prediction), color = brewer.pal(8, "Dark2")[7], lwd = 1) +
  theme_classic() +
  xlab("Aridity") +
  ylab(expression("Percentage of competitively excluded species"))
ggsave("Pbare_scatterplot.png", Pbare_scatterplot, width = 1200, height = 1200, units = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Figures")


####Graphs of Chisq results###
chisq_results <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis\\Facilitation data\\results\\Chisq_results_6Feb2024.csv", row.names = 1)
chisq_results$ID <- as.factor(chisq_results$ID)

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



###Fig 6: Barplot of NINtc  cover at different grazing levels####
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
  ylim(-1, 1) +
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
  ylim(-1, 1) +
  geom_text(data = cov_aster, aes(x = graz, y = ycoord), label = "*", size = 6) +
  xlab("Grazing pressure") +
  ylab(expression(Arithmetic~mean~NInt[C]~cover))+
  scale_x_discrete(labels = c("ungrazed", "low", "medium", "high")) +
  theme_classic()+
  theme(legend.position = "none")
arith_cov_graz_bar

##arrange the arithmetic and lsmean plots on the same pane
cov_grazlevel_arith_and_lsmeans <- ggarrange(lsmean_cov_graz_bar, arith_cov_graz_bar, ncol = 2, nrow = 1, labels = c("a", "b"))
ggsave("combo_arithmetic_lsmean_cov_grazlevel_bar.png", cov_grazlevel_arith_and_lsmeans, width = 1900, height = 1000, unit = "px",
       path = "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Figures")



###Appendix Fig1: Barplot of NINta  cover at different grazing levels####
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



