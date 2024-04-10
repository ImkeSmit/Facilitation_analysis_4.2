###explore MAT as covariate instead of aridity###
library(tidyverse)
library(tidylog)
library(ggplot2)

#import siteinfo
siteinfo <- read.csv("Facilitation data\\BIODESERT_sites_information.csv") |> 
  select(ID, ARIDITY.v3, AMT, RAI)
siteinfo$ID <- as.factor(siteinfo$ID)

#import nint results and meerge the siteinfo
all_result <- read.csv("Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv", row.names = 1) |> 
  left_join(siteinfo, by = "ID") |> 
  distinct(ID, .keep_all = T) |> 
  select(ID, ARIDITY.v3, AMT, RAI)
all_result$ID <- as.factor(all_result$ID)

#Annual mean temperature vs aridity
ggplot(all_result, aes(x= ARIDITY.v3, y = AMT)) + 
  geom_point() +
  theme_classic()
#correlation test
cor.test(all_result$ARIDITY.v3, all_result$AMT, method = "pearson") # r = 0.3900675, p-value = 7.83e-05 
#significant positive correlation

#annual mean precipitation vs aridity
ggplot(all_result, aes(x= ARIDITY.v3, y = RAI)) + 
  geom_point() +
  theme_classic()
#correlation test
cor.test(all_result$ARIDITY.v3, all_result$RAI, method = "pearson") #r = -0.8854929, p-value < 2.2e-16 
#significant negative relationship

#AMT vs RAI
ggplot(all_result, aes(x= AMT, y = RAI)) + 
  geom_point() +
  theme_classic()
#correlation test
cor.test(all_result$AMT, all_result$RAI, method = "pearson") #r = -0.01414008, p-value = 0.8907
#no correlation

##Spread of aridity
ggplot(all_result, aes(x = ID, y = ARIDITY.v3)) + 
  geom_point() +
  scale_x_discrete(limits = all_result[order(all_result$ARIDITY.v3), "ID"]) +
  theme_classic()

##Spread of MAT
ggplot(all_result, aes(x = ID, y = AMT)) + 
  geom_point() +
  scale_x_discrete(limits = all_result[order(all_result$AMT), "ID"]) +
  theme_classic()

##Spread of RAI
ggplot(all_result, aes(x = ID, y = RAI)) + 
  geom_point() +
  scale_x_discrete(limits = all_result[order(all_result$RAI), "ID"]) +
  theme_classic()


