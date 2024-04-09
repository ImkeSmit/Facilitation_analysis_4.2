###explore MAT as covariate instead of aridity###
library(tidyverse)
library(tidylog)

#import nint results
all_result <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv", row.names = 1)

#import siteinfo
siteinfo <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\BIODESERT_sites_information.csv") |> 
  select(ID, ARIDITY.v3, AMT, RAI)
siteinfo$ID <- as.factor(siteinfo$ID)
  
all_result <- all_result |> 
  left_join(siteinfo, by = "ID")

#Annual mean temperature vs aridity
ggplot(all_result, aes(x= ARIDITY.v3, y = AMT)) + 
  geom_point() +
  theme_classic()
#correlation test
cor.test(all_result$ARIDITY.v3, all_result$AMT, method = "spearman") #p-value < 2.2e-16 
#significantly positively correlated


#annual mean precipitation vs aridity
ggplot(all_result, aes(x= ARIDITY.v3, y = RAI)) + 
  geom_point() +
  theme_classic()
#correlation test
cor.test(all_result$ARIDITY.v3, all_result$RAI, method = "spearman") #p-value < 2.2e-16 
#significantly negatively correlated


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
  #geom_point(aes(y = AMT), col = "green") +
  theme_classic()
