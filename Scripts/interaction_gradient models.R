###Models and other descriptive statistics regarding NIntc across grazing and gradients of MAT, Aridity, RASE, ph and sand content
library(glmmTMB)
library(car)
library(lsmeans)
library(multcomp)
library(multcompView)
library(MuMIn)
library(dplyr)
library(tidyverse)
library(tidylog)

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
  left_join(siteinfo, by = "plotref") |> 
  select(!plotref)
drypop$ID <- as.factor(drypop$ID)

#join the env covariates to the facilitation data
all_result <- all_result |> 
  inner_join(drypop, by = "ID")

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


###Correlations####

#env variables
cordata <- all_result |> 
  select(aridity, AMT, RAI, RASE, pH.b, SAC.b) |> 
  na.omit()
cormat <- cor(cordata, method = "pearson")

setwd("Figures")
png("environmental_variables_correlation.png")
corrplot(cormat, method = "number", type = "lower")
dev.off()

#only RAI and aridity is strongly correlated
#in the models we will use graz+aridity+AMT+RASE+ph.b+SAc.b

#Interaction indexes
cordata2 <- all_result |> 
  select(contains("NInt")) |>
  select(!contains("binom")) |> 
  na.omit()
cormat2 <- cor(cordata2, method = "pearson")

setwd("Figures")
png("nint_correlation.png")
corrplot(cormat2, method = "number", type = "lower")
dev.off()


