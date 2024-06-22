predictors <- c("graz", "aridity", "aridity2", "AMT", "AMT2", "RASE", "pH", "SAC", 
                "graz:aridity", "graz:RASE", "graz:AMT", "graz:pH", "graz:SAC", "RASE:AMT", "RASE:aridity", "AMT:aridity")

#how many combinations are possible?
n_possible_models = 2^length(predictors) -1

modlist <- list()
l = 1
for(counter1 in 1:length(predictors)) {
  combos <- as.matrix(combn(predictors, counter1))
  
  
  for(counter2 in 1:ncol(combos)) {
    mod <- paste(c(combos[, counter2]), collapse = "+")
    
    modlist[[l]] <- mod
    l = l+1
  }
}



