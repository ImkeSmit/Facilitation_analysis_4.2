predictors <- c("graz", "aridity", "aridity2", "AMT", "AMT2", "RASE", "pH", "SAC")


modlist <- list()
l = 1
for(counter1 in 1:length(predictors)) {
  combos <- as.matrix(combn(predictors, counter1))
  for(counter2 in 1:ncol(combos)) {
    mod <- c(combos[, counter2])
    
    modlist[[l]] <- mod
    l = l+1
  }
}

#now loop through every entry in modlist and paste all elements in antry together with + as the separator

