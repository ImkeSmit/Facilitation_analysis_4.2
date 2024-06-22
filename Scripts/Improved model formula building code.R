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


# Function to check if a model is valid
is_valid_model <- function(model) {
  terms <- unlist(strsplit(model, "\\+"))
  
  # Define main effects and their corresponding interaction/squared terms
  interactions <- list("graz" = c("graz:aridity", "graz:RASE", "graz:AMT", "graz:pH", "graz:SAC"),
                       "aridity" = c("graz:aridity", "RASE:aridity", "AMT:aridity"),
                       "RASE" = c("graz:RASE", "RASE:AMT", "RASE:aridity"),
                       "AMT" = c("graz:AMT", "RASE:AMT", "AMT:aridity"),
                       "pH" = "graz:pH",
                       "SAC" = "graz:SAC")
  
  squared_terms <- list("aridity" = "aridity2", "AMT" = "AMT2")
  
  # Check for interaction terms without main effects
  for (main_effect in names(interactions)) {
    if (any(interactions[[main_effect]] %in% terms) && !(main_effect %in% terms)) {
      return(FALSE)
    }
  }
  
  # Check for squared terms without main effects
  for (main_effect in names(squared_terms)) {
    if ((squared_terms[[main_effect]] %in% terms) && !(main_effect %in% terms)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#run modlist through the function to see which models are valid
validity = list()
for(m in 1:length(modlist)) {
  validity[[m]] <- is_valid_model(modlist[[m]])
}

#subset modlist to keep only valid models
valid_modlist <- modlist[which(validity == TRUE)]
