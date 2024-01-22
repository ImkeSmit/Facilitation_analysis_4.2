###Playing with the facilitation data 
##17 May 2023

wd <- "C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation data"
setwd(wd)

#read in facilitation data from SA
SA_fac <- read.csv("Countries\\SouthAfrica_facilitation.csv")
#remove rows with no data
SA_fac <- SA_fac[-which(is.na(SA_fac[,1])),]
#There are cells in "species within quadrat" that have only spaces, replace them with NA's
SA_fac[which(SA_fac$Species.within.quadrat == ""), which(colnames(SA_fac) == "Species.within.quadrat")] <- NA

#Create a unique code for each unit of analysis - a nurse/open microsite in a plot in a site
code <- paste(SA_fac$SITE_ID, SA_fac$PLOT, sep = "-")
code2 <- paste(SA_fac$ID_Microsite, SA_fac$Number.of.replicate, sep = "-")
SA_fac$microsite_rep <- paste(code, code2, sep = "-")
#create a summary data frame for each nurse-open pair to calculate NIntc with 
column_names <- c("site_ID" , "plot" , "microsite" , "microsite_ID" , "replicate_no", "uniquerep", "sprichness", "shannondiv", "totalcover")
summary_fac <- data.frame(matrix(nrow = length(unique(SA_fac$microsite_rep)), ncol = length(column_names)))
colnames(summary_fac) <- column_names
summary_fac$uniquerep <- unique(SA_fac$microsite_rep)
#fill the table
matches <- match(summary_fac$uniquerep, SA_fac$microsite_rep)
summary_fac$site_ID <- SA_fac$SITE_ID[matches]
summary_fac$plot <- SA_fac$PLOT[matches]
summary_fac$microsite <- SA_fac$Microsite[matches]
summary_fac$microsite_ID <- SA_fac$ID_Microsite[matches]
summary_fac$replicate_no <- SA_fac$Number.of.replicate[matches]

#Check that is an equal number of "2" and "1" microsites
length(which(summary_fac$microsite == 1))
length(which(summary_fac$microsite == 2))


#Add the total cover of each uniquerep
totalcover <- tapply(SA_fac$Cover...., SA_fac$microsite_rep, sum)
coverdf <- data.frame(totalcover)
#the rownames are not in the same order, lets order both in the same way
index <- match(summary_fac$uniquerep, rownames(coverdf)) #positions ofuniquerep in rownames(coverdf)
coverdfsort <- coverdf[index, ]
summary_fac$totalcover <- coverdfsort
#replace NA values with zero's
summary_fac$totalcover[is.na(summary_fac$totalcover)] <- 0

#sort summary_fac by increasing row index
summary_fac$rowindex <- as.numeric(row.names(summary_fac))
summary_fac <- summary_fac[order(summary_fac$rowindex), ]

#Loop to calculate and add the species richness of each uniquerep
replist <- summary_fac$uniquerep
l = 1
    for (i in replist) {
  subset <- SA_fac[which(SA_fac$microsite_rep == i),] #subset by uniquerep
  subset <- subset[which(!is.na(subset$Species.within.quadrat)),] #keep only rows that do not have NA values in the Species within quadrat column
  sprichness <- length(subset$Species.within.quadrat)
  summary_fac[l, which(colnames(summary_fac) == "sprichness")] <- sprichness
  l = l+1
}
summary_fac$sprichness <- as.numeric(summary_fac$sprichness)

#Calculate the shannon diversity of each uniquerep
replist <- summary_fac$uniquerep
l = 1
for (i in replist) {
  sub <- SA_fac[which(SA_fac$microsite_rep == i),] #subset by uniquerep
  sub$p <- sub$Number.of.individuals / sum(sub$Number.of.individuals)
  sub$prod <- sub$p * log(sub$p)
  shannon <- -sum(sub$prod)
  summary_fac[l, which(colnames(summary_fac) == "shannondiv")] <- shannon
  l = l+1
}
#Shannondiv is zero when there is one species
#Shannondiv is NA when there are no species

#remove the last empty row in summary_fac
summary_fac <- summary_fac[-which(is.na(summary_fac[,1])),]

#calculate NIntc for each nurse-open pair per plot
sitelist <- unique(summary_fac$site_ID)
plotlist <- unique(summary_fac$plot)
replicatelist <- unique(summary_fac$replicate_no)
colnames <- c("site_ID", "plot", "replicate_no", "nurse", "NIntc_richness", "NIntc_cover", "NIntc_shannon")
NIntc_results <- data.frame(matrix(nrow = length(sitelist)*length(plotlist)*75, ncol = length(colnames)))
colnames(NIntc_results) <- colnames
l = 1

for (j in sitelist) {
  for (k in plotlist) {
    for (i in replicatelist) {
      sub <- summary_fac[which(summary_fac$site_ID == j), ]
      sub2 <- sub[which(sub$plot == k),]
      sub3 <- sub2[which(sub2$replicate_no == i),]
      
      NIntc_results[l,1] <- sub3$site_ID[1]
      NIntc_results[l,2] <- sub3$plot[1]
      NIntc_results[l,3] <- sub3$replicate_no[1]
      NIntc_results[l,4] <- sub3$microsite_ID[1]
      
      #Calculate NIntc_richness
      sprichness_bare <- sub3[which(sub3$microsite == 1), ]$sprichness
      sprichness_nurse <- sub3[which(sub3$microsite == 2), ]$sprichness
      NIntc_richness <- 2*((sprichness_nurse - sprichness_bare)/((sprichness_nurse + sprichness_bare) + abs(sprichness_nurse - sprichness_bare)))
      NIntc_results[l, which(colnames(NIntc_results) == "NIntc_richness")] <- NIntc_richness
      
      #Calculate NIntc_cover
      cover_bare <- sub3[which(sub3$microsite == 1), ]$totalcover
      cover_nurse <- sub3[which(sub3$microsite == 2), ]$totalcover
      NIntc_cover <- 2*((cover_nurse - cover_bare)/((cover_nurse + cover_bare) + abs(cover_nurse - cover_bare)))
      NIntc_results[l, which(colnames(NIntc_results) == "NIntc_cover")] <- NIntc_cover
      
      #Calculate NIntc_shannon
      shannon_bare <- sub3[which(sub3$microsite == 1), ]$shannondiv
      shannon_nurse <- sub3[which(sub3$microsite == 2), ]$shannondiv
      NIntc_shannon <- 2*((shannon_nurse - shannon_bare)/((shannon_nurse + shannon_bare) + abs(shannon_nurse - shannon_bare)))
      NIntc_results[l, which(colnames(NIntc_results) == "NIntc_shannon")] <- NIntc_shannon
      
      l = l+1
    }}}
#NIntc returns NA values when any of the performance variables are zero. 


