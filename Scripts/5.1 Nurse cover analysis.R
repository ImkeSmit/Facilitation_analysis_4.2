#testing whether the canopy size of dominant plants decrease with grazing pressure

#import FT data
FT <- read.csv("C:\\Users\\imke6\\Documents\\Msc Projek\\Functional trait analysis clone\\Functional trait data\\Clean data\\FT_match_facilitation_plots_plotspecific_species.csv", row.names = 1) |> 
  select(ID, GRAZ, coverDryfun20, taxon) 
FT$ID <- as.factor(FT$ID)
FT$GRAZ <- as.factor(FT$GRAZ)

#import nint results
all_result <- read.csv("Facilitation data\\results\\NIntc_results_allcountries_6Feb2024.csv", row.names = 1) |> 
  distinct(ID, nurse, graz)
all_result$ID <- as.factor(all_result$ID)
all_result$graz <- as.factor(all_result$graz)

#join the cover in the FT data to the nurse in the nint data
join <- all_result |> 
  left_join(FT, by = c("ID" = "ID", "nurse" = "taxon", "graz" = "GRAZ")) |> 
  filter(!(is.na(coverDryfun20))) #remove na cover values

ggplot(join, aes(x = graz, y = coverDryfun20)) +
  geom_boxplot()

hist(join$coverDryfun20)

nurse_covermod <- lm(log(coverDryfun20) ~ graz, data = join)

summary(nurse_covermod)
Anova(nurse_covermod)
