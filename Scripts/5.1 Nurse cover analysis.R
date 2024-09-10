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

##Seems like most countries did note the canopy area or at least the diameter, lets try work with that data
data_files <- list.files("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\Countriesv3")
countrynames <- c("algeria", "argentina", "australia", "chile", "chinachong", "chinaxin", "iranabedi", "iranfarzam", 
                  "israel", "namibiablaum", "namibiawang", "southafrica",  "spainmaestre", "spainrey")
for(i in 1:length(data_files)) {                              
  assign(paste0(countrynames[i]),                                   
         read.csv2(paste0("C:\\Users\\imke6\\Documents\\Msc Projek\\Facilitation analysis clone\\Facilitation data\\Countriesv3\\",
                          data_files[i])))
}


for (i in 1:length(countrynames)) {
  cou <- get(countrynames[i]) #This is the original data for each country
  
  if("diameter_1.cm." %in% colnames(cou)) { #australia doesnt have this data so let the loop continue without it

  cou_canopy <- cou |> 
    select(ID, GRAZ, ARIDITY.v3, Number.of.replicate, Microsite, ID_Microsite, diameter_1.cm., diameter_2.cm., canopy.area..cm2.) |> 
    filter(Microsite == 2) |> 
    distinct(ID, Number.of.replicate, .keep_all = T)
  cou_canopy$diameter_1.cm. <- as.numeric(cou_canopy$diameter_1.cm.)
  cou_canopy$diameter_2.cm. <- as.numeric(cou_canopy$diameter_2.cm.)
  cou_canopy$canopy.area..cm2. <- as.numeric(cou_canopy$canopy.area..cm2.)

if(i == 1) {
  canopy_area <- cou_canopy
} else {
  canopy_area <- bind_rows(canopy_area, cou_canopy) #bind all the tables together
}
  }}


##make sure we have canopy area for every record that we have diameters for
canopy_area_final <- canopy_area |> 
  filter(!is.na(diameter_1.cm.) | !is.na(diameter_2.cm.)) |> 
  mutate(canopy_area_cm2 = (((diameter_1.cm.+diameter_2.cm.)/4)^2)*3.14159) #make sure that canopy are is always calculated.this formula from the raw data sheet
canopy_area_final$GRAZ <- as.factor(canopy_area_final$GRAZ)


#now check of canopy are decreases with graz
ggplot(canopy_area_final, aes(y = log10(canopy_area_cm2), x = GRAZ)) +
geom_boxplot()

ggplot(canopy_area_final, aes(y = log10(canopy_area_cm2), x = ARIDITY.v3)) +
  geom_point()



hist(log10(canopy_area_final$canopy_area_cm2))

canopy_mod <- lm(log10(canopy_area_cm2) ~ GRAZ*ARIDITY.v3, data = canopy_area_final)
summary(canopy_mod)
Anova(canopy_mod)
