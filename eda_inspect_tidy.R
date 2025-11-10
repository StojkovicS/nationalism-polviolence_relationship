############################################################
############### Inspect and Tidy previously joined elections and violence datasets from interwar Yugoslavia
############### Stefan Stojkovic
#############################################

rm(list = ls())

# libs
library(tidyverse); library(pscl); library(scales); library(effects)
library(xtable); library(knitr); library(kableExtra); library(tidyr);
library(stargazer); library(MASS); library(pscl); library(xtable); library(tidyverse)
library(lmtest); library(sandwich); library(glmmTMB)
library(marginaleffects); library(ggeffects); library(gridExtra); library(ggeffects); library(ppcor)


# data
mydata <- read_csv("dataset.csv")
glimpse(mydata)

# Calculate total victims per locality; replace NAs with 0s
mydata$total_civilian_deaths <- mydata$killer_cetnik+mydata$killer_NA+mydata$killer_ustasa
mydata[is.na(mydata)] <- 0

summary(mydata$total_civilian_deaths)
sum(mydata$total_civilian_deaths)

# Calculate the turnout variable
mydata$turnout_fr <- mydata$glasova/mydata$biraca
summary(mydata$turnout_fr)

# Calculate religious fractions
mydata$orth_fr <- mydata$orthodox/mydata$sum_relig
mydata$rcath_fr <- mydata$r_catholic/mydata$sum_relig

# How many obs by state
mydata %>% 
  group_by(state) %>% 
  summarise(serb_vict = sum(victim_serb, na.rm = T),
            cro_vict = sum(victim_croat, na.rm = T))

mydata$dem_agr <- mydata$democratic + mydata$agrarian
mydata$rad_natbloc <- mydata$radical + mydata$national_bloc

# calculate fractions and percentage for parties
vars <- c("radical", "democratic", "agrarian", "hrss", "ymo", "german_party", "dzemijet",
          "rad_natbloc", "dem_agr")

for (v in vars) {
  mydata[[paste0(v, "_fr")]]   <- mydata[[v]] / mydata$glasova
  mydata[[paste0(v, "_perc")]] <- mydata[[v]] / mydata$glasova * 100
}



############### ELECTORAL PARITY index IV #################

### (1)
mydata$comp_index_rad_hrss <- 1 - ((mydata$radik_perc - mydata$hrss_perc) / 100)^2

# If any of the two components equals 0, then index equals 0
mydata$comp_index_rad_hrss <- ifelse(
  mydata$radik_perc == 0 & mydata$hrss_perc == 0, 0, 
  ifelse(mydata$radik_perc < 1 | mydata$hrss_perc < 1,
         0, mydata$comp_index_rad_hrss))

### (1b)
mydata$comp_index_radNatbl_hrss <- 1 - ((mydata$rad_natbloc_perc - mydata$hrss_perc) / 100)^2

# If any of the two components equals 0, then index equals 0
mydata$comp_index_radNatbl_hrss <- ifelse(
  mydata$rad_natbloc_perc == 0 & mydata$hrss_perc == 0, 0, 
  ifelse(mydata$rad_natbloc_perc < 1 | mydata$hrss_perc < 1,
         0, mydata$comp_index_radNatbl_hrss))

### (2) dem vs hrss

mydata$comp_index_dem_hrss <- 1 - ((mydata$democ_perc - mydata$hrss_perc) / 100)^2

mydata$comp_index_dem_hrss <- ifelse(
  mydata$democ_perc == 0 & mydata$hrss_perc == 0, 0, 
  ifelse(mydata$democ_perc < 1 | mydata$hrss_perc < 1,
         0, mydata$comp_index_dem_hrss))

### (2b) dem vs hrss

mydata$comp_index_demAgr_hrss <- 1 - ((mydata$dem_agr_perc - mydata$hrss_perc) / 100)^2

mydata$comp_index_demAgr_hrss <- ifelse(
  mydata$dem_agr_perc == 0 & mydata$hrss_perc == 0, 0, 
  ifelse(mydata$dem_agr_perc < 1 | mydata$hrss_perc < 1,
         0, mydata$comp_index_demAgr_hrss))

### (3) radical vs jmo

mydata$comp_index_rad_jmo <- 1 - ((mydata$radik_perc - mydata$jmo_perc) / 100)^2

mydata$comp_index_rad_jmo <- 
  ifelse(mydata$radik_perc == 0 & mydata$jmo_perc == 0, NA, 
         ifelse(mydata$radik_perc < 0.3 | mydata$jmo_perc < 0.3,
                0, 1 - ((mydata$radik_perc - mydata$jmo_perc) / 100)^2))


### (3) hrss vs jmo

mydata$comp_index_hrss_jmo <- 1 - ((mydata$hrss_perc - mydata$jmo_perc) / 100)^2

mydata$comp_index_hrss_jmo <- 
  ifelse(mydata$hrss_perc == 0 & mydata$jmo_perc == 0, NA, 
         ifelse(mydata$hrss_perc < 0.3 | mydata$jmo_perc < 0.3,
                0, 1 - ((mydata$hrss_perc - mydata$jmo_perc) / 100)^2))



#### Calculate and tidy the proportion of Serbs and Croats #### 

mydata$total_croats <- mydata$serbs_croats-mydata$orthodox-mydata$muslim
mydata$total_serbs <- mydata$serbs_croats - mydata$total_croats

mydata <- mydata %>%
  mutate(
    total_croats = ifelse(total_croats < 0, 0, total_croats),
    total_serbs = ifelse(total_serbs < 0, 0, total_serbs))


mydata <- mydata %>%
  mutate(
    total_croats = ifelse(total_croats > serbs_croats, serbs_croats, total_croats),
    total_serbs = ifelse(total_serbs > serbs_croats, serbs_croats, total_serbs))


mydata$serb_fr <- mydata$total_serbs/mydata$sum_ethn
mydata$cro_fr <- mydata$total_croats/mydata$sum_ethn
mydata$musl_fr <- mydata$muslim/mydata$sum_relig

### ok after this step, the Muslims ARE NOT within the serbo-croat categories




################# Calculate the rest and ETHNIC FRACTIONALIZATION INDEX ####################

### ethnicity fractions

mydata$serbs_croats_fr <- round((mydata$serbs_croats / mydata$sum_ethn), 3)

vars <- c("slovenes","czech","polish","rusyns","russian",
          "turkish","arnauts","hungarians","german","romanian",
          "italian","french","english","other")

for (v in vars) {
  short <- substr(v, 1, 3)
  mydata[[paste0(short, "_fr")]] <- round(mydata[[v]] / mydata$sum_ethn, 3)
}

mydata$bos_musl_fr <- mydata$serbs_croats_fr - mydata$serb_fr - mydata$cro_fr

mydata$ethn_frac <- 1-(mydata$serb_fr^2+mydata$cro_fr^2+mydata$slo_fr^2+mydata$cze_fr^2+mydata$pol_fr^2+mydata$rys_fr^2+mydata$rus_fr^2+
                         mydata$tur_fr^2+mydata$arn_fr^2+mydata$hun_fr^2+mydata$ger_fr^2+mydata$rom_fr^2+mydata$ita_fr^2+
                         mydata$fre_fr^2+mydata$eng_fr^2+mydata$oth_fr^2
                  #     +mydata$bos_musl_fr^2
                       )



########################### ETHNIC PARITY #######################

# Calculate the proportion of Serbs and Croats
mydata$prop_serbs <- round(mydata$total_serbs / (mydata$total_serbs + mydata$total_croats), 3)
mydata$prop_croats <- round(mydata$total_croats / (mydata$total_serbs + mydata$total_croats), 3)

# Calculate the ethnic parity index
mydata$parity_index <- 1 - (mydata$prop_serbs - mydata$prop_croats)^2
mydata$parity_index <- round(mydata$parity_index, digits = 3)



#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### Orthodox and Catholic ####
mydata$prop_orth <- round(mydata$orthodox / (mydata$orthodox + mydata$r_catholic), 3)
mydata$prop_cath <- round(mydata$r_catholic / (mydata$orthodox + mydata$r_catholic), 3)

mydata$parity_orth_cath <- 1 - (mydata$prop_orth - mydata$prop_cath)^2
mydata$parity_orth_cath <- round(mydata$parity_orth_cath, digits = 3)



#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### Orthodox and Muslim ####
mydata$prop_orth <- round(mydata$orthodox / (mydata$orthodox + mydata$muslim), 3)
mydata$prop_musl <- round(mydata$muslim / (mydata$orthodox + mydata$muslim), 3)

mydata$parity_orth_musl <- 1 - (mydata$prop_orth - mydata$prop_musl)^2
mydata$parity_orth_musl <- round(mydata$parity_orth_musl, digits = 3)



#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### Catholic and Muslim ####
mydata$prop_cath <- round(mydata$r_catholic / (mydata$r_catholic + mydata$muslim), 3)
mydata$prop_musl <- round(mydata$muslim / (mydata$r_catholic + mydata$muslim), 3)

mydata$parity_cath_musl <- 1 - (mydata$prop_cath - mydata$prop_musl)^2
mydata$parity_cath_musl <- round(mydata$parity_cath_musl, digits = 3)


######################## ETHNIC POLARIZATION #########################

mydata$ethnic_polarization <- 4 * (
  (mydata$serb_fr^2 * (1 - mydata$serb_fr)) +
    (mydata$cro_fr^2 * (1 - mydata$cro_fr)) +
    (mydata$slo_fr^2 * (1 - mydata$slo_fr)) +
    (mydata$cze_fr^2 * (1 - mydata$cze_fr)) +
    (mydata$pol_fr^2 * (1 - mydata$pol_fr)) +
    (mydata$rys_fr^2 * (1 - mydata$rys_fr)) +
    (mydata$rus_fr^2 * (1 - mydata$rus_fr)) +
   # (mydata$bos_musl_fr^2 * (1 - mydata$bos_musl_fr)) +
    (mydata$tur_fr^2 * (1 - mydata$tur_fr)) +
    (mydata$arn_fr^2 * (1 - mydata$arn_fr)) +
    (mydata$hun_fr^2 * (1 - mydata$hun_fr)) +
    (mydata$ger_fr^2 * (1 - mydata$ger_fr)) +
    (mydata$rom_fr^2 * (1 - mydata$rom_fr)) +
    (mydata$ita_fr^2 * (1 - mydata$ita_fr)) +
    (mydata$fre_fr^2 * (1 - mydata$fre_fr)) +
    (mydata$eng_fr^2 * (1 - mydata$eng_fr)) +
    (mydata$oth_fr^2 * (1 - mydata$oth_fr))
)

# Convert categorical vars to factor type, to fix them in the analyses
mydata$district <- as.factor(mydata$district)
mydata$county <- as.factor(mydata$county)
mydata$state <- as.factor(mydata$state)

mydata$parity_category <- cut(mydata$parity_index, 
                              breaks = c(0, 0.1, 0.35, 1), 
                              labels = c("Low", "Medium", "High"), 
                              include.lowest = TRUE)


mydata$total_pop_scaled <- scale(mydata$total_pop)
mydata$log_victims <- log1p(mydata$total_civilian_deaths)  # log(1 + x)
mydata$parity_cat <- ifelse(mydata$parity_index >= median(mydata$parity_index, na.rm = TRUE), "High", "Low")
mydata$parity_cat2 <- ifelse(mydata$parity_index >= mean(mydata$parity_index, na.rm = TRUE), "High", "Low")
mydata$total_pop_scaled <- as.numeric(mydata$total_pop_scaled)
