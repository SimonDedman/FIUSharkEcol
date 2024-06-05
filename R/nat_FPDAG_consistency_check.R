# Threw this together, hope it helps!!



library(dplyr)
library(dagitty)
library(base64enc)
library(readr)
library(DataCombine)

rm(list = ls())
wd <- "~/Library/CloudStorage/OneDrive-DalhousieUniversity/Documents/Manuscripts/French Polynesia Reef Sharks/DAG consistency checks"
wd <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/NFF Data code/"
setwd(wd)


# DOWNLOAD THE DAG ####
DAG <- dagitty('dag {
ambush_piscivore [pos="-0.876,-1.268"]
ave_npp [pos="-0.927,0.696"]
ave_temp [pos="-0.890,1.097"]
bed_shear_stress [latent,pos="-0.963,1.079"]
blacktip_reef_shark [pos="-0.970,-1.278"]
browser [latent,pos="-0.771,-0.705"]
cloud_cover [latent,pos="-0.785,1.464"]
common_blacktip_shark [latent,pos="-0.962,-2.393"]
coral_spawning [latent,pos="-1.250,0.392"]
crown_of_thorns [latent,pos="-1.147,-0.199"]
crustose_coraline_algae [pos="-0.923,0.349"]
cyclones [latent,pos="-1.113,1.307"]
depth [latent,pos="-0.838,1.489"]
emerged_land_area [pos="-0.883,1.585"]
grazer [pos="-0.893,-0.705"]
great_hammerhead_shark [latent,pos="-0.785,-2.397"]
grey_reef_shark [exposure,pos="-1.206,-1.756"]
hard_coral [outcome,pos="-1.030,-0.171"]
invert [pos="-1.103,0.007"]
invertivore [pos="-1.108,-0.673"]
isl_grp [pos="-1.042,1.753"]
island_geomorphology [pos="-0.970,1.724"]
lagoon_size [pos="-1.086,1.471"]
latitude [pos="-1.104,1.716"]
light [latent,pos="-0.765,1.037"]
longitude [pos="-1.090,1.884"]
nutrient_run_off [latent,pos="-1.076,1.247"]
offshore_prey [latent,pos="-1.301,-0.762"]
other_algae [pos="-0.825,0.260"]
other_offshore_prey_proxies [latent,pos="-1.283,-0.470"]
planktivore [pos="-1.203,-0.701"]
pop_dens [pos="-0.711,0.242"]
population_size [pos="-0.720,0.716"]
pursuit_piscivore [pos="-0.779,-1.271"]
relief [pos="-1.123,0.674"]
scalloped_hammerhead_shark [latent,pos="-1.092,-2.397"]
scraper [pos="-1.003,-0.698"]
season [pos="-0.720,1.543"]
sicklefin_lemon_shark [pos="-1.095,-2.097"]
silvertip_shark [latent,pos="-0.753,-1.802"]
tawny_nurse_shark [pos="-1.067,-1.285"]
tiger_shark [latent,pos="-0.879,-2.400"]
transient_pelagic_sharks [pos="-0.933,-2.078"]
turbidity [latent,pos="-0.773,0.592"]
wave_exposure [latent,pos="-0.953,1.322"]
whitetip_reef_shark [pos="-1.168,-1.385"]
zooplankton [pos="-1.210,0.096"]
ambush_piscivore -> browser
ambush_piscivore -> grazer
ambush_piscivore -> invertivore
ambush_piscivore -> planktivore
ambush_piscivore -> scraper
ave_npp -> crustose_coraline_algae
ave_npp -> hard_coral
ave_npp -> invert
ave_npp -> offshore_prey
ave_npp -> turbidity
ave_npp -> zooplankton
ave_temp -> ave_npp
ave_temp -> crustose_coraline_algae
ave_temp -> hard_coral
ave_temp -> offshore_prey
ave_temp -> other_algae
bed_shear_stress -> crustose_coraline_algae
bed_shear_stress -> relief
blacktip_reef_shark -> browser
blacktip_reef_shark -> grazer
blacktip_reef_shark -> invertivore
blacktip_reef_shark -> planktivore
blacktip_reef_shark -> scraper
browser -> other_algae
cloud_cover -> light
common_blacktip_shark -> transient_pelagic_sharks
crown_of_thorns -> hard_coral
crustose_coraline_algae -> hard_coral
cyclones -> relief
depth -> bed_shear_stress
depth -> light
depth -> wave_exposure
emerged_land_area -> nutrient_run_off
emerged_land_area -> pop_dens
grazer -> crustose_coraline_algae
grazer -> other_algae
great_hammerhead_shark -> transient_pelagic_sharks
grey_reef_shark -> ambush_piscivore
grey_reef_shark -> browser
grey_reef_shark -> grazer
grey_reef_shark -> invertivore
grey_reef_shark -> offshore_prey
grey_reef_shark -> planktivore
grey_reef_shark -> pursuit_piscivore
grey_reef_shark -> scraper
hard_coral -> coral_spawning
invertivore -> invert
invertivore -> other_algae
isl_grp -> island_geomorphology
isl_grp -> light
island_geomorphology -> ave_npp
island_geomorphology -> bed_shear_stress
island_geomorphology -> emerged_land_area
island_geomorphology -> lagoon_size
island_geomorphology -> nutrient_run_off
island_geomorphology -> offshore_prey
lagoon_size -> nutrient_run_off
latitude -> cyclones
latitude -> isl_grp
latitude -> light
light -> ave_npp
light -> ave_temp
light -> turbidity
longitude -> isl_grp
nutrient_run_off -> ave_npp
nutrient_run_off -> crown_of_thorns
nutrient_run_off -> crustose_coraline_algae
nutrient_run_off -> other_algae
nutrient_run_off -> turbidity
other_algae -> hard_coral
other_offshore_prey_proxies -> offshore_prey
planktivore -> coral_spawning
pop_dens -> ambush_piscivore
pop_dens -> browser
pop_dens -> grazer
pop_dens -> hard_coral
pop_dens -> invertivore
pop_dens -> nutrient_run_off
pop_dens -> offshore_prey
pop_dens -> planktivore
pop_dens -> pursuit_piscivore
pop_dens -> scraper
population_size -> pop_dens
pursuit_piscivore -> browser
pursuit_piscivore -> grazer
pursuit_piscivore -> invertivore
pursuit_piscivore -> planktivore
pursuit_piscivore -> scraper
relief -> browser
relief -> grazer
relief -> hard_coral
relief -> invert
relief -> invertivore
relief -> planktivore
relief -> scraper
scalloped_hammerhead_shark -> transient_pelagic_sharks
scraper -> crustose_coraline_algae
scraper -> hard_coral
scraper -> other_algae
season -> ave_temp
season -> cloud_cover
season -> light
sicklefin_lemon_shark -> ambush_piscivore
sicklefin_lemon_shark -> blacktip_reef_shark
sicklefin_lemon_shark -> grey_reef_shark
sicklefin_lemon_shark -> offshore_prey
sicklefin_lemon_shark -> pursuit_piscivore
sicklefin_lemon_shark -> silvertip_shark
sicklefin_lemon_shark -> tawny_nurse_shark
sicklefin_lemon_shark -> whitetip_reef_shark
silvertip_shark -> ambush_piscivore
silvertip_shark -> browser
silvertip_shark -> grazer
silvertip_shark -> invertivore
silvertip_shark -> offshore_prey
silvertip_shark -> planktivore
silvertip_shark -> pursuit_piscivore
silvertip_shark -> scraper
tawny_nurse_shark -> browser
tawny_nurse_shark -> grazer
tawny_nurse_shark -> invertivore
tawny_nurse_shark -> planktivore
tawny_nurse_shark -> scraper
tiger_shark -> transient_pelagic_sharks
transient_pelagic_sharks -> ambush_piscivore
transient_pelagic_sharks -> blacktip_reef_shark
transient_pelagic_sharks -> grey_reef_shark
transient_pelagic_sharks -> offshore_prey
transient_pelagic_sharks -> pursuit_piscivore
transient_pelagic_sharks -> silvertip_shark
transient_pelagic_sharks -> tawny_nurse_shark
transient_pelagic_sharks -> whitetip_reef_shark
turbidity -> crustose_coraline_algae
turbidity -> hard_coral
turbidity -> other_algae
wave_exposure -> bed_shear_stress
whitetip_reef_shark -> browser
whitetip_reef_shark -> grazer
whitetip_reef_shark -> invertivore
whitetip_reef_shark -> planktivore
whitetip_reef_shark -> scraper
zooplankton -> planktivore
}')

names(DAG)

###### IMPORT DATA ######

dat <- read.csv(paste(wd, "/ReefWideBRUVUVC.csv", sep = ""))
str(dat)

wd <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/Nat resources/"
setwd(wd)
name_match <- read.csv(paste(wd, "/FPDAG_match_table.csv", sep = ""))
str(name_match)


###### REPLACE NAMES #####
var_names <- data.frame(current_name = colnames(dat))

var_names$corrected_name <- FindReplace(var_names,
  "current_name",
  name_match,
  from = "name_in_data",
  to = "name_in_dag",
  exact = T,
  vector = F
)

colnames(dat) <- as.character(var_names$corrected_name$current_name)


###### SUBSET DATA TO MATCH DAG ######
ddat <- dat[, colnames(dat) %in% names(DAG)]
str(ddat)


###### DO ANY VARIABLES NEED TO BE TRANSFORMED ? ######
num_vars <- select_if(ddat, is.numeric)

graphics.off()
par(mfrow = c(3, 3))
for (i in 1:ncol(num_vars)) {
  num_var_min <- min(num_vars[, i], na.rm = TRUE)

  num_var_log <- if (num_var_min > 0) {
    log(num_vars[, i])
  } else {
    log(num_vars[, i] + ceiling(abs(num_var_min)) + 1)
  }

  hist(num_vars[, i], main = NA)
  title(colnames(num_vars)[i])

  hist(num_var_log, main = NA)
  title(paste("log", colnames(num_vars)[i]))

  hist((num_vars[, i]^2), main = NA)
  title(paste("square", colnames(num_vars)[i]))
}

# N didn't actually look through to see if anything needs to be transformed...


###### TRANSFORM DATA ######

# Function to standardize
stdize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE) * 2)
} # Sometimes do sd*2 in this...because MacNeil does it in his code...no better reason

ddat[, 3:29] <- lapply(ddat[, 3:29], stdize)


# check % of data which NAs ####
sapply(ddat, function(x) paste(round(sum(is.na(x)) / length(x), 2) * 100, "%", sep = ""))
# browser 18%. Is marked as latent in dag import.
# Just browser...it will be angry so we remove rows with NA...
ddat <- ddat[complete.cases(ddat), ]
# SD: loses 5 of 28 rows, would be better to lose the whole browser column? Or use dummy data?


# VARIABLES IN DAG BUT NOT DATA ####
InDagNotData <- names(DAG)[!names(DAG) %in% names(ddat)]
"bed_shear_stress"
"cloud_cover"
"common_blacktip_shark"
"coral_spawning"
"crown_of_thorns"
"cyclones"
"depth"
"great_hammerhead_shark"
"isl_grp"
"light"
"nutrient_run_off"
"offshore_prey"
"other_offshore_prey_proxies"
"scalloped_hammerhead_shark"
"tiger_shark"
# VARIABLES IN DATA BUT NOT DAG ####
names(ddat)[!names(ddat) %in% names(DAG)]


# Reload DAG omitting latent variables & browser ####
DAG <- dagitty('dag {
ambush_piscivore [pos="-0.876,-1.268"]
ave_npp [pos="-0.927,0.696"]
ave_temp [pos="-0.890,1.097"]
blacktip_reef_shark [pos="-0.970,-1.278"]
crustose_coraline_algae [pos="-0.923,0.349"]
emerged_land_area [pos="-0.883,1.585"]
grazer [pos="-0.893,-0.705"]
grey_reef_shark [exposure,pos="-1.206,-1.756"]
hard_coral [outcome,pos="-1.030,-0.171"]
invert [pos="-1.103,0.007"]
invertivore [pos="-1.108,-0.673"]
island_geomorphology [pos="-0.970,1.724"]
lagoon_size [pos="-1.086,1.471"]
latitude [pos="-1.104,1.716"]
longitude [pos="-1.090,1.884"]
other_algae [pos="-0.825,0.260"]
planktivore [pos="-1.203,-0.701"]
pop_dens [pos="-0.711,0.242"]
population_size [pos="-0.720,0.716"]
pursuit_piscivore [pos="-0.779,-1.271"]
relief [pos="-1.123,0.674"]
scraper [pos="-1.003,-0.698"]
season [pos="-0.720,1.543"]
sicklefin_lemon_shark [pos="-1.095,-2.097"]
silvertip_shark [latent,pos="-0.753,-1.802"]
tawny_nurse_shark [pos="-1.067,-1.285"]
transient_pelagic_sharks [pos="-0.933,-2.078"]
whitetip_reef_shark [pos="-1.168,-1.385"]
ambush_piscivore -> grazer
ambush_piscivore -> invertivore
ambush_piscivore -> planktivore
ambush_piscivore -> scraper
ave_npp -> crustose_coraline_algae
ave_npp -> hard_coral
ave_npp -> invert
ave_temp -> ave_npp
ave_temp -> crustose_coraline_algae
ave_temp -> hard_coral
ave_temp -> other_algae
blacktip_reef_shark -> grazer
blacktip_reef_shark -> invertivore
blacktip_reef_shark -> planktivore
blacktip_reef_shark -> scraper
crustose_coraline_algae -> hard_coral
emerged_land_area -> pop_dens
grazer -> crustose_coraline_algae
grazer -> other_algae
grey_reef_shark -> ambush_piscivore
grey_reef_shark -> grazer
grey_reef_shark -> invertivore
grey_reef_shark -> planktivore
grey_reef_shark -> pursuit_piscivore
grey_reef_shark -> scraper
invertivore -> invert
invertivore -> other_algae
island_geomorphology -> ave_npp
island_geomorphology -> emerged_land_area
island_geomorphology -> lagoon_size
other_algae -> hard_coral
pop_dens -> ambush_piscivore
pop_dens -> grazer
pop_dens -> hard_coral
pop_dens -> invertivore
pop_dens -> planktivore
pop_dens -> pursuit_piscivore
pop_dens -> scraper
population_size -> pop_dens
pursuit_piscivore -> grazer
pursuit_piscivore -> invertivore
pursuit_piscivore -> planktivore
pursuit_piscivore -> scraper
relief -> grazer
relief -> hard_coral
relief -> invert
relief -> invertivore
relief -> planktivore
relief -> scraper
scraper -> crustose_coraline_algae
scraper -> hard_coral
scraper -> other_algae
season -> ave_temp
sicklefin_lemon_shark -> ambush_piscivore
sicklefin_lemon_shark -> blacktip_reef_shark
sicklefin_lemon_shark -> grey_reef_shark
sicklefin_lemon_shark -> pursuit_piscivore
sicklefin_lemon_shark -> silvertip_shark
sicklefin_lemon_shark -> tawny_nurse_shark
sicklefin_lemon_shark -> whitetip_reef_shark
silvertip_shark -> ambush_piscivore
silvertip_shark -> grazer
silvertip_shark -> invertivore
silvertip_shark -> planktivore
silvertip_shark -> pursuit_piscivore
silvertip_shark -> scraper
tawny_nurse_shark -> grazer
tawny_nurse_shark -> invertivore
tawny_nurse_shark -> planktivore
tawny_nurse_shark -> scraper
transient_pelagic_sharks -> ambush_piscivore
transient_pelagic_sharks -> blacktip_reef_shark
transient_pelagic_sharks -> grey_reef_shark
transient_pelagic_sharks -> pursuit_piscivore
transient_pelagic_sharks -> silvertip_shark
transient_pelagic_sharks -> tawny_nurse_shark
transient_pelagic_sharks -> whitetip_reef_shark
whitetip_reef_shark -> grazer
whitetip_reef_shark -> invertivore
whitetip_reef_shark -> planktivore
whitetip_reef_shark -> scraper
}')

# CREATE FAKE VARIABLES ####
# The test won't work with missing variables so will need to create some fake ones for now
# Suchinta recommends against this, and itr's obviated by doing the trimmed DAG
# ddat$`bed_shear_stress` <- runif(n = nrow(ddat), 0, 1)
# # ddat$`browser` <- runif(n = nrow(ddat), 0, 1) # browser added, observed with 5 NAs, marked as unobserved and adding fake data. Added this to the 5 NAs instead
# ddat$`cloud_cover` <- runif(n = nrow(ddat), 0, 1)
# ddat$`common_blacktip_shark` <- runif(n = nrow(ddat), 0, 1)
# ddat$`coral_spawning` <- runif(n = nrow(ddat), 0, 1)
# ddat$`crown_of_thorns` <- runif(n = nrow(ddat), 0, 1)
# ddat$`cyclones` <- runif(n = nrow(ddat), 0, 1)
# ddat$`depth` <- runif(n = nrow(ddat), 0, 1)
# ddat$`great_hammerhead_shark` <- runif(n = nrow(ddat), 0, 1)
# ddat$`isl_grp` <- runif(n = nrow(ddat), 0, 1)
# ddat$`light` <- runif(n = nrow(ddat), 0, 1)
# ddat$`nutrient_run_off` <- runif(n = nrow(ddat), 0, 1)
# ddat$`offshore_prey` <- runif(n = nrow(ddat), 0, 1)
# ddat$`other_offshore_prey_proxies` <- runif(n = nrow(ddat), 0, 1)
# ddat$`scalloped_hammerhead_shark` <- runif(n = nrow(ddat), 0, 1)
# ddat$`tiger_shark` <- runif(n = nrow(ddat), 0, 1)
# ddat$`turbidity` <- runif(n = nrow(ddat), 0, 1)
# ddat$`wave_exposure` <- runif(n = nrow(ddat), 0, 1)
# ddat$`zooplankton` <- runif(n = nrow(ddat), 0, 1)
#
# ddat <- ddat |> select(names(DAG))
# identical(colnames(ddat), names(DAG))
# names(DAG)[!names(DAG) %in% names(ddat)] # Gucci


# EVALUATE THE D-SEPARATION IMPLICATIONS OF THE DAG ####
test <- dagitty::localTests(x = DAG,
                            data = ddat,
                            abbreviate.names = FALSE)
write.csv(x = test,
          file = "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/Nat resources/dag_inconsistencies_all.csv",
          row.names = TRUE)


# SUBSET DATA BASED ON CORRELATION VALUE OR P VALUE ####
# can define pass or fail based on thresholds of p-value (meh) or correlation coefficient (sure)
testf2 <- subset(test, estimate >= 0.3 | estimate <= -0.3)


# SHOW THE INDEPENDENCIES THAT FAILED ####
write.csv(x = testf2,
          file = "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/Nat resources/dag_inconsistencies.csv",
          row.names = TRUE)
res <- test[order(abs(test$estimate)), ] # Sort test results by effect size
dev.off()
dagitty::plotLocalTestResults(tail(res, 20)) # Plot 20 results with largest effect size
