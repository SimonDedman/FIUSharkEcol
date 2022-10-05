# 2022-10-04 Chris Mull creeas@gmail.com
# code for extracting data from fishbase.org using rfishbase
# adapted by Simon Dedman simondedman@gmail.com to extract trophic level

install.packages("rfishbase")
library(rfishbase)
library(tidyverse)
elasmo.list <- rfishbase::species_list(Class = "Elasmobranchii")
chimaera.list <- rfishbase::species_list(Class = "Holocephali")
splist <- c(elasmo.list, chimaera.list)
length(splist) # 1303

# fb.species <- rfishbase::species(splist)
# colnames(fb.species)
#
# fisheries.importance <- fb.species %>%
#   dplyr::select(
#     "SpecCode",
#     "Species",
#     "Genus",
#     "Importance",
#     "PriceCateg",
#     "MainCatchingMethod",
#     "II",
#     "MSeines",
#     "MGillnets",
#     "MCastnets",
#     "MTraps",
#     "MSpears",
#     "MTrawls",
#     "MDredges",
#     "MLiftnets",
#     "MHooksLines",
#     "MOther"
#   )

fisheries.importance <- rfishbase::species(
  species_list = splist,
  fields = c(
    "SpecCode",
    "Species",
    "Genus",
    "Importance",
    "PriceCateg",
    "MainCatchingMethod",
    "II",
    "MSeines",
    "MGillnets",
    "MCastnets",
    "MTraps",
    "MSpears",
    "MTrawls",
    "MDredges",
    "MLiftnets",
    "MHooksLines",
    "MOther"
  ))

rfishbase::fb_tables() # list tables
predatortroph <- rfishbase::fb_tbl("predatortroph")
rfishbase::species_fields
colnames(model.estimates)
model.estimates <- rfishbase::estimate(
  species_list = splist,
  fields = c(
    "Species",
    "Troph",
    "seTroph"
  ))
readr::write_csv(x = model.estimates,
                 file = "/home/simon/Dropbox/FIU/FinPrint/Data/RFishBaseTrophicLevel.csv",)
