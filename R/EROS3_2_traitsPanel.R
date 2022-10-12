# EROS 3 depletions, Panel 2: Trait based measure across the gradient of fishing pressure.
# Simon Dedman simondedman@gmail.com

# read in excel database ####
library(tidyverse)
library(openxlsx)
finprint <- openxlsx::read.xlsx(xlsxFile = "/home/simon/Dropbox/FIU/FinPrint/Data/FinPrint_Set_Data_13Sep2022_KFlowers_SDedman.xlsx",
                                sheet = 1,
                                sep.names = "_",
                                na.strings = c("NA", "9999", "", " ", "#N/A"),
                                detectDates = TRUE) # converts excel style (e.g. 41056) dates but leaves characters "'22/01/2016" the same


# fix dates ####
library(anytime)
# anytime::getFormats()
anytime::removeFormats("%m/%d/%Y")
anytime::addFormats("%d/%m/%Y")
finprint$set_date <- anytime::anydate(x = finprint$set_date)


# filter for sharks only
library(magrittr)
finprint %<>% dplyr::filter(!stringr::str_detect(common_name, "ray|Ray|guitarfish|Guitarfish|wobbegong|Wobbegong|Carcharhinus sp.|Manta|manta|stingaree|numbfish|unknown|Unknown"))


# prepare plot data ####
library(ggplot2)
data.plot <- finprint %>%
  dplyr::rename(GeoRange = "GeographicRange_mk^2_Dulvy_etal_2021_CurrBiol",
                CFAR = CFAR_illou_VDW_Dulvy) %>%
  # select() # # select only relevant columns
  # species: common_name	latin_name family
  # locations: region_name	region_id	location_name	location_code	location_id	site_name	site_code	site_type	site_id	reef_name	reef_code	reef_id	trip_year	trip_code	set_code	set_id	set_latitude	set_longitude
  # environment: reef_type [filter for forereef like Katie?]	depth, seagrass etc %?,
  # time: set_date
  # trait: trophic level: TrophicLevel	TrophicLevelSE
  # trait: caudal fin aspect ratio: CFAR_illou_VDW_Dulvy
  # trait: breadth of habitat: GeographicRange_mk^2_Dulvy_etal_2021_CurrBiol
  # abundance/richness/composition: maxn
  # gradient of fishing pressure: protection_status, mpa_area	mpa_year_founded	mpa_isolation mpa_compliance Shark_Protection_Status	Shark_fishing_restrictions	Shark_gears Grav_Total
  dplyr::group_by(location_name, reef_name) %>% # group_by: region_name		location_name			site_name				reef_name
  dplyr::summarise(MarketGravity = round(mean(Grav_Total, na.rm = TRUE), digits = 2),
                   MaxN = mean(maxn, na.rm = TRUE), # average [maxn, TL, CFAR, geoRange] per reef and country
                   TrophicLevel = mean(TrophicLevel, na.rm = TRUE),
                   CFAR = mean(CFAR, na.rm = TRUE),
                   GeoRange = mean(GeoRange, na.rm = TRUE)) %>%
  dplyr::filter(!is.nan(MarketGravity)) %>% # remove NaN row MarketGravity
  tidyr::unite(Location_Reef, c(location_name, reef_name), sep = ": ") %>% # join columns, location: reef (MarketGravity)
  dplyr::mutate(dummy = ")") %>%
  tidyr::unite(Location_Reef, c(Location_Reef, MarketGravity), sep = " (", remove = FALSE) %>%
  tidyr::unite(Location_Reef_MarketGravity, c(Location_Reef, dummy), sep = "")

MarketGravity.plot <- data.plot %>%
  dplyr::arrange(MarketGravity) %>% # ordered by MarketGravity, low to high
  dplyr::mutate(MarketGravity = cut(MarketGravity,
                                    breaks = c(-Inf, 0, 5, 25, 100, 500, 1000, Inf),
                                    labels = c("0", "<=5", "<=25", "<=100", "<=500", "<=1000", ">1000")))

# boxplots vs MarketGravity ####
library(lubridate)
sd_theme <- ggplot2::theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.5)),
                                                      title = element_text(size = rel(1.5)),
                                                      panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                                      panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                      plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                      strip.text.x = element_text(size = rel(2)),
                                                      panel.border = element_rect(colour = "black", fill = NA, size = 1))
saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/Ecological Importance of Sharks Workshop/Papers Prep/2022 Science/3_DepletionGradient/Figures/Panel2_Traits_SD/"
ggplot(MarketGravity.plot) + geom_boxplot(mapping = aes(x = MarketGravity, y = MaxN)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-MaxN.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 6, #NA default. Manually adjust plot box in RStudio after ggplot()
       height = 4.5, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
       dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_boxplot(mapping = aes(x = MarketGravity, y = TrophicLevel)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-TrophicLevel.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 6, height = 4.5, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_boxplot(mapping = aes(x = MarketGravity, y = CFAR)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-CFAR.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 6, height = 4.5, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_boxplot(mapping = aes(x = MarketGravity, y = GeoRange)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-GeoRange.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 6, height = 4.5, units = "in",dpi = 300, limitsize = TRUE)

# Species Richness vs Market Gravity ####
# 2022-10-11 await Katie Flowers reply
# Frances reply
# shannon index?
# cos maxn = CPUE you're not getting an area sampled which will bug some people, have to say relative abundance
# speak to someone more detail oriented?
# by set: number of unique species seen in each video, average richness
# how many seen overall by reef, vs doing it as a by-set thing?
# her stuff, marquesas had high richness esp when including rays.
# not sure how well maxn would play with richness? Similar to CPUE but not exactly.
# Shannon weiner with CPUE means it should be convertible to maxn.
# average per region, also 15 shark species here in general, but on average in any specific video we only saw 2.
# number of unique species her extraction code, teleost & shark code.
# https://github.com/nffarabaugh
# https://github.com/nffarabaugh/Shark_exploration_forSimon
# /home/simon/Dropbox/Galway/Analysis/R/NaomiFarabaugh/Shark_exploration_forSimon/
# start with data.exploration_02_2022_ SharksOnly.R
# that doc builds all the dataframes, then exported, and other stuff e.g. summary frames
# spp_div is what I want = how many species per video
# wide.df2.sharks.csv

richness.plot <- finprint %>%
  dplyr::rename(GeoRange = "GeographicRange_mk^2_Dulvy_etal_2021_CurrBiol",
                CFAR = CFAR_illou_VDW_Dulvy) %>%
  # by set: number of unique species seen in each video, average richness
  # set_code
  dplyr::group_by(set_code) %>%
  dplyr::summarise(dplyr::across(c(1:20, 47:66, 73:98), first),
                   Distinct_Species = dplyr::n_distinct(common_name),
                   total_maxn = sum(maxn), # not used
                   mean_maxn = sum(maxn) / dplyr::n_distinct(common_name)) %>%  # not used
  dplyr::group_by(location_name, reef_name) %>%
  dplyr::summarise(MarketGravity = round(mean(Grav_Total, na.rm = TRUE), digits = 2),
                   Distinct_Species = round(mean(Distinct_Species, na.rm = TRUE), digits = 2)) %>%
  dplyr::filter(!is.nan(MarketGravity)) %>% # remove NaN row MarketGravity
  tidyr::unite(Location_Reef, c(location_name, reef_name), sep = ": ") %>% # join columns for format= 'location: reef (MarketGravity)'
  dplyr::mutate(dummy = ")") %>% # dummy column to add ) later
  tidyr::unite(Location_Reef, c(Location_Reef, MarketGravity), sep = " (", remove = FALSE) %>% # join cols adding (
  tidyr::unite(Location_Reef_MarketGravity, c(Location_Reef, dummy), sep = "") %>% # join again, adds )
  dplyr::arrange(MarketGravity) %>% # ordered by MarketGravity, low to high
  dplyr::mutate(MarketGravity = cut(MarketGravity,
                                    breaks = c(-Inf, 0, 5, 25, 100, 500, 1000, Inf),
                                    labels = c("0", "<=5", "<=25", "<=100", "<=500", "<=1000", ">1000")))

ggplot(richness.plot) +
  geom_boxplot(mapping = aes(x = MarketGravity, y = Distinct_Species)) +
  sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-Distinct_Species.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 6, height = 4.5, units = "in",dpi = 300, limitsize = TRUE)


# boxplots vs MaxN ####
sort(finprint.plot$MaxN)

MaxN.plot <- data.plot %>%
  dplyr::arrange(MaxN) %>% # ordered by MarketGravity, low to high
  dplyr::mutate(MaxN = cut(MaxN,
                           breaks = c(-Inf, 1, 1.125, 1.25, 1.5, 2, 5, Inf),
                           labels = c("1", "<=1.125", "<=1.25", "<=1.5", "<=2", "<=5", ">5")))

ggplot(MaxN.plot) + geom_boxplot(mapping = aes(x = MaxN, y = log1p(MarketGravity))) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MaxN-MarketGravity.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 6, height = 4.5, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MaxN.plot) + geom_boxplot(mapping = aes(x = MaxN, y = TrophicLevel)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MaxN-TrophicLevel.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 6, height = 4.5, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MaxN.plot) + geom_boxplot(mapping = aes(x = MaxN, y = CFAR)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MaxN-CFAR.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 6, height = 4.5, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MaxN.plot) + geom_boxplot(mapping = aes(x = MaxN, y = GeoRange)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MaxN-GeoRange.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 6, height = 4.5, units = "in",dpi = 300, limitsize = TRUE)

# Trophic level: mean trophic level of species seen maybe smears out apex preds? Could be max trophic level?
# Tried



# scatterplot x=marketgravity not binned
