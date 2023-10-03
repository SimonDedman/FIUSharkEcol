# EROS 3 depletions, Panel 2: Trait based measure across the gradient of fishing pressure.
# Simon Dedman simondedman@gmail.com

# library functions ####
library(tidyverse)
library(openxlsx)
library(anytime)
library(magrittr)
library(ggplot2)
library(lubridate)
library(ggridges)
library(scales)

cutlogzero <- function(
    x = NULL, # numeric vector
    nbreaks = 10, # integer of breaks desired
    unloglabs = FALSE # if x are logged, give the unlogged equivalents for labels
){
  nbreaks <- nbreaks - 2 # adds Infs at ends
  x <- cut(x = x,
           breaks = c(-Inf, 0:nbreaks, Inf),
           if (unloglabs) {
             labels = c(as.character(0),
                        paste0("<=", round(expm1(1:nbreaks))), # labels in rounded log scale
                        paste0(">", round(expm1(nbreaks))))
           } else { # if not unloglabs, i.e. x are not logged, then:
             labels = c(as.character(0),
                        paste0("<=", 1:nbreaks), # labels in rounded log scale
                        paste0(">", nbreaks))
           } # close ifelse
  ) # close cut
  return(x) # returns a factor
}

# Theme & save location ####
sd_theme <- ggplot2::theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.1)),
                                                      title = element_text(size = rel(1.5)),
                                                      panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                                      panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                      plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                      strip.text.x = element_text(size = rel(2)),
                                                      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/Ecological Importance of Sharks Workshop/Papers Prep/2022 Science/3_DepletionGradient/Figures/Panel2_Traits_SD/"


# load data ####
finprint <- openxlsx::read.xlsx(xlsxFile = "/home/simon/Dropbox/FIU/FinPrint/Data/FinPrint_Set_Data_13Sep2022_KFlowers_SDedman.xlsx",
                                sheet = 1,
                                sep.names = "_",
                                na.strings = c("NA", "9999", "", " ", "#N/A"),
                                detectDates = TRUE) # converts excel style (e.g. 41056) dates but leaves characters "'22/01/2016" the same


# clean data ####
# fix dates
# anytime::getFormats()
anytime::removeFormats("%m/%d/%Y")
anytime::addFormats("%d/%m/%Y")
finprint$set_date <- anytime::anydate(x = finprint$set_date)

# filter for sharks only
finprint %<>% dplyr::filter(!stringr::str_detect(common_name, "ray|Ray|guitarfish|Guitarfish|wobbegong|Wobbegong|Carcharhinus sp.|Manta|manta|stingaree|numbfish|unknown|Unknown"))


# prepare plot data ####
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
                   MaxN = max(maxn, na.rm = TRUE), # average [maxn, TL, CFAR, geoRange] per reef and country
                   TrophicLevelMax = max(TrophicLevel, na.rm = TRUE),
                   TrophicLevel = mean(TrophicLevel, na.rm = TRUE),
                   CFAR = mean(CFAR, na.rm = TRUE),
                   GeoRange = mean(GeoRange, na.rm = TRUE)) %>%
  dplyr::filter(!is.nan(MarketGravity)) %>% # remove NaN row MarketGravity
  tidyr::unite(Location_Reef, c(location_name, reef_name), sep = ": ") %>% # join columns, location: reef (MarketGravity)
  dplyr::mutate(dummy = ")", # add dummy column of closed brackets for next step
                log1pMarketGravity = log1p(MarketGravity)) %>% # log market gravity for bins in ggplot
  tidyr::unite(Location_Reef, c(Location_Reef, MarketGravity), sep = " (", remove = FALSE) %>% # join cols, don't remove originals
  tidyr::unite(Location_Reef_MarketGravity, c(Location_Reef, dummy), sep = "") # join cols, do remove originals








# ggridges vs MarketGravity ####
MarketGravity.plot <- data.plot %>%
  dplyr::arrange(desc(MarketGravity)) %>% # ordered by MarketGravity, low to high
  # dplyr::mutate(MarketGravity = cutlogzero(x = MarketGravity, nbreaks = 10, unloglabs = TRUE)) # Jerry log approach
  dplyr::mutate(MarketGravity = cut(x = MarketGravity,
                                    breaks = c(-Inf, 0, 5, 25, 100, 500, 1000, Inf),
                                    labels = c(as.character(0),
                                               "<=5", "<=25", "<=100", "<=500", "<=1000", ">1000")))

# CFAR
ggplot(MarketGravity.plot, aes(x = CFAR, y = MarketGravity, group = MarketGravity)) +
  geom_density_ridges(scale = 2, size = 0.25, rel_min_height = 0.03, alpha = 0.7) +
  scale_x_continuous(limits = c(0.0, 5.5), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.2)),
                   limits = rev) + # reverse y axis order
  coord_cartesian(clip = "off") +
  # coord_flip() +
  labs(x = "Caudal Fin Aspect Ratio", y = "Market Gravity") +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) %+replace% theme(panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                                                         panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                                         plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                                         panel.border = element_rect(colour = "black", fill = NA, size = 1),
                                                                         # https://stackoverflow.com/questions/67487132/r-ggridges-plot-showing-y-axis-ticks-and-labels
                                                                         axis.ticks = element_line(size = 0.5),         # turn ticks back on
                                                                         axis.ticks.length = grid::unit(5, "pt"),     # set length
                                                                         axis.ticks.y = element_line(colour = "black"), # define tick line color
                                                                         axis.text.y = element_text(vjust = .4))       # center text with tick
ggsave(paste0(saveloc, today(), "_ridgeplot-MarketGravity-CFAR.png"), plot = last_plot(),
       device = "png", path = "", scale = 1, width = 9, height = 5.94, units = "in", dpi = 300, limitsize = TRUE)


# GeoRange
ggplot(MarketGravity.plot |> filter(Location_Reef_MarketGravity != "Barbados: Northwest (114.5)"), aes(x = GeoRange, y = MarketGravity, group = MarketGravity)) +
  geom_density_ridges(scale = 2,
                      size = 0.25,
                      rel_min_height = 0.03,
                      alpha = 0.7) +
  # scale_x_continuous(limits = c(0, 18000000), # -1500000, 18000000
  #                    n.breaks = 8,
  #                    labels = scales::unit_format(unit = "",
  #                                                 scale = 1e-6),
  #                    expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 15000000), #  18000000
                     n.breaks = 15,
                     labels = scales::unit_format(unit = "",
                                                  scale = 1e-6),
                     expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0, 0)),
                   limits = rev) +
  coord_cartesian(clip = "off") +
  # coord_flip() +
  labs(x = expression("Geographic Range" ~ (Million ~ Km^{2})), # https://stackoverflow.com/questions/20980761/superscript-in-r
       y = "Market Gravity") +

  theme_ridges(grid = FALSE, center_axis_labels = TRUE) %+replace% theme(panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                                                         panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                                         plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                                         panel.border = element_rect(colour = "black", fill = NA, size = 1),
                                                                         axis.ticks = element_line(size = 0.5),         # turn ticks back on
                                                                         axis.ticks.length = grid::unit(5, "pt"),     # set length
                                                                         axis.ticks.y = element_line(colour = "black"), # define tick line color
                                                                         axis.text.y = element_text(vjust = .4))       # center text with tick
ggsave(paste0(saveloc, today(), "_ridgeplot-MarketGravity-GeoRange.png"), plot = last_plot(),
       device = "png", path = "", scale = 1, width = 9, height = 5.94, units = "in", dpi = 300, limitsize = TRUE)


# Trophic Level
ggplot(MarketGravity.plot, aes(x = TrophicLevel, y = MarketGravity, group = MarketGravity)) +
  geom_density_ridges(scale = 2, size = 0.25, rel_min_height = 0.01, alpha = 0.7) + #
  scale_x_continuous(limits = c(3.5, 4.6),
                     expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0, 0)),
                   limits = rev) +
  coord_cartesian(clip = "off") +
  labs(x = "Trophic Level", y = "Market Gravity") +
  # coord_flip() +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) %+replace% theme(panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                                                         panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                                         plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                                         panel.border = element_rect(colour = "black", fill = NA, size = 1),
                                                                         axis.ticks = element_line(size = 0.5),         # turn ticks back on
                                                                         axis.ticks.length = grid::unit(5, "pt"),     # set length
                                                                         axis.ticks.y = element_line(colour = "black"), # define tick line color
                                                                         axis.text.y = element_text(vjust = .4))       # center text with tick
ggsave(paste0(saveloc, today(), "_ridgeplot-MarketGravity-TrophicLevel.png"), plot = last_plot(),
       device = "png", path = "", scale = 1, width = 9, height = 5.94, units = "in", dpi = 300, limitsize = TRUE)


# MaxTrophic Level: change L45, run lines 29 & 130
ggplot(MarketGravity.plot, aes(x = TrophicLevelMax, y = MarketGravity, group = MarketGravity)) +
  geom_density_ridges(scale = 2, size = 0.25, rel_min_height = 0.001, alpha = 0.7) + #
  # scale_x_continuous(limits = c(3.6, 4.8), expand = c(0, 0)) +
  scale_x_continuous(limits = c(3.5, 5), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.2)),
                   limits = rev) +
  coord_cartesian(clip = "off") +
  # coord_flip() +
  labs(x = "Maximum Trophic Level", y = "Market Gravity") +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) %+replace% theme(panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                                                         panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                                         plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                                                         panel.border = element_rect(colour = "black", fill = NA, size = 1),
                                                                         axis.ticks = element_line(size = 0.5),         # turn ticks back on
                                                                         axis.ticks.length = grid::unit(5, "pt"),     # set length
                                                                         axis.ticks.y = element_line(colour = "black"), # define tick line color
                                                                         axis.text.y = element_text(vjust = .4))       # center text with tick
ggsave(paste0(saveloc, today(), "_ridgeplot-MarketGravity-MaxTrophicLevel.png"), plot = last_plot(),
       device = "png", path = "", scale = 1, width = 9, height = 5.94, units = "in", dpi = 300, limitsize = TRUE)


# outliers investigate
# CFAR 4.73 @ <=500 grav: Bermuda: North.
outlierCFAR <- finprint |> filter(location_name == "Bermuda", reef_name == "North") # 1 Galapagos shark
# GeoRange 1.68M @ <=500 grav: Barbados: Northwest.
outlierGeoRange <- finprint |> filter(location_name == "Barbados", reef_name == "Northwest") # 1 great hammerhead shark. Filtered out above

# pseudo log scale Y axis debate
plot(x = c(0:6), y = expm1(1:7)) # black = log
points(x = c(0:6), y = c(0, 1, 5, 25, 100, 500, 1000), col = "red") # red = SD pseudo log
expm1(1:7) # 1.718282    6.389056   19.085537   53.598150  147.413159  402.428793 1095.633158
log10(c(1, 10, 100, 1000)) # 0 1 2 3
log10(c(1, 5, 10, 50, 100, 500, 1000)) # 0.00000 0.69897 1.00000 1.69897 2.00000 2.69897 3.00000




# boxplots vs MarketGravity ####
MarketGravity.plot <- data.plot %>%
  dplyr::arrange(MarketGravity) %>% # ordered by MarketGravity, low to high
  dplyr::mutate(MarketGravity = cut(MarketGravity,
                                    breaks = c(-Inf, 0, 5, 25, 100, 500, 1000, Inf),
                                    labels = c("0", "<=5", "<=25", "<=100", "<=500", "<=1000", ">1000")),
                MarketGravity = factor(x = MarketGravity, # reverse factor order for plotting
                                       levels = c("0", "<=5", "<=25", "<=100", "<=500", "<=1000", ">1000"))) # reverse order with rev() here

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
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_boxplot(mapping = aes(x = MarketGravity, y = CFAR)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-CFAR.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_boxplot(mapping = aes(x = MarketGravity, y = GeoRange)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-GeoRange.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)







# violinplots vs MarketGravity ####
MarketGravity.plot <- data.plot %>%
  dplyr::arrange(MarketGravity) %>% # ordered by MarketGravity, low to high
  dplyr::mutate(MarketGravity = cut(MarketGravity,
                                    breaks = c(-Inf, 0, 5, 25, 100, 500, 1000, Inf),
                                    labels = c("0", "<=5", "<=25", "<=100", "<=500", "<=1000", ">1000")),
                MarketGravity = factor(x = MarketGravity, # reverse factor order for plotting
                                       levels = rev(c("0", "<=5", "<=25", "<=100", "<=500", "<=1000", ">1000")))) # reverse order with rev() here. Don't reverse for vertical: use L191 code block

ggplot(MarketGravity.plot) + geom_violin(mapping = aes(x = MarketGravity, y = MaxN), fill = "black") + sd_theme +
  # coord_flip() +
  labs(x = "Market Gravity")
ggsave(paste0(saveloc, today(), "_ViolinPlot-MarketGravity-MaxN.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 6, #NA default. Manually adjust plot box in RStudio after ggplot()
       height = 4.5, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
       dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_violin(mapping = aes(x = MarketGravity, y = TrophicLevel), fill = "black") + sd_theme +
  # coord_flip() +
  labs(x = "Market Gravity", y = "Mean Trophic Level")
ggsave(paste0(saveloc, today(), "_ViolinPlot-MarketGravity-TrophicLevel.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_violin(mapping = aes(x = MarketGravity, y = TrophicLevelMax), fill = "black") + sd_theme +
  # coord_flip() +
  labs(x = "Market Gravity", y = "Max Trophic Level")
ggsave(paste0(saveloc, today(), "_ViolinPlot-MarketGravity-MaxTrophicLevel.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_violin(mapping = aes(x = MarketGravity, y = CFAR), fill = "black") + sd_theme +
  # coord_flip() +
  labs(x = "Market Gravity", y = "Caudal Fin Aspect Ratio")
ggsave(paste0(saveloc, today(), "_ViolinPlot-MarketGravity-CFAR.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MarketGravity.plot) + geom_violin(mapping = aes(x = MarketGravity, y = GeoRange), fill = "black") + sd_theme +
  # coord_flip() +
  labs(x = "Market Gravity", y = "Geographic Range (km2)") +
  scale_y_continuous(limits = c(0, 20000000),
                     labels = scales::unit_format(unit = "M",
                                                  scale = 1e-6))
ggsave(paste0(saveloc, today(), "_ViolinPlot-MarketGravity-GeoRange.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)


# Species Richness vs Market Gravity boxplot####
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

ggplot(richness.plot) + geom_boxplot(mapping = aes(x = MarketGravity, y = Distinct_Species)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-Distinct_Species.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)


# boxplots vs MaxN ####
MaxN.plot <- data.plot %>%
  dplyr::arrange(MaxN) %>% # ordered by MarketGravity, low to high
  dplyr::mutate(MaxN = cut(MaxN,
                           breaks = c(-Inf, 1, 1.125, 1.25, 1.5, 2, 5, Inf),
                           labels = c("1", "<=1.125", "<=1.25", "<=1.5", "<=2", "<=5", ">5")))

ggplot(MaxN.plot) + geom_boxplot(mapping = aes(x = MaxN, y = log1p(MarketGravity))) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MaxN-MarketGravity.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MaxN.plot) + geom_boxplot(mapping = aes(x = MaxN, y = TrophicLevel)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MaxN-TrophicLevel.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MaxN.plot) + geom_boxplot(mapping = aes(x = MaxN, y = CFAR)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MaxN-CFAR.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
ggplot(MaxN.plot) + geom_boxplot(mapping = aes(x = MaxN, y = GeoRange)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MaxN-GeoRange.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)
# Trophic level: mean trophic level of species seen maybe smears out apex preds? Could be max trophic level?
ggplot(MarketGravity.plot) + geom_boxplot(mapping = aes(x = MarketGravity, y = TrophicLevelMax)) + sd_theme
ggsave(paste0(saveloc, today(), "_Boxplot-MarketGravity-MaxTrophicLevel.png"), plot = last_plot(), device = "png",
       path = "", scale = 2, width = 9, height = 5.94, units = "in",dpi = 300, limitsize = TRUE)


# scatterplot x=marketgravity not binned

# 2023-08-21 CFAR vs Linfinity ####
# Colin question whether larger CFAR = larger sharks
# Do top steps until & inc: filter for sharks only
finprintCFAR <- finprint |>
  dplyr::select(common_name, latin_name, CFAR_illou_VDW_Dulvy) |>
  dplyr::group_by(latin_name) |>
  dplyr::summarise(CFAR = mean(CFAR_illou_VDW_Dulvy, na.rm = TRUE)) |>
  dplyr::filter(!is.na(CFAR))

maxLength <- rfishbase::length_freq(finprintCFAR$latin_name) |>
  dplyr::select(Species, MLMax) |>
  dplyr::group_by(Species) |>
  dplyr::summarise(MLMax = max(MLMax, na.rm = TRUE)) |>
  dplyr::rename(latin_name = Species) |>
  dplyr::left_join(finprintCFAR)

ggplot(data = maxLength, aes(x = MLMax, y = CFAR)) +
  geom_point() +
  geom_smooth(method = lm)

# mild positive correlation between CFAR and max length
# tried to get more data but doesn't seem to be available from fishbase:

tmp <- rfishbase::length_freq() |>
  dplyr::select(Species, MLMax) |>
  dplyr::group_by(Species) |>
  dplyr::summarise(MLMax = max(MLMax, na.rm = TRUE)) |>
  dplyr::rename(latin_name = Species)

tmp2 <- rfishbase::common_names() |>
  group_by(SpecCode) |>
  summarise_all(first)

finprintCFAR$latin_name[which(finprintCFAR$latin_name %in% tmp2$Species)]
tmp2[which(tmp2$Species %in% finprintCFAR$latin_name), "SpecCode"]
