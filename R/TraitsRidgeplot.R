# EROS 3 depletions, Panel 2: Trait based measure across the gradient of fishing pressure.
# Simon Dedman simondedman@gmail.com

# library functions ####
library(tidyverse)
library(openxlsx) # read excel
library(anytime) # format dates
library(magrittr) # %<>%
library(ggplot2) # plot
library(lubridate) # today
library(ggridges) # ridge plot
library(scales) # SI units scales

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
finprint <- openxlsx::read.xlsx(xlsxFile = "data/TraitsRidgeplot.xlsx",
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
MarketGravity.plot <- finprint %>%
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
  tidyr::unite(Location_Reef_MarketGravity, c(Location_Reef, dummy), sep = "") %>% # join cols, do remove originals
  dplyr::arrange(desc(MarketGravity)) %>% # ordered by MarketGravity, low to high
  dplyr::mutate(MarketGravity = cut(x = MarketGravity,
                                    breaks = c(-Inf, 0, 5, 25, 100, 500, 1000, Inf),
                                    labels = c(as.character(0),
                                               "<=5", "<=25", "<=100", "<=500", "<=1000", ">1000")))

# :CFAR ####
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
                                                                         panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                                                         # https://stackoverflow.com/questions/67487132/r-ggridges-plot-showing-y-axis-ticks-and-labels
                                                                         axis.ticks = element_line(linewidth = 0.5),         # turn ticks back on
                                                                         axis.ticks.length = grid::unit(5, "pt"),     # set length
                                                                         axis.ticks.y = element_line(colour = "black"), # define tick line color
                                                                         axis.text.y = element_text(vjust = .4))       # center text with tick
ggsave(paste0(saveloc, today(), "_ridgeplot-MarketGravity-CFAR.png"), plot = last_plot(),
       device = "png", path = "", scale = 1, width = 9, height = 5.94, units = "in", dpi = 300, limitsize = TRUE)


# :GeoRange####
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
                                                                         panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                                                         axis.ticks = element_line(linewidth = 0.5),         # turn ticks back on
                                                                         axis.ticks.length = grid::unit(5, "pt"),     # set length
                                                                         axis.ticks.y = element_line(colour = "black"), # define tick line color
                                                                         axis.text.y = element_text(vjust = .4))       # center text with tick
ggsave(paste0(saveloc, today(), "_ridgeplot-MarketGravity-GeoRange.png"), plot = last_plot(),
       device = "png", path = "", scale = 1, width = 9, height = 5.94, units = "in", dpi = 300, limitsize = TRUE)


# :Trophic Level####
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
                                                                         axis.ticks = element_line(linewidth = 0.5),         # turn ticks back on
                                                                         axis.ticks.length = grid::unit(5, "pt"),     # set length
                                                                         axis.ticks.y = element_line(colour = "black"), # define tick line color
                                                                         axis.text.y = element_text(vjust = .4))       # center text with tick
ggsave(paste0(saveloc, today(), "_ridgeplot-MarketGravity-TrophicLevel.png"), plot = last_plot(),
       device = "png", path = "", scale = 1, width = 9, height = 5.94, units = "in", dpi = 300, limitsize = TRUE)


# :MaxTrophic Level####
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
                                                                         axis.ticks = element_line(linewidth = 0.5),         # turn ticks back on
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
