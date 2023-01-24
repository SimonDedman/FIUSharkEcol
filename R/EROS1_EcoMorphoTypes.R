# 2023-01-23 Ecomorphotypes bars
# Simon Dedman, simondedman@gmail.com

# ToDo ####
# fix study with more n for effect than evidence - score the evidence, re-run plots. Unknown ref. Asked Jerry.

# load packages####
# install.packages("googlesheets4")
library(googlesheets4)
library(tidyverse)
library(magrittr) # not used (yet)
library(tidylog)
library(ggplot2)
library(lubridate) # today
library(png) # add images to plots
library(grid) # add images to plots

# import data & images####
emt <- read_sheet("https://docs.google.com/spreadsheets/d/1dNDEUENa4M1__VNksolw8Or9ysPutEYWMTgZJ-qOe9I/edit#gid=0")

# nstudies <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1dNDEUENa4M1__VNksolw8Or9ysPutEYWMTgZJ-qOe9I/edit#gid=0",
#                        sheet = "Ecomorphotypes Matrix Figure",
#                        range = "A34:J47")
# meaneffect <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1dNDEUENa4M1__VNksolw8Or9ysPutEYWMTgZJ-qOe9I/edit#gid=0",
#                          sheet = "Ecomorphotypes Matrix Figure",
#                          range = "A50:J63")
# meanevidence <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1dNDEUENa4M1__VNksolw8Or9ysPutEYWMTgZJ-qOe9I/edit#gid=0",
#                            sheet = "Ecomorphotypes Matrix Figure",
#                            range = "A66:J79")

saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/Ecological Importance of Sharks Workshop/Papers Prep/2022 Science/1_EROS/"

# https://stackoverflow.com/questions/9917049/inserting-an-image-to-ggplot2
littoral <- rasterGrob(readPNG(paste0(saveloc, "Graphics/emt_littoral.png")), interpolate = TRUE)
archipelagic <- rasterGrob(readPNG(paste0(saveloc, "Graphics/emt_archipelagic.png")), interpolate = TRUE)
macroceanic <- rasterGrob(readPNG(paste0(saveloc, "Graphics/emt_macroceanic.png")), interpolate = TRUE)
# rajobenthic <- rasterGrob(readPNG(paste0(saveloc, "Graphics/emt_rajobenthic.png")), interpolate = TRUE) # rays if required
# aquilopelagic <- rasterGrob(readPNG(paste0(saveloc, "Graphics/emt_aquilopelagic.png")), interpolate = TRUE)



# Analysis####
# Only 3/12 EMTs have any studies at all outside of diet.
# bar chart for ecological roles: stacked bars based on realm, habitat, etc.
# Caption.

# clean table, convert low medium high to 1 2 3
emt %<>%
  # select cols of interest
  select(Ecomorphotype, `Functional Group`, Realm, Ecosystem, `Ocean Basin`, `Top-down: Direct Predation: Effect Size`:`Bottom-up: Nutrient Vector Storage: Excretion & Egestion: Strength of Evidence`) %>%
  # remove NA emt rows
  drop_na(Ecomorphotype) %>%
  # convert High Medium Low to 3 2 1, keep NAs
  mutate(across(
    .cols = `Top-down: Direct Predation: Effect Size`:`Bottom-up: Nutrient Vector Storage: Excretion & Egestion: Strength of Evidence`,
    ~ dplyr::case_when(
      . == "High" ~ 3,
      . == "Medium" ~ 2,
      . == "Low" ~ 1
    )
  ))


# nstudies table & plot####
# nstudies <-
emt %>%
  filter(!Ecomorphotype %in% c("Aquilopelagic", "Rajobenthic")) %>% # remove rays
  select(c(Ecomorphotype, contains("Effect Size"))) %>% # only select Effect Size columns - s.o.evidence is a dupe
  group_by(Ecomorphotype) %>%
  summarise(across(
    .cols = `Top-down: Direct Predation: Effect Size`:`Bottom-up: Nutrient Vector Storage: Excretion & Egestion: Effect Size`,
    # .cols = 2:last_col(), # this should be the same as above but it cuts out the first (2nd) column, TD:DP:ES. Why?
    # nstudies = count which rows have values
    ~ length(which(!is.na(.)))
  )) %>%
  # remove columns where all effect types/strengths are empty (0)
  select(where(~ !all(.x == 0))) %>%
  # pivot longer to create plot
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Effect Type"
  ) %>%
  # remove "Effect Size"
  mutate(`Effect Type` = str_remove(string = `Effect Type`,
                                    pattern = ": Effect Size"),
         # make Effect Type an ordered factor
         `Effect Type` = ordered(`Effect Type`, levels = c(
           "Top-down: Direct Predation",
           "Top-down: Risk Effects",
           "Top-down: Trophic Cascade",
           "Competition",
           "Bottom-up: Nutrient Vector Storage: Bioturbation",
           "Bottom-up: Nutrient Vector Storage: Excretion & Egestion"))) %>%
  na_if(0) %>% # remove zero rows
  drop_na(value) %>%
  ggplot(aes(x = Ecomorphotype, y = value, fill = `Effect Type`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(y = "Number of Studies") +
  scale_y_continuous(
    breaks = c(0, 4, 8, 12, 16, 20, 24),
    expand = c(0, 0)
  ) +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1)),
    axis.title = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1)),
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  # add pictures. ymin/max as expected. xmin max presumably based on 3 columns @ 1,2,3, w/ xrange 0:4
  annotation_custom(archipelagic, xmin = 0.5, xmax = 1.5, ymin = 15, ymax = 17) +
  annotation_custom(littoral, xmin = 1.5, xmax = 2.5, ymin = 15, ymax = 17) +
  annotation_custom(macroceanic, xmin = 2.5, xmax = 3.5, ymin = 15, ymax = 17)

ggsave(paste0(saveloc, today(), "_EMT_NStudies_bars.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, # changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 8, # NA default. Manually adjust plot box in RStudio after ggplot()
       height = 6, # NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", # c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
       dpi = 300, limitsize = TRUE
)


# mean effect table & plot####
# meaneffect <-
emt %>%
  filter(!Ecomorphotype %in% c("Aquilopelagic", "Rajobenthic")) %>% # remove rays
  select(c(Ecomorphotype, contains("Effect Size"))) %>% # only select Effect Size columns
  group_by(Ecomorphotype) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  # remove columns where all effect types/strengths are empty (NaN)
  select(where(~ !all(is.nan(.x)))) %>%
  # pivot longer to create plot
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Effect Type"
  ) %>%
  # remove "Effect Size"
  mutate(`Effect Type` = str_remove(string = `Effect Type`,
                                    pattern = ": Effect Size"),
         # make Effect Type an ordered factor
         `Effect Type` = ordered(`Effect Type`, levels = c(
           "Top-down: Direct Predation",
           "Top-down: Risk Effects",
           "Top-down: Trophic Cascade",
           "Competition",
           "Bottom-up: Nutrient Vector Storage: Bioturbation",
           "Bottom-up: Nutrient Vector Storage: Excretion & Egestion"))) %>%
  na_if(0) %>% # remove zero rows
  drop_na(value) %>%
  ggplot(aes(x = Ecomorphotype, y = value, fill = `Effect Type`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(y = "Mean Effect Size") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1)),
    axis.title = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1)),
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  # add pictures. ymin/max as expected. xmin max presumably based on 3 columns @ 1,2,3, w/ xrange 0:4
  annotation_custom(archipelagic, xmin = 0.5, xmax = 1.5, ymin = 0.33, ymax = 0.66) +
  annotation_custom(littoral, xmin = 1.5, xmax = 2.5, ymin = 0.33, ymax = 0.66) +
  annotation_custom(macroceanic, xmin = 2.5, xmax = 3.5, ymin = 0.33, ymax = 0.66)

ggsave(paste0(saveloc, today(), "_EMT_MeanEffect_bars.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, # changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 8, # NA default. Manually adjust plot box in RStudio after ggplot()
       height = 6, # NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", # c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
       dpi = 300, limitsize = TRUE
)




# mean evidence table####
# meanevidence <-
emt %>%
  filter(!Ecomorphotype %in% c("Aquilopelagic", "Rajobenthic")) %>% # remove rays
  select(c(Ecomorphotype, contains("Evidence"))) %>% # only select Effect Size columns
  group_by(Ecomorphotype) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  # remove columns where all effect types/strengths are empty (NaN)
  select(where(~ !all(is.nan(.x)))) %>%
  # pivot longer to create plot
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Effect Type"
  ) %>%
  mutate(`Effect Type` = str_remove(string = `Effect Type`,
                                    pattern = ": Strength of Evidence"),
         # make Effect Type an ordered factor
         `Effect Type` = ordered(`Effect Type`, levels = c(
           "Top-down: Direct Predation",
           "Top-down: Risk Effects",
           "Top-down: Trophic Cascade",
           "Competition",
           "Bottom-up: Nutrient Vector Storage: Bioturbation",
           "Bottom-up: Nutrient Vector Storage: Excretion & Egestion"
         ))) %>%
  na_if(0) %>% # remove zero rows
  drop_na(value) %>%
  ggplot(aes(x = Ecomorphotype, y = value, fill = `Effect Type`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(y = "Mean Strength of Evidence") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1)),
    axis.title = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1)),
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  # add pictures. ymin/max as expected. xmin max presumably based on 3 columns @ 1,2,3, w/ xrange 0:4
  annotation_custom(archipelagic, xmin = 0.5, xmax = 1.5, ymin = 0.33, ymax = 0.66) +
  annotation_custom(littoral, xmin = 1.5, xmax = 2.5, ymin = 0.33, ymax = 0.66) +
  annotation_custom(macroceanic, xmin = 2.5, xmax = 3.5, ymin = 0.33, ymax = 0.66)

ggsave(paste0(saveloc, today(), "_EMT_MeanEvidence_bars.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, # changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 8, # NA default. Manually adjust plot box in RStudio after ggplot()
       height = 6, # NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", # c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
       dpi = 300, limitsize = TRUE
)
