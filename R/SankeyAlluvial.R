# 2023-11-13 Sankey/Alluvial plot of Functional group & realm vs Effect type, coloured by effect size####
# Simon Dedman, simondedman@gmail.com

library(googlesheets4)
library(tidyverse)
library(magrittr) # %<>% & %>T%
library(tidylog) # verbosity
library(ggplot2)
library(lubridate) # today
# library(remotes)
# remotes::install_github("corybrunson/ggalluvial@main", build_vignettes = TRUE)
library(ggalluvial)

saveloc <- "path to where you want to save the output"

emt <- readr::read_csv("data/SankeyAlluvial.csv") %>%
# emt <- read_sheet("https://docs.google.com/spreadsheets/d/1dNDEUENa4M1__VNksolw8Or9ysPutEYWMTgZJ-qOe9I/edit#gid=0") %>%
  # select cols of interest
  select(Ecomorphotype, `Functional Group`, Realm, Ecosystem, `Ocean Basin`, `Top-down: Direct Predation: Effect Size`:`Bottom-up: Nutrient Vector Storage: Excretion & Egestion: Strength of Evidence`) %>%
  # remove NA emt rows
  drop_na(`Functional Group`, Realm) %>%
  # convert High Medium Low to 3 2 1, keep NAs
  mutate(across(
    .cols = `Top-down: Direct Predation: Effect Size`:`Bottom-up: Nutrient Vector Storage: Excretion & Egestion: Strength of Evidence`,
    ~ dplyr::case_when(
      . == "High" ~ 3,
      . == "Medium" ~ 2,
      . == "Low" ~ 1
    )
  )) %>%
  # remove rays
  filter(!Ecomorphotype %in% c("Aquilopelagic", "Rajobenthic")) %>%
  # Need to parse each row out into multi rows when they have more than 1 effect type/SoE combo
  # Need 1 column each for effect type, SoE, EMT.
  tidyr::pivot_longer(cols = contains("Effect Size"),
                      names_to = "Effect Type",
                      values_to = "Effect Size") %>%
  tidyr::pivot_longer(cols = contains("Strength of Evidence"),
                      names_to = "Effect Type2",
                      values_to = "Strength of Evidence") %>%
  # remove ": Effect Size" & ": Strength of Evidence"
  mutate(`Effect Type` = str_remove_all(string = `Effect Type`, pattern = ": Effect Size"),
         `Effect Type2` = str_remove_all(string = `Effect Type2`, pattern = ": Strength of Evidence"),
         # double pivot_longer creates n=effecttype^2 rows, all permutations of both. Collapse to where both match
         EffectTypesMatch = ifelse(`Effect Type` == `Effect Type2`, TRUE, FALSE),
         # also want to remove when both are NA later
         BothNA = ifelse(is.na(`Effect Size`) & is.na(`Strength of Evidence`), TRUE, FALSE),
         # for plotting, make integer = discrete
         `Effect Size` = as.integer(`Effect Size`),
         `Strength of Evidence` = as.integer(`Strength of Evidence`),
         # Edit Functional Group names
         `Functional Group` = case_match(`Functional Group`,
                                         "Apex sharks" ~ "Macropredatory sharks",
                                         "Meso sharks / baby apex sharks" ~ "Mesopredatory sharks"),
         # Edit Effect Type Group names
         `Effect Type` = case_match(`Effect Type`,
                                    "Bottom-up: Nutrient Vector Storage: Sharks as Food" ~ "BU:NVS:SAF",
                                    "Bottom-up: Nutrient Vector Storage: Excretion & Egestion" ~ "BU:NVS:EAE",
                                    .default = `Effect Type`),
         # so the legend order is logical
         `Effect Type` = ordered(`Effect Type`,
                                 levels = c("Top-down: Direct Predation",
                                            "Top-down: Risk Effects",
                                            "Top-down: Trophic Cascade",
                                            "Competition",
                                            "BU:NVS:SAF",
                                            "BU:NVS:EAE"))) %>%
  # filter for only those that match, remove permutation dupes
  filter(EffectTypesMatch,
         # remove where both are NA
         !BothNA) %>%
  unite(col = "FnGpRealm", c(`Functional Group`, Realm), sep = ": ") %>%
  # keep only columns needed for this plot
  select(FnGpRealm, `Effect Type`, `Effect Size`, `Strength of Evidence`) %>%
  # make (ordered) factors for all but Freq. Effect type already done above.
  mutate(FnGpRealm = ordered(FnGpRealm, levels = c("Macropredatory sharks: Inshore/Shelf",
                                                   "Macropredatory sharks: Pelagic",
                                                   "Mesopredatory sharks: Inshore/Shelf",
                                                   "Mesopredatory sharks: Pelagic")),
         `Effect Size` = ordered(`Effect Size`, levels = c(3, 2, 1)),
         `Strength of Evidence` = ordered(`Strength of Evidence`, levels = c(3, 2, 1))) %T>% # note matrittr tee pipe
  saveRDS(file = paste0(saveloc, today(), "_EMTdataFnGpRealm.rds"))


# 2bar, FnGpRealm, EfTyp, col=EfSz###
ggplot(data = emt %>%
         mutate(FnGpRealm = stringr::str_replace_all(string = FnGpRealm,
                                                     pattern = "Macropredatory sharks",
                                                     replacement = "Ma"),
                FnGpRealm = stringr::str_replace_all(string = FnGpRealm,
                                                     pattern = "Mesopredatory sharks",
                                                     replacement = "Me")) %>%
         tidyr::drop_na(`Effect Type`) %>% # remove leftover row
         # 2023-10-26 edit labels, FnGpRealm,  Mesopredatory sharks:
         group_by(`FnGpRealm`, `Effect Type`, `Effect Size`) %>%
         summarise(Count = n()),
       aes(axis1 = `FnGpRealm`,
           axis2 = `Effect Type`,
           y = Count)) +
  scale_x_discrete(limits = c("Functional Group: Realm", "Effect Type"), expand = c(.2, .05)) +
  geom_alluvium(mapping = aes(fill = `Effect Size`),
                alpha = 0.75 # default is 1/2
  ) +
  scale_fill_manual(values = c(rgb(0.11, 0.62, 0.47),
                               rgb(0.46, 0.44, 0.7),
                               rgb(0.85, 0.37, 0.01))) +
  geom_stratum(width = 1/3) + # default colour black, fill white, width 1/3
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # text just overlays the text. lebel = stringr::str_wrap(after_stat(stratum), 5)
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    legend.key = element_blank())

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_FnGp.Realm-EfTyp_Col-EfSz.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2,
       width = 4, # 6
       height = 6, # 4
       units = "in", dpi = 300, limitsize = TRUE)

# ended up blanking out "Ma: Pelagic" in Gimp and replacing with asterix.
