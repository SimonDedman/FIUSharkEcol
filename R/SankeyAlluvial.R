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

saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/Ecological Importance of Sharks Workshop/Papers Prep/2022 Science/1_EROS/" # path to where you want to save the output

emt <- readr::read_csv("data/SankeyAlluvial.csv") %>%
  # emt <- read_sheet("https://docs.google.com/spreadsheets/d/1dNDEUENa4M1__VNksolw8Or9ysPutEYWMTgZJ-qOe9I/edit#gid=0") %>%
  # select cols of interest
  dplyr::select(Ecomorphotype, `Functional Group`, Realm, Ecosystem, `Ocean Basin`, `Top-down: Direct Predation: Effect Size`:`Bottom-up: Nutrient Vector Storage: Excretion & Egestion: Strength of Evidence`) %>%
  # remove NA emt rows
  tidyr::drop_na(`Functional Group`, Realm) %>%
  # convert High Medium Low to 3 2 1, keep NAs
  dplyr::mutate(dplyr::across(
    .cols = `Top-down: Direct Predation: Effect Size`:`Bottom-up: Nutrient Vector Storage: Excretion & Egestion: Strength of Evidence`,
    ~ dplyr::case_when(
      . == "High" ~ 3,
      . == "Medium" ~ 2,
      . == "Low" ~ 1
    )
  )) %>%
  # remove rays
  dplyr::filter(!Ecomorphotype %in% c("Aquilopelagic", "Rajobenthic")) %>%
  # Need to parse each row out into multi rows when they have more than 1 effect type/SoE combo
  # Need 1 column each for effect type, SoE, EMT.
  tidyr::pivot_longer(cols = tidyselect::contains("Effect Size"),
                      names_to = "Effect Type",
                      values_to = "Effect Size") %>%
  tidyr::pivot_longer(cols = tidyselect::contains("Strength of Evidence"),
                      names_to = "Effect Type2",
                      values_to = "Strength of Evidence") %>%
  # remove ": Effect Size" & ": Strength of Evidence"
  dplyr::mutate(`Effect Type` = stringr::str_remove_all(string = `Effect Type`, pattern = ": Effect Size"),
                `Effect Type2` = stringr::str_remove_all(string = `Effect Type2`, pattern = ": Strength of Evidence"),
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
                `Effect Type` = dplyr::case_match(`Effect Type`,
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
  dplyr::filter(EffectTypesMatch,
                # remove where both are NA
                !BothNA) %>%
  tidyr::unite(col = "FnGpRealm", c(`Functional Group`, Realm), sep = ": ") %>%
  # keep only columns needed for this plot
  dplyr::select(FnGpRealm, `Effect Type`, `Effect Size`, `Strength of Evidence`) %>%
  # make (ordered) factors for all but Freq. Effect type already done above.
  dplyr::mutate(`Effect Size` = ordered(`Effect Size`, levels = c(3, 2, 1)),
                `Strength of Evidence` = ordered(`Strength of Evidence`, levels = c(3, 2, 1))) %>%
  # dplyr::mutate(FnGpRealm = stringr::str_replace_all(string = FnGpRealm, # fix names shorter
  #                                                    pattern = "Macropredatory sharks",
  #                                                    replacement = "Ma"),
  #               FnGpRealm = stringr::str_replace_all(string = FnGpRealm,
  #                                                    pattern = "Mesopredatory sharks",
  #                                                    replacement = "Me"),
  #               FnGpRealm = stringr::str_replace_all(string = FnGpRealm,
  #                                                    pattern = "Ma: Pelagic",
  #                                                    replacement = "*"),
  #               ) %>%
  dplyr::mutate(FnGpRealm = stringr::str_replace_all(string = FnGpRealm, # fix names shorter
                                                     pattern = "Macropredatory sharks: Pelagic",
                                                     replacement = "*"),
                FnGpRealm = ordered(FnGpRealm, levels = c("Macropredatory sharks: Inshore/Shelf",
                                                          "*",
                                                          "Mesopredatory sharks: Inshore/Shelf",
                                                          "Mesopredatory sharks: Pelagic"))) %>%
  tidyr::drop_na(`Effect Type`) %>% # remove leftover row
  # 2023-10-26 edit labels, FnGpRealm,  Mesopredatory sharks:
  dplyr::group_by(`FnGpRealm`, `Effect Type`, `Effect Size`) %>%
  dplyr::summarise(Count = dplyr::n()) %T>% # note matrittr tee pipe
  saveRDS(file = paste0(saveloc, lubridate::today(), "_EMTdataFnGpRealm.rds"))


# 2bar, FnGpRealm, EfTyp, col=EfSz###
ggplot(data = emt,
       aes(axis1 = `FnGpRealm`,
           axis2 = `Effect Type`,
           y = Count)) +
  scale_x_discrete(limits = c("Functional Group: Realm", "Effect Type"), expand = c(0, 0)) + # remove margins
  scale_y_continuous(expand = c(0, 0)) + # remove margins
  geom_alluvium(mapping = aes(fill = `Effect Size`),
                alpha = 0.75 # default is 1/2
  ) +
  scale_fill_manual(values = c(rgb(0.11, 0.62, 0.47),
                               rgb(0.46, 0.44, 0.7),
                               rgb(0.85, 0.37, 0.01))) +
  geom_stratum(width = 1/3) + # default colour black, fill white, width 1/3
  geom_text(stat = "stratum",
            aes(label = stringr::str_wrap(after_stat(stratum), 16)), # text just overlays the text. str_wrap wraps to 12 characters wide before newline.
            size = 5.5, # size of text in strata boxes, was 4.5
            lineheight = 0.75) + # adjust spacing between lines
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)), # strata bottom text, y axis numbers
    axis.title = element_text(size = rel(2)), # "count"
    legend.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(2)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = c(0.12, 0.94), # dist from left, dist from bottom
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    plot.margin = unit(c(0, 0, 0, 0.05), "cm")) # T R B L

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_FnGp.Realm-EfTyp_Col-EfSz.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2,
       width = 4, # 6
       height = 6, # 4
       units = "in", dpi = 300, limitsize = TRUE)

# ended up blanking out "Ma: Pelagic" in Gimp and replacing with asterix.
