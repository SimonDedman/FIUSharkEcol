# 2023-01-23 Ecomorphotypes bars
# Simon Dedman, simondedman@gmail.com
# imports Google sheet & local png images, tidies table, plots, per ecomorphotype: number of studies,
# mean effect size, mean strength of evidence.

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
# https://corybrunson.github.io/ggalluvial/ nicer than
# https://r-charts.com/flow/sankey-diagram-ggplot2/
# install.packages("ggalluvial")
# library(remotes)
# remotes::install_github("corybrunson/ggalluvial@main", build_vignettes = TRUE)
# https://github.com/corybrunson/ggalluvial/issues/110
# alluvial and ggfittext not being installed that are required to build the vignettes.
# Those packages are listed under Suggests: rather than under Imports: in the DESCRIPTION,
# which i understand to be the correct policy for packages that are only required for vignettes.
# Could you install those packages and try again?
library(ggalluvial)

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
  )) %>%
  # remove rays
  filter(!Ecomorphotype %in% c("Aquilopelagic", "Rajobenthic"))


# Loop through nstudies effectSize strengthEffect for 5 grouping variables
for (groupvariable in c("Ecomorphotype", # groupvariable <- "Ecomorphotype"
                        "Functional Group", # groupvariable <- "Functional Group"
                        "Realm", # groupvariable <- "Realm"
                        "Ecosystem", # groupvariable <- "Ecosystem"
                        "Ocean Basin")) { # open groupvariable loop. # groupvariable <- "Ocean Basin"
  # programming with dplyr vignette, loop over multiple variables: .data[[groupvariable]]
  # depreciated in tidyselect 1.2.0, use all_of(groupvariable)
  # Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
  # Thus use all_of(groupvariable) in select() and .data[[groupvariable]] elsewhere. UGH.

  # nstudies table & plot####
  # nstudies <-
  emt %>%
    select(c(all_of(groupvariable), contains("Effect Size"))) %>% # only select groupvariable & Effect Size columns - s.o.evidence is a dupe
    group_by(.data[[groupvariable]]) %>%
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
    ggplot(aes(x = .data[[groupvariable]], y = value, fill = `Effect Type`)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(y = "Number of Studies") +
    # for EcoMorphotype only
    {if (groupvariable == "Ecomorphotype")
      scale_y_continuous(
        breaks = c(0, 4, 8, 12, 16, 20, 24),
        expand = c(0, 0)
      )} +
    # add pictures. ymin/max as expected. xmin max presumably based on 3 columns @ 1,2,3, w/ xrange 0:4
    {if (groupvariable == "Ecomorphotype") annotation_custom(archipelagic, xmin = 0.5, xmax = 1.5, ymin = 15, ymax = 17)} +
    {if (groupvariable == "Ecomorphotype") annotation_custom(littoral, xmin = 1.5, xmax = 2.5, ymin = 15, ymax = 17)} +
    {if (groupvariable == "Ecomorphotype") annotation_custom(macroceanic, xmin = 2.5, xmax = 3.5, ymin = 15, ymax = 17)} +
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
      legend.key = element_blank()) # removed whitespace buffer around legend boxes which is nice

  ggsave(paste0(saveloc, today(), "_", groupvariable, "_NStudies_bars.png"),
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
    select(c(all_of(groupvariable), contains("Effect Size"))) %>% # only select Effect Size columns
    group_by(.data[[groupvariable]]) %>%
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
    ggplot(aes(x = .data[[groupvariable]], y = value, fill = `Effect Type`)) +
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
    {if (groupvariable == "Ecomorphotype") annotation_custom(archipelagic, xmin = 0.5, xmax = 1.5, ymin = 0.33, ymax = 0.66)} +
    {if (groupvariable == "Ecomorphotype") annotation_custom(littoral, xmin = 1.5, xmax = 2.5, ymin = 0.33, ymax = 0.66)} +
    {if (groupvariable == "Ecomorphotype") annotation_custom(macroceanic, xmin = 2.5, xmax = 3.5, ymin = 0.33, ymax = 0.66)}

  ggsave(paste0(saveloc, today(), "_", groupvariable, "_MeanEffect_bars.png"),
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
    select(c(all_of(groupvariable), contains("Evidence"))) %>% # only select Effect Size columns
    group_by(.data[[groupvariable]]) %>%
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
    ggplot(aes(x = .data[[groupvariable]], y = value, fill = `Effect Type`)) +
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
    {if (groupvariable == "Ecomorphotype") annotation_custom(archipelagic, xmin = 0.5, xmax = 1.5, ymin = 0.33, ymax = 0.66)} +
    {if (groupvariable == "Ecomorphotype") annotation_custom(littoral, xmin = 1.5, xmax = 2.5, ymin = 0.33, ymax = 0.66)} +
    {if (groupvariable == "Ecomorphotype") annotation_custom(macroceanic, xmin = 2.5, xmax = 3.5, ymin = 0.33, ymax = 0.66)}

  ggsave(paste0(saveloc, today(), "_", groupvariable, "_MeanEvidence_bars.png"),
         plot = last_plot(), device = "png", path = "",
         scale = 2, # changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 8, # NA default. Manually adjust plot box in RStudio after ggplot()
         height = 6, # NA default; Then ggsave with defaults, changes from 7x7" to e.g.
         units = "in", # c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
         dpi = 300, limitsize = TRUE
  )

} # close groupvariable loop


#2023-02-09 EMT scatterplot ####
emt %<>%
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
         # so the legend order is logical
         `Effect Type` = ordered(`Effect Type`,
                                 levels = c("Top-down: Direct Predation",
                                            "Top-down: Risk Effects",
                                            "Top-down: Trophic Cascade",
                                            "Competition",
                                            "Bottom-up: Nutrient Vector Storage: Sharks as Food",
                                            "Bottom-up: Nutrient Vector Storage: Excretion & Egestion"))) %>%
  # filter for only those that match, remove permutation dupes
  filter(EffectTypesMatch,
         # remove where both are NA
         !BothNA) %>%
  # remove temp columns
  select(!c(`Effect Type2`, EffectTypesMatch, BothNA)) %>%
  # keep only columns needed for this plot
  select(Ecomorphotype, `Effect Type`, `Effect Size`, `Strength of Evidence`)

saveRDS(emt, file = paste0(saveloc, today(), "_EMTdata.rds"))

ggplot(emt) +
  # jitter is a shortcut for geom_point position = "jitter". Not great for discrete 1:3 though.
  geom_jitter(mapping = aes(x = `Effect Size`,
                            y = `Strength of Evidence`,
                            colour = `Effect Type`,
                            shape = Ecomorphotype),
              size = 3) + # width = 0.9, height = 0.9 does nothing
  # change labels 1 2 3 to low medium high
  scale_x_discrete(limits = 1:3,
                   labels = c("Low", "Medium", "High"),
                   expand = c(0,0)) +
  scale_y_discrete(limits = 1:3,
                   labels = c("Low", "Medium", "High"),
                   expand = c(0,0)) +
  # match colours to map figure
  scale_colour_manual(values = c("red",
                                      "dodgerblue4",
                                      "darkorchid4",
                                      "darkorange1",
                                      "goldenrod3",
                                      "green3")) +
                                        theme_minimal() %+replace% theme(
                                          axis.text = element_text(size = rel(1.5)),
                                          axis.title = element_text(size = rel(2)),
                                          legend.text = element_text(size = rel(1.5)),
                                          legend.title = element_text(size = rel(1.5)),
                                          legend.title.align = 0, # otherwise effect type title centre aligned for some reason
                                          plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                          strip.text.x = element_text(size = rel(2)),
                                          panel.border = element_rect(colour = "black", fill = NA, size = 1),
                                          legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
                                          legend.background = element_blank(),
                                          panel.background = element_rect(fill = "white", colour = "grey50"),
                                          panel.grid = element_line(colour = "grey90"),
                                          legend.key = element_blank()) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_Scatter_x-EfSz_y-SoEv_col-EfTyp_shp-EMT.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, # changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 10, # NA default. Manually adjust plot box in RStudio after ggplot()
       height = 6, # NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", # c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
       dpi = 300, limitsize = TRUE
)







# emt scatterplot facet EMT & Effect type ####
# facet this panels with a facet_wrap() so that ecomorphotypes & type of interaction are presented on separate graphs
ggplot(emt) +
  # jitter is a shortcut for geom_point position = "jitter". Not great for discrete 1:3 though.
  geom_jitter(mapping = aes(x = `Effect Size`,
                            y = `Strength of Evidence`,
                            colour = `Effect Type`,
                            shape = Ecomorphotype),
              size = 3) + # width = 0.9, height = 0.9 does nothing
  facet_wrap(vars(`Effect Type`, Ecomorphotype)) +
  # change labels 1 2 3 to low medium high
  scale_x_discrete(limits = 1:3,
                   labels = c("Low", "Medium", "High"),
                   expand = c(0,0)) +
  scale_y_discrete(limits = 1:3,
                   labels = c("Low", "Medium", "High"),
                   expand = c(0,0)) +
  # match colours to map figure
  scale_colour_manual(
    values = c("red",
                    "dodgerblue4",
                    "darkorchid4",
                    "darkorange1",
                    "goldenrod3",
                    "green3")) +
                      theme_minimal() %+replace% theme(
                        axis.text = element_text(size = rel(1.5)),
                        axis.title = element_text(size = rel(2)),
                        legend.text = element_text(size = rel(1.5)),
                        legend.title = element_text(size = rel(1.5)),
                        legend.title.align = 0, # otherwise effect type title centre aligned for some reason
                        plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                        strip.text.x = element_text(size = rel(2)),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1),
                        legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
                        legend.background = element_blank(),
                        panel.background = element_rect(fill = "white", colour = "grey50"),
                        panel.grid = element_line(colour = "grey90"),
                        legend.key = element_blank()
                      ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_Scatter_x-EfSz_y-SoEv_col-EfTyp_shp-EMT_FacetEfTypEMT.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, # changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 10, # NA default. Manually adjust plot box in RStudio after ggplot()
       height = 6, # NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", # c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
       dpi = 300, limitsize = TRUE
)


# emt scatterplot facet EMT ####
ggplot(emt) +
  geom_jitter(mapping = aes(x = `Effect Size`,
                            y = `Strength of Evidence`,
                            colour = `Effect Type`,
                            shape = Ecomorphotype),
              size = 3) +
  facet_wrap(vars(Ecomorphotype)) +
  scale_x_discrete(limits = 1:3,
                   labels = c("Low", "Medium", "High"),
                   expand = c(0,0)) +
  scale_y_discrete(limits = 1:3,
                   labels = c("Low", "Medium", "High"),
                   expand = c(0,0)) +
  scale_colour_manual(
    values = c("red",
                    "dodgerblue4",
                    "darkorchid4",
                    "darkorange1",
                    "goldenrod3",
                    "green3")) +
                      theme_minimal() %+replace% theme(
                        axis.text = element_text(size = rel(1.5)),
                        axis.title = element_text(size = rel(2)),
                        legend.text = element_text(size = rel(1.5)),
                        legend.title = element_text(size = rel(1.5)),
                        legend.title.align = 0, # otherwise effect type title centre aligned for some reason
                        plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                        strip.text.x = element_text(size = rel(2)),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1),
                        legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
                        legend.background = element_blank(),
                        panel.background = element_rect(fill = "white", colour = "grey50"),
                        panel.grid = element_line(colour = "grey90"),
                        legend.key = element_blank()
                      ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_Scatter_x-EfSz_y-SoEv_col-EfTyp_shp-EMT_FacetEMT.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 2.5, units = "in", dpi = 300, limitsize = TRUE)


# emt scatterplot facet `Effect Type` ####
ggplot(emt) +
  geom_jitter(mapping = aes(x = `Effect Size`,
                            y = `Strength of Evidence`,
                            colour = `Effect Type`,
                            shape = Ecomorphotype),
              size = 3) +
  facet_wrap(vars(`Effect Type`)) +
  scale_x_discrete(limits = 1:3,
                   labels = c("Low", "Medium", "High"),
                   expand = c(0,0)) +
  scale_y_discrete(limits = 1:3,
                   labels = c("Low", "Medium", "High"),
                   expand = c(0,0)) +
  scale_colour_manual(
    values = c("red",
                    "dodgerblue4",
                    "darkorchid4",
                    "darkorange1",
                    "goldenrod3",
                    "green3")) +
                      theme_minimal() %+replace% theme(
                        axis.text = element_text(size = rel(1.5)),
                        axis.title = element_text(size = rel(2)),
                        legend.text = element_text(size = rel(1.5)),
                        legend.title = element_text(size = rel(1.5)),
                        legend.title.align = 0, # otherwise effect type title centre aligned for some reason
                        plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                        strip.text.x = element_text(size = rel(2)),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1),
                        legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
                        legend.background = element_blank(),
                        panel.background = element_rect(fill = "white", colour = "grey50"),
                        panel.grid = element_line(colour = "grey90"),
                        legend.key = element_blank()
                      ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_Scatter_x-EfSz_y-SoEv_col-EfTyp_shp-EMT_FacetEfTyp.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)










# 2023-02-10 Sankey AKA alluvial plot ####
# load & prep variables
saveloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/Ecological Importance of Sharks Workshop/Papers Prep/2022 Science/1_EROS/"
emt <- readRDS(file = paste0(saveloc, "2023-02-22", "_EMTdata.rds")) %>%
  # make (ordered) factors for all but Freq. Effect type already done above.
  mutate(Ecomorphotype = ordered(Ecomorphotype, levels = c("Archipelagic", "Littoral", "Macroceanic")),
         `Effect Size` = ordered(`Effect Size`, levels = c(3, 2, 1)),
         `Strength of Evidence` = ordered(`Strength of Evidence`, levels = c(3, 2, 1)))

# All 4 variables, no colour ####
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(Ecomorphotype, `Effect Type`, `Effect Size`, `Strength of Evidence`) %>%
         summarise(Count = n()),
       aes(axis1 = Ecomorphotype,
           axis2 = `Effect Type`,
           axis3 = `Effect Size`,
           axis4 = `Strength of Evidence`,
           y = Count)
) +
  scale_x_discrete(limits = c("Ecomorphotype",
                              "Effect Type",
                              "Effect Size",
                              "Strength of Evidence"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium() + # aes(fill = `Strength of Evidence`)
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EMT-EfTyp-EfSiz-SoEv.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)



# 3bar: EMT, EfTyp, EfSiz, SoEv=colour ####
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(Ecomorphotype, `Effect Type`, `Effect Size`, `Strength of Evidence`) %>%
         summarise(Count = n()),
       aes(axis1 = Ecomorphotype,
           axis2 = `Effect Type`,
           axis3 = `Effect Size`,
           y = Count)
) +
  scale_x_discrete(limits = c("Ecomorphotype",
                              "Effect Type",
                              "Effect Size"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = `Strength of Evidence`)) + #
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EMT-EfTyp-EfSiz_Col-SoEv.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)


# 3bar: EMT, EfSiz, EfTyp rearrange, SoEv=colour ####
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(Ecomorphotype, `Effect Type`, `Effect Size`, `Strength of Evidence`) %>%
         summarise(Count = n()),
       aes(axis1 = Ecomorphotype,
           axis2 = `Effect Size`,
           axis3 = `Effect Type`,
           y = Count)
) +
  scale_x_discrete(limits = c("Ecomorphotype",
                              "Effect Size",
                              "Effect Type"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = `Strength of Evidence`)) + #
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EMT-EfSiz-EfTyp_Col-SoEv.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)



# 3bar: EMT, EfSiz, SoEv, EfTyp=colour ####
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(Ecomorphotype, `Effect Type`, `Effect Size`, `Strength of Evidence`) %>%
         summarise(Count = n()),
       aes(axis1 = Ecomorphotype,
           axis2 = `Effect Size`,
           axis3 = `Strength of Evidence`,
           y = Count)
) +
  scale_x_discrete(limits = c("Ecomorphotype",
                              "Effect Size",
                              "Strength of Evidence"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = `Effect Type`)) + #
  scale_fill_manual(values = c("red", "dodgerblue4", "darkorchid4", "darkorange1", "goldenrod3", "green3")) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EMT-EfSiz-SoEv_Col-EfTyp.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)



# 3bar: EfTyp, EfSiz, SoEv, EMT=colour ####
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(Ecomorphotype, `Effect Type`, `Effect Size`, `Strength of Evidence`) %>%
         summarise(Count = n()),
       aes(axis1 = `Effect Type`,
           axis2 = `Effect Size`,
           axis3 = `Strength of Evidence`,
           y = Count)
) +
  scale_x_discrete(limits = c("Effect Type",
                              "Effect Size",
                              "Strength of Evidence"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = Ecomorphotype)) + #
  # scale_fill_manual(values = c("red", "dodgerblue4", "darkorchid4", "darkorange1", "goldenrod3", "green3")) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EfTyp-EfSiz-SoEv_Col-EMT.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)



# 2bar: EfTyp, EMT, EfSiz=colour ####
# SoEv,
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(`Effect Type`, Ecomorphotype, `Effect Size`) %>%
         summarise(Count = n()),
       aes(axis1 = `Effect Type`,
           axis2 = Ecomorphotype,
           y = Count)
) +
  scale_x_discrete(limits = c("Effect Type",
                              "Ecomorphotype"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = `Effect Size`)) + #
  # scale_fill_manual(values = c("red", "dodgerblue4", "darkorchid4", "darkorange1", "goldenrod3", "green3")) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EfTyp-EMT_Col-EfSiz.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)



# 2bar: EfTyp, EMT, SoEv=colour ####
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(`Effect Type`, Ecomorphotype, `Strength of Evidence`) %>%
         summarise(Count = n()),
       aes(axis1 = `Effect Type`,
           axis2 = Ecomorphotype,
           y = Count)
) +
  scale_x_discrete(limits = c("Effect Type",
                              "Ecomorphotype"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = `Strength of Evidence`)) + #
  # scale_fill_manual(values = c("red", "dodgerblue4", "darkorchid4", "darkorange1", "goldenrod3", "green3")) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EfTyp-EMT_Col-SoEv.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)



# 2bar: EMT, EfTyp, EfSiz=colour ####
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(Ecomorphotype, `Effect Type`, `Effect Size`) %>%
         summarise(Count = n()),
       aes(axis1 = Ecomorphotype,
           axis2 = `Effect Type`,
           y = Count)
) +
  scale_x_discrete(limits = c("Ecomorphotype",
                              "Effect Type"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = `Effect Size`)) + #
  # scale_fill_manual(values = c("red", "dodgerblue4", "darkorchid4", "darkorange1", "goldenrod3", "green3")) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EMT-EfTyp_Col-EfSiz.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)



# 2bar: EMT, EfTyp, col=SoEv ####
ggplot(data = emt %>%
         # create a Freq column which is a count of all rows with the same attributes for all other columns
         group_by(Ecomorphotype, `Effect Type`, `Strength of Evidence`) %>%
         summarise(Count = n()),
       aes(axis1 = Ecomorphotype,
           axis2 = `Effect Type`,
           y = Count)
) +
  scale_x_discrete(limits = c("Ecomorphotype",
                              "Effect Type"),
                   expand = c(.2, .05)) +
  # xlab("Demographic") +
  geom_alluvium(aes(fill = `Strength of Evidence`)) + #
  # scale_fill_manual(values = c("red", "dodgerblue4", "darkorchid4", "darkorange1", "goldenrod3", "green3")) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival") +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.5)),
    legend.title.align = 0, # otherwise effect type title centre aligned for some reason
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EMT-EfTyp_Col-SoEv.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)



# 2bar, PerEMT: EfTyp, EfSiz, col=SoEv####
for (EMT in levels(emt$Ecomorphotype)) {
  ggplot(data = emt %>%
           filter(Ecomorphotype == EMT) %>%
           # create a Freq column which is a count of all rows with the same attributes for all other columns
           group_by(`Effect Type`, `Effect Size`, `Strength of Evidence`) %>%
           summarise(Count = n()),
         aes(axis1 = `Effect Type`,
             axis2 = `Effect Size`,
             y = Count)
  ) +
    scale_x_discrete(limits = c("Effect Type",
                                "Effect Size"),
                     expand = c(.2, .05)) +
    # xlab("Demographic") +
    geom_alluvium(aes(fill = `Strength of Evidence`)) + #
    # scale_fill_manual(values = c("red", "dodgerblue4", "darkorchid4", "darkorange1", "goldenrod3", "green3")) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
    ggtitle(EMT) +
    theme_minimal() %+replace% theme(
      axis.text = element_text(size = rel(1.5)),
      axis.title = element_text(size = rel(2)),
      legend.text = element_text(size = rel(1)),
      legend.title = element_text(size = rel(1.5)),
      legend.title.align = 0, # otherwise effect type title centre aligned for some reason
      plot.background = element_rect(fill = "white", colour = "grey50"), # white background
      strip.text.x = element_text(size = rel(2)),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
      legend.background = element_blank(),
      panel.background = element_rect(fill = "white", colour = "grey50"),
      panel.grid = element_line(colour = "grey90"),
      legend.key = element_blank()
    ) # removed whitespace buffer around legend boxes which is nice

  ggsave(paste0(saveloc, today(), "_SankeyAlluvial_", EMT, "_EfTyp-EfSize_Col-SoEv.png"),
         plot = last_plot(), device = "png", path = "",
         scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)
}


# 2bar, EMT+EfSz, EfTyp, col=SoEv####
ggplot(data = emt %>%
         # join EMT & EffectSize
         unite(col = "EMT-EfSz", c(Ecomorphotype, `Effect Size`), sep = ": ") %>%
         group_by(`EMT-EfSz`, `Effect Type`, `Strength of Evidence`) %>%
         summarise(Count = n()),
       aes(axis1 = `EMT-EfSz`,
           axis2 = `Effect Type`,
           y = Count)) +
  scale_x_discrete(limits = c("EMT: Effect Size", "Effect Type"), expand = c(.2, .05)) +
  geom_alluvium(aes(fill = `Strength of Evidence`)) + #
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
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
    legend.key = element_blank())

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EMT.EfSz-EfTyp_Col-SoEv.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)




# 2bar, EMT+SoEv, EfTyp, col=EfSz####
ggplot(data = emt %>%
         # join EMT & EffectSize
         unite(col = "EMT-SoEv", c(Ecomorphotype, `Strength of Evidence`), sep = ": ") %>%
         group_by(`EMT-SoEv`, `Effect Type`, `Effect Size`) %>%
         summarise(Count = n()),
       aes(axis1 = `EMT-SoEv`,
           axis2 = `Effect Type`,
           y = Count)) +
  scale_x_discrete(limits = c("EMT: Strength of Evidence", "Effect Type"), expand = c(.2, .05)) +
  geom_alluvium(aes(fill = `Effect Size`)) +
  scale_fill_manual(values = c("black", "blue", "cadetblue1")) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # what does this do?
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
    legend.key = element_blank())

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_EMT.SoEv-EfTyp_Col-EfSz.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 10, height = 4, units = "in", dpi = 300, limitsize = TRUE)
# Archipelagic NA SoE: Tinker et al. 2016, Moxley et al. 2019 [12], and Hughes et al. 2013, Hughes et al. 2019 [15]







# 2023-02-22 FunctionalGroup+Realm####
# replace EMT with this.
emt <- read_sheet("https://docs.google.com/spreadsheets/d/1dNDEUENa4M1__VNksolw8Or9ysPutEYWMTgZJ-qOe9I/edit#gid=0") %>%
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
  # # remove temp columns
  # select(!c(`Effect Type2`, EffectTypesMatch, BothNA)) %>%
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


# 2bar, FnGpRealm, EfTyp, col=EfSz [usethis]####
ggplot(data = emt %>%
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
  # geom_flow() + #just makes everything alpha & greyish?
  # geom_stratum(width = 1/12, fill = "white", color = "grey") +
  geom_stratum(width = 1/3) + # default colour black, fill white, width 1/3
  # geom_label(stat = "stratum", aes(label = after_stat(stratum))) + # label puts in balloon,
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # text just overlays the text. lebel = stringr::str_wrap(after_stat(stratum), 5)
  # coord_flip() +
  # ggtitle() +
  theme_minimal() %+replace% theme(
    axis.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.5)),
    # legend.position = "none", #to hide legend & put in caption if desired
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

# shrink axes width: geom_stratum(width) but then lines expand past it
# todo: wrap text in strata/axes. Turn sideways?  geom_text(angle = 90). Font: family = "Times New Roman"
 ## could wrap the text itself by addling a line break to specific categories. paste(strwrap(x, ...), collapse = "\n")
# remove dead space outside plot - margin?
# nicer colours
# remove y axis labels & title??

ggsave(paste0(saveloc, today(), "_SankeyAlluvial_FnGp.Realm-EfTyp_Col-EfSz.png"),
       plot = last_plot(), device = "png", path = "",
       scale = 2, width = 6, height = 4, units = "in", dpi = 300, limitsize = TRUE)
