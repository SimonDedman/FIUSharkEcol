# Code by Matias Braccini, matias.Braccini@dpird.wa.gov.au, 2023
# Edited & packaged by Simon Dedman, simondedman@gmail.com, 2023

library(tidyverse)
library(stringr)
library(cowplot)
library(scales)

# Read in data ------------------------------------------------------------
hndl <- "C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/External collaborations/Shark Importance/" # Matias local
hndl <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/Ecological Importance of Sharks Workshop/Papers Prep/2022 Science/2_Baselines/Figure2c_Fisheries_Matias/" # Simon local
FAO <- read.csv("data/FAO catch by nation and year.csv")
Effort <- read.csv("data/TotalEffortby_FishingCountry_LengthBoat_Gear_Sector.csv")


# Manipulate data ------------------------------------------------------------
# Catch
FAO.long <- FAO %>%
  replace(is.na(.), 0) %>% # there are reported NAs and 0s, then we assume that NA==0
  gather(Country, Landings, -year) %>%
  group_by(year) %>%
  mutate(Total = sum(Landings)) %>%
  group_by(Country) %>%
  mutate(Keyplayers = sum(Landings)) %>%
  ungroup() %>%
  mutate(Prop = Keyplayers / sum(Landings))
CumKtch <- FAO.long %>%
  distinct(Country, .keep_all = T) %>%
  arrange(-Prop) %>%
  mutate(
    Nrow = row_number(),
    Category = ifelse(Nrow <= 10, Country, "Other"),
    Category = gsub("\\.", " ", Category),
    LVLS = ifelse(Nrow <= 10, Nrow, 11)
  )
# ACA
n.others <- length(unique(CumKtch %>% filter(Category == "Other") %>% pull(Country)))
CumKtch <- CumKtch %>%
  mutate(Category = ifelse(Category == "Other", paste0("Other (", n.others, " countries/territories)"),
    Category
  ))
LVLS <- CumKtch %>%
  distinct(Category, LVLS) %>%
  arrange(LVLS) %>%
  pull(Category)
CumKtch$Category <- factor(CumKtch$Category, levels = LVLS)
FAO.long <- FAO.long %>%
  dplyr::select(-c(Keyplayers, Prop)) %>%
  left_join(
    CumKtch %>%
      dplyr::select(Country, Category),
    by = "Country"
  )
# Effort
Displayed.gears <- c(
  "Gillnet", "Longline",
  "Handline & pole", "Purse seine",
  "Bottom trawl"
)
Effort <- Effort %>% # The units are kW x days, and the plot in the most recent paper used GWxdays to get rid of some of the zeros
  filter(!is.na(Country)) %>%
  mutate(Effort = EffActive) %>% # assumed effort variable: 'EffActive'
  mutate(
    Gear = case_when(
      Gear == "Lines_Longlines" ~ "Longline",
      Gear == "Gillnets" ~ "Gillnet",
      Gear == "Lines_Handlines_and_poles" ~ "Handline & pole",
      Gear == "Seine_Purse_Seine" ~ "Purse seine",
      Gear == "Trawl_Bottom" ~ "Bottom trawl",
      TRUE ~ Gear
    ),
    Gear_category = ifelse(Gear %in% Displayed.gears, Gear, "Other"),
    Gear_category = factor(Gear_category, levels = c(Displayed.gears, "Other"))
  ) %>%
  rename(year = Year) %>%
  filter(!Gear_category == "Other")

CPUE <- FAO.long %>%
  group_by(year) %>%
  summarise(Landings = sum(Landings) / 1000) %>%
  left_join(
    Effort %>%
      group_by(year, Gear_category) %>%
      summarise(Effort = sum(Effort, na.rm = T)) %>%
      group_by(Gear_category) %>%
      mutate(Re.eff = Effort / max(Effort, na.rm = T)) %>%
      group_by(year) %>%
      summarise(Re.eff = sum(Re.eff, na.rm = T)),
    by = "year"
  ) %>%
  mutate(
    CPUE = Landings / Re.eff,
    CPUE.rel = CPUE / max(CPUE, na.rm = T)
  ) %>%
  filter(!is.na(CPUE))

# Generate outputs ------------------------------------------------------------

# 1. Countries combined
# landings
p <- FAO.long %>%
  group_by(year) %>%
  summarise(Landings = sum(Landings) / 1000) %>%
  ggplot() +
  geom_area(aes(year, Landings), fill = "cadetblue3", colour = 1, alpha = .35, stat = "identity") +
  theme_bw() +
  ylab("Reported landings (1000s of tonnes)") +
  xlab("Year") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

# effort
eff.transf <- "Relative"
if (eff.transf == "Relative") {
  Agg.eff <- Effort %>%
    group_by(year, Gear_category) %>%
    summarise(Effort = sum(Effort)) %>%
    group_by(Gear_category) %>%
    mutate(Re.eff = Effort / max(Effort))
  coeff <- 700 / max(Agg.eff$Re.eff)
  p <- p +
    geom_line(data = Agg.eff, aes(x = year, y = Re.eff * coeff, colour = Gear_category), linewidth = 1.1, alpha = 1, linetype = 1) +
    scale_y_continuous(sec.axis = sec_axis(~ . / coeff, name = "Relative effort: coloured lines. CPUE: broken line"),
                       expand = c(0,0))
}
if (eff.transf == "LOG10") {
  Agg.eff <- Effort %>%
    group_by(year, Gear_category) %>%
    summarise(Effort = sum(Effort)) %>%
    ungroup() %>%
    mutate(log.Effort = log10(Effort))
  coeff <- 700 / max(Agg.eff$log.Effort)
  p <- p +
    geom_line(data = Agg.eff, aes(x = year, y = log.Effort * coeff, colour = Gear_category), linewidth = 1.1, alpha = 1, linetype = 1) +
    scale_y_continuous(sec.axis = sec_axis(~ . / coeff,
      name = "Effort (log10) (coloured lines) or CPUE (broken line)",
      breaks = seq(7, 12, by = 1)
    ))
}

# cpue
p +
  geom_line(
    data = CPUE, aes(x = year, y = CPUE.rel * coeff), colour = 1,
    linewidth = 1.1, alpha = 1, linetype = 2
  ) +
  theme(
    panel.background = element_blank(),
    # panel.border = element_rect(colour = "black", fill = NA, size = 1), # 1.15
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.spacing = unit(0.1, "cm"),
    legend.direction = "horizontal",
    panel.spacing = unit(0.1, "cm"),
    legend.text = element_text(size = 8), #14
    axis.title = element_text(size = 7), #16
    axis.text = element_text(size = 8), #14
    legend.position = c(0.31, 0.87), # "bottom"
    legend.box = "vertical",
    legend.margin = margin(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  guides(colour = guide_legend(nrow = 2)) +
  scale_fill_viridis_d() +
  scale_color_manual(values = c("cadetblue4", "coral4", "forestgreen", "cornflowerblue", "firebrick1"))

ggsave(paste0(hndl, "Outputs/Figure2c_combined_countries.tiff"),
  width = 18.4,
  height = 6.37,
  units = "cm",
  dpi = 300, compression = "lzw"
)
