### Code to produce Figures 1C and 1D in Dedman et al. Ecological roles and importance of sharks in the Anthropocene Ocean.


library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(ggpubr)


rm(list = ls())

###### GRAVITY DATA ######

grav <- read.csv("/data/totgravity_data.csv")
str(grav)

nozero <- grav %>% filter(Grav_tot > 0)
grav$Grav_tot_plusmin <- grav$Grav_tot + min(nozero$Grav_tot) # Add minimum non-zero gravity value for transformation
grav$Grav_tot_log <- log(grav$Grav_tot_plusmin)


###### FINPRINT REEF DATA ######

fpdat <- read.csv("/data/FinPrint_reefs_gravity.csv")
str(fpdat)

grv2 <- grav
grv2$mean_maxn_perdrop <- "NA"
grv2 <- grv2[, c("Grav_tot_plusmin", "mean_maxn_perdrop")]
colnames(grv2) <- c("grav_total_dec", "mean_maxn_perdrop")
grv2$mean_maxn_perdrop <- as.numeric(grv2$mean_maxn_perdrop)


###### GRAVITY AND MAXN PLOT ######

df <- bind_rows(fpdat, grv2)

attfin <- attributes(scale(log(df$grav_total_dec)))
min(df$grav_total_dec) # This should be the "0" in mybreaksfin
mylabelsfin <- c(0, 1, 10, 100, 1000, 10000, 100000)
mybreaksfin <- scale(log(c(4.105e-07, 1, 10, 100, 1000, 10000, 100000)), attfin$`scaled:center`, attfin$`scaled:scale`)

formatternewnew <- function(x) {
  x / 1500
} # To format 1st y-axis labels

options(scipen = 6)

quantile(grav$Grav_tot_plusmin)
perc25 <- scale(log(2.5529737474), attfin$`scaled:center`, attfin$`scaled:scale`)
perc50 <- scale(log(22.1285958925), attfin$`scaled:center`, attfin$`scaled:scale`)
perc75 <- scale(log(103.7238080165), attfin$`scaled:center`, attfin$`scaled:scale`)
perc0 <- scale(log(0.0000004105), attfin$`scaled:center`, attfin$`scaled:scale`) # Just to check (-4.91)
min(scale(log(df$grav_total_dec))) # This should match

# Plot
gravplot <- ggplot(data = df, aes(x = scale(log(grav_total_dec)))) +
  geom_histogram(aes(y = after_stat(count)), colour = "#95D840FF", fill = "#95D840FF", alpha = 0.3, boundary = 0.1, bins = 30) +
  scale_x_continuous(labels = scales::comma(mylabelsfin), breaks = mybreaksfin, expand = c(0.01, 0.01)) +
  geom_smooth(aes(y = mean_maxn_perdrop * 1500), colour = "#238A8DFF", se = T, fill = "#238A8DFF") +
  scale_y_continuous(labels = formatternewnew, breaks = seq(0, 4500, 1500), sec.axis = sec_axis(~., name = "Number of coral reefs (total 27,156)", breaks = seq(0, 4500, 1500), labels = seq(0, 4500, 1500)), expand = c(0.015, 0.015)) +
  labs(x = "Total gravity", y = "Reef shark MaxN per BRUVS drop") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.margin = margin(0.2, 0.1, 0.2, 0.1, "cm")) +
  theme(axis.text = element_text(size = 4.5, colour = "black"), axis.title = element_text(size = 6, colour = "black"), axis.ticks = element_line(size = 0.3)) +
  geom_vline(xintercept = perc25, linetype = "dotted", colour = "darkgrey", linewidth = 0.8) +
  geom_vline(xintercept = perc50, linetype = "dotted", colour = "darkgrey", linewidth = 0.8) +
  geom_vline(xintercept = perc75, linetype = "dotted", colour = "darkgrey", linewidth = 0.8) +
  coord_cartesian(ylim = c(0, 4500)) # Limits y-axis without removing data
gravplot

# ggsave(gravplot, filename="grav_plot.tiff",width = 8.255, height = 5.715, units = "cm", compression = "lzw", bg = "white",dpi = 600)


###### COUNTERFACTUAL EXPECTED MAXN PLOT ######

hum <- read.csv("/humansmaxn.csv")
non <- read.csv("/nohumansmaxn.csv")

hum$type <- "humans"
non$type <- "nohumans"

cdf <- rbind(hum, non)
colnames(cdf) <- c("maxn", "type")

cfplot <- ggplot(cdf, aes(x = maxn, color = type, fill = type)) +
  geom_density(alpha = 0.5, outline.type = "full", bw = 0.6) +
  scale_x_continuous(name = "Expected relative abundance of reef sharks (MaxN per BRUVS drop)", limits = c(0, 8), breaks = seq(0, 8, 1), expand = c(0, 0)) +
  scale_y_continuous(name = "Density", limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1), labels = seq(0, 0.5, 0.1), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(size = 5, colour = "black"), axis.title = element_text(size = 6, colour = "black")) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("#F95D3E", "#7B2382"), labels = c("Status quo", "No humans")) +
  scale_colour_manual(values = c("#F95D3E", "#7B2382"), labels = c("Status quo", "No humans")) +
  theme(legend.title = element_blank(), legend.position = c(0.86, 0.88), legend.margin = margin(t = 0, r = 1, b = 1, l = 1, unit = "mm"), legend.spacing.y = unit(1, "mm"), legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_line(size = 0.3))
cfplot

# ggsave(cfplot, filename="counterfactual_plot.tiff",width = 8.255, height = 5.715, units = "cm", compression = "lzw", bg = "white",dpi = 600)
