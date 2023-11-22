# EROS gravity plot code by Natalie Klinnard, 2023, natalie.klinard@dal.ca, natalie.klinard@gmail.com
# Tweaked for upload by Simon Dedman, simondedman@gmail.com

library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(ggpubr)


### Global coral reef gravity values - DECIMAL GRAVITY DATA

rm(list = ls())

dat <- readOGR("TotGravity.shp") # Read in shapefile
str(dat)
dat2 <- data.frame(dat)

nozero <- dat2 %>% filter(Grav_tot > 0) # Josh said just add minimum gravity value before log transforming so let's do that
dat2$Grav_tot_new <- dat2$Grav_tot + min(nozero$Grav_tot)
dat2$Grav_tot_newlog <- log(dat2$Grav_tot_new)

plot <- ggplot(dat2, aes(x = scale(log(Grav_tot_new)))) +
  geom_density() # In Josh's paper they log-transformed and standardized..so I will too
plot

# In Josh's paper they back-calculated to arithmetic units in his paper so as not to confused people with log x-axis scale so will do
plot2 <- ggplot(dat2, aes(x = Grav_tot_newlog)) + # Tried this but x-axis units ugly?
  geom_density() +
  scale_x_continuous(breaks = pretty(dat2$Grav_tot_newlog), labels = exp(pretty(dat2$Grav_tot_newlog)))
plot2

# This page helped figure out back calculating log-data to arithmetic scale done below: https://stackoverflow.com/questions/64175368/r-unscale-and-back-transform-plot-axis-or-use-axis-from-original-data-column
att <- attributes(scale(log(dat2$Grav_tot_new)))
mingrav <- min(dat2$Grav_tot_new) # Use this value as the "0" in mybreaks so "0" will show up on x-axis
mylabels <- c(0, 1, 10, 100, 1000, 10000)
mybreaks <- scale(log(c(4.105e-07, 1, 10, 100, 1000, 10000)), att$`scaled:center`, att$`scaled:scale`)

plot3 <- ggplot(data = dat2, aes(scale(log(Grav_tot_new)))) +
  geom_density() +
  scale_x_continuous(labels = mylabels, breaks = mybreaks)
plot3 # Looks pretty good compared to Fig 5b in Cinner paper (https://www.pnas.org/doi/pdf/10.1073/pnas.1708001115)




### Now need to bring in FinPrint data to plot MaxN vs gravity for reefs sampled and combine plots
fpdat <- read.csv(paste(wd, "/FPstudyFinal.csv", sep = ""))
str(fpdat)

xdat <- fpdat[, c("reef_id", "MaxN_shark", "Grav_Total")]
xdat$prop_maxn <- xdat$MaxN_shark / max(xdat$MaxN_shark)
xdat$grav_total_dec <- xdat$Grav_Total + 0.0001

att2 <- attributes(scale(log(xdat$grav_total_dec)))
mylabels2 <- c(0, 1, 10, 100, 1000, 10000)
mybreaks2 <- scale(log(c(0.0001, 1, 10, 100, 1000, 10000)), att2$`scaled:center`, att2$`scaled:scale`)

ugh <- ggplot(data = xdat, aes(scale(log(grav_total_dec)), prop_maxn)) +
  geom_smooth() +
  scale_x_continuous(labels = mylabels2, breaks = mybreaks2) +
  scale_y_continuous()
ugh

# Colin used the average maxn for each reef by dividing total maxN per reef by number of drops per reef. So we'll try it that way too...but using our own finprint data because what he sent had fewer reefs than what's in finprint for some reason?
ydat <- xdat %>%
  group_by(reef_id) %>%
  summarize(grav_total = mean(Grav_Total), sum_maxn = sum(MaxN_shark), nsets = length(MaxN_shark))
ydat$mean_maxn_perdrop <- ydat$sum_maxn / ydat$nsets
ydat$prop_maxn_drop <- ydat$mean_maxn_perdrop / max(ydat$mean_maxn_perdrop)
ydat$grav_total_dec <- ydat$grav_total + 4.105e-07

att3 <- attributes(scale(log(ydat$grav_total_dec)))
min(ydat$grav_total_dec) # This should be the "0" in mybreaks3
mylabels3 <- c(0, 1, 10, 100, 1000, 10000)
mybreaks3 <- scale(log(c(4.105e-07, 1, 10, 100, 1000, 10000)), att3$`scaled:center`, att3$`scaled:scale`)

ugh2 <- ggplot(data = ydat, aes(scale(log(grav_total_dec)), prop_maxn_drop)) +
  geom_smooth() +
  scale_x_continuous(labels = mylabels3, breaks = mybreaks3) +
  scale_y_continuous()
ugh2 # Cool, looks pretty much like what Colin had

# K so we like plot3 and ugh2 separately, but now need to combine these both into one plot with two y-axes
# ggplot doesn't like creating dual axis plots so will combine our two separate datasets into one and see what we can do. This also should help with x-axis values being slightly different between the two datasets...
adat <- dat2
adat$prop_maxn_drop <- "NA"
adat <- adat[, c("Grav_tot_new", "prop_maxn_drop")]
colnames(adat) <- c("grav_total_dec", "prop_maxn_drop")
adat$prop_maxn_drop <- as.numeric(adat$prop_maxn_drop)

bdat <- ydat
bdat <- bdat[, c("grav_total_dec", "prop_maxn_drop")]

cdat <- bind_rows(adat, bdat)

attfin <- attributes(scale(log(cdat$grav_total_dec)))
min(cdat$grav_total_dec) # This should be the "0" in mybreaksfin
mylabelsfin <- c(0, 1, 10, 100, 1000, 10000, 100000)
mybreaksfin <- scale(log(c(4.105e-07, 1, 10, 100, 1000, 10000, 100000)), attfin$`scaled:center`, attfin$`scaled:scale`)

formatterhalf <- function(x) {
  x * 0.5
} # So that 1st y-axis labels plot are halved? Idk, some weird formatting shit because double y-axes

options(scipen = 6)
finalplot <- ggplot(data = cdat, aes(x = scale(log(grav_total_dec)))) +
  geom_density(colour = "#95D840FF", fill = "#95D840FF", alpha = 0.3) +
  scale_x_continuous(labels = scales::comma(mylabelsfin), breaks = mybreaksfin, expand = c(0.01, 0.01)) +
  geom_smooth(aes(y = prop_maxn_drop * 2), colour = "#238A8DFF", se = F) +
  scale_y_continuous(labels = formatterhalf, sec.axis = sec_axis(~., name = "Density of reefs"), expand = c(0.01, 0.01)) +
  labs(x = "Gravity", y = "Proportion of MaxN") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(axis.text = element_text(size = 10, colour = "black"), axis.title = element_text(size = 12, colour = "black"))
finalplot

# ggsave(finalplot, filename="sec2panelB.tiff",width = 7, height = 4, units = "in", compression = "lzw", bg = "white",dpi = 600)
