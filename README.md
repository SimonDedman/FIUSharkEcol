[![DOI](https://zenodo.org/badge/468063603.svg)](https://zenodo.org/doi/10.5281/zenodo.10235359)

# Ecological roles and importance of sharks in the Anthropocene Ocean

Scripts for FIU-led manuscript *Ecological roles and importance of sharks in the Anthropocene Ocean*, Dedman et al 2023/4.

## EROS_Fig1A_share.Rmd

![](./figures/fig1a.png)

Figure 1A: Millennial-scale changes in relative reef shark abundance. i: Perceived abundance of sharks in Caribbean Panama inferred from archaeological, historical, ecological, and fisheries records divided into cultural periods in Panama’s history (based on data in (18). ii: Falling dermal denticle accumulation rates (proxy for relative abundance) suggest 71% (Caribbean Panama, black circles) and 75% (Dominican Republic, gray circles) declines in reef shark abundances since the mid-Holocene. Modified from Dillon et al 2021.

### Data-specific information for Fig1A

Data that are missing or not applicable are indicated with a period (“.”)

Panel i. historical ecology

Dataset: "/data/Fig1A_histecol_share.csv"
Description: Perceived ecological state of sharks in Caribbean Panama, based on archaeological, historical, ecological, and fisheries records spanning ~4ka-2020. Records were separated into seven cultural periods in Panama's history (see Dillon et al. 2021 for methodological details) and were independently reviewed by 17 individuals, most of whom identified as having a background in ecology, environmental science, paleobiology, or the geosciences (IRB Protocol #3-20-0211). Data are published in Dillon et al. 2021.
Dates collected: 2020
Nrows: 119 (excludes header)
Ncols: 3

a. response: individual reviewer id
b. time_period: temporal bin into which records were broken, consisting of pre_human (>6 ka), pre_contact (~6 ka-1500 CE), colonial (1501-1821 CE), post_colonial (1822-1903 CE), modern_one (1904-1958 CE), modern_two (1959-1999 CE), and modern_three (2000-2020 CE) (categorical)
c. score: ecological state score representing perceived abundance - scores were assigned estimated numerical values of %depletion following Kittinger et al. 2011

pristine: 5% (0-10%)
abundant: 20% (11-29%)
uncommon: 45% (30-59%)
rare: 75% (60-89%)
ecologically extinct: 95% (90-99%)
globally extinct: 100% (100%)


Panel ii. denticle accumulation
Dataset: "/data/Fig1A_denticles_share.csv"
Description: Fossil shark dermal denticles isolated from bulk samples collected from mid-Holocene and modern coral reefs in the Dominican Republic and Panama, Caribbean Sea. The data from Panama are published in Dillon et al. 2021.
Dates collected: 2014-2016
Nrows: 75 (excludes header)
Ncols: 10

a. basin: all data collected in the Caribbean (categorical)
b. region: sampling region in the Caribbean (categorical)
c. site_name: name of sampling location (categorical)
d. sample_unique: sample number, either in the format collector-year-site-replicate or collector-year-site-subsite-replicate
e. age_group: time period, based on whether the reef was fossil or modern (categorical)
f. sampling_group: randomly assigned sampling group for Dominican Republic samples where denticle counts in the individual replicates were low
g. total_dd_count: total count of denticles (discrete)
h. fragment_count: total count of denticle fragments, where less than half of the denticle crown is present, not included in total_dd_count (discrete)
i. sed_weight_kg: estimated number of years in the sample (continuous)
j. no_years: estimated number of years in the sample (continuous)

Post-processing: After producing the figures in R (R-4.2.2), minor formatting was done using Illustrator. The timeline (with time period labels) in panel i was added and the text was resized for consistency.

Written, edited, & packaged by Erin Dillon, emdillon23@gmail.com, 2023.

### Citations

Dillon, E. M., D. J. McCauley, J. M. Morales-Saldaña, N. D. Leonard, J. Zhao, and A. O’Dea. 2021: Fossil dermal denticles reveal the preexploitation baseline of a Caribbean coral reef shark community. Proceedings of the National Academy of Sciences 118:e2017735118.

Kittinger, J. N., J. M. Pandolfi, J. H. Blodgett, T. L. Hunt, H. Jiang, K. Maly, L. E. McClenachan, J. K. Schultz, and B. A. Wilcox. 2011: Historical Reconstruction Reveals Recovery in Hawaiian Coral Reefs. PLoS ONE 6:e25460.

## Figure 1B_Matias.R

![](./figures/fig1b.png)

Figure 1B: Shark landings (Y1; blue area), relative effort (Y2; lines), and CPUE (Y2; dashed line) through time.

Uses "/data/FAO catch by nation and year.csv"" and "/data/TotalEffortby_FishingCountry_LengthBoat_Gear_Sector.csv". Written by Matias Braccini, matias.Braccini@dpird.wa.gov.au, 2023, edited & packaged by Simon Dedman, simondedman@gmail.com, 2023.

## Counterfactual plots, 1C & 1D

![](./figures/fig1c.png)

Figure 1C: Reef shark relative abundance (MaxN per drop; Y1; blue line with 95% confidence interval; (MacNeil et al 2020)) and number of global coral reefs (Y2; green histogram; vertical dotted lines are quartiles, SERF (Cinner et al 2016)) along a gradient of human pressure (total gravity (Cinner et al 2018); SERF (Cinner et al 2016)). Shark abundances are highest on remote reefs, which are rare.

![](./figures/fig1d.png)

Figure 1D: Counterfactual predictions of relative abundance of reef sharks with (status-quo) and without humans (SERF (Cinner et al 2016) models set human-related variables to zero). Expected relative abundance was estimated using MaxN measurements in a global study from 371 reefs (MacNeil et al 2020)).

Code and data to reproduce counterfactual model analyses. Written, edited, & packaged by Natalie Klinnard, natalie.klinard@dal.ca & natalie.klinard@gmail.com, 2023.

Main data file is available as part of MacNeil et al. 2020 "Global status and conservation potential of reef sharks" here: https://www.dropbox.com/s/wjwld9lrfl1a7pk/FinPrint_Set_Data.csv?dl=0

"/data/FinPrint_Set_Data.csv" and "/data/Location_Covariates.csv" are from MacNeil et al. 2020 and are used in "/Python/EROS_counterfactual_models.ipynb" to produce final distributions (“/data/humansmaxn.csv” and “/data/nohumansmaxn.csv”) that are used in "/R/EROS_gravity_maxn_plots.R" to plot Figure 1D: Counterfactual predictions of relative abundance of reef sharks in the presence (status quo) and absence of humans.

"/data/FinPrint_reefs_gravity.csv" and "/data/totgravity_data.csv" are used in "/R/EROS_gravity_maxN_plots.R" to produce Figure 1C: *Reef shark relative abundance and number of global coral reefs along a gradient of human pressure (total gravity)*. "/data/FinPrint_reefs_gravity.csv" contains the gravity and average MaxN values associated with all 371 reefs sampled as part of FinPrint data associated with MacNeil et al. 2020. "/data/totgravity_data.csv" contains total gravity decimal values for a global distribution of coral reefs created by Dr. Eva Maire, Lancaster University.

### Citations

J. E. Cinner, C. Huchery, M. A. MacNeil, N. A. Graham, T. R. McClanahan, J. Maina, E. Maire, J. N. Kittinger, C. C. Hicks, C. Mora, E. H. Allison, S. D’Agata, A. Hoey, D. A. Feary, L. Crowder, I. D. Williams, M. Kulbicki, L. Vigliola, L. Wantiez, G. Edgar, R. D. Stuart-Smith, S. A. Sandin, A. Green, M. J. Hardt, M. Beger, A. Friedlander, S. J. Campbell, K. E. Holmes, S. K. Wilson, E. Brokovich, A. J. Brooks, J. J. Cruz-Motta, D. J. Booth, P. G. C. Chabanet, M. Tupper, S. C. Ferse, U. R. Sumaila, D. Mouillot, Bright spots among the world’s coral reefs. Nature. 535, 416–419 (2016).

J. E. Cinner, E. Maire, C. Huchery, M. A. MacNeil, N. A. Graham, C. Mora, T. R. McClanahan, M. L. Barnes, J. N. Kittinger, C. C. Hicks, S. D’Agata, A. S. Hoey, G. G. Gurney, D. A. Feary, I. D. Williams, M. Kulbicki, L. Vigliola, L. Wantiez, G. J. Edgar, R. D. Stuart-Smith, S. A. Sandin, A. Green, M. J. Hardt, M. Beger, A. M. Friedlander, S. K. Wilson, E. Brokovich, A. J. Brooksa, J. J. Cruz-Mottab, D. J. Boothc, P. Chabanetd, C. Goughe, M. Tupperf, S. C. Ferseg, U. R. Sumailah, S. Pardede, D. Mouillot, Gravity of human impacts mediates coral reef conservation gains. Proceedings of the National Academy of Sciences. 115, E6116–E6125 (2018).

M. A. MacNeil, D. D. Chapman, M. Heupel, C. A. Simpfendorfer, M. Heithaus, M. Meekan, E. Harvey, J. Goetze, J. Kiszka, M. E. Bond, L. M. Currey-Randall, C. W. Speed, C. S. Sherman, M. J. Rees, V. Udyawer, K. I. Flowers, G. Clementi, J. Valentin-Albanese, T. Gorham, M. S. Adam, K. Ali, F. Pina-Amargós, J. A. Angulo-Valdés, J. Asher, L. García Barcia, O. Beaufort, C. Benjamin, A. T. Bernard, M. L. Berumen, S. Bierwagen, E. Bonnema, R. M. Bown, D. Bradley, E. Brooks, J. J. Brown, D. Buddo, P. Burke, C. Cáceres, D. Cardeñosa, J. C. Carrier, J. E. Caselle, V. Charloo, T. Claverie, E. Clua, J. E. Cochran, N. Cook, J. Cramp, B. D’Alberto, M. de Graaf, M. Dornhege, A. Estep, L. Fanovich, N. F. Farabaugh, D. Fernando, A. L. Flam, C. Floros, V. Fourqurean, R. Garla, K. Gastrich, L. George, R. Graham, T. Guttridge, R. S. Hardenstine, S. Heck, A. C. Henderson, H. Hertler, R. Hueter, M. Johnson, S. Jupiter, D. Kasana, S. T. Kessel, B. Kiilu, T. Kirata, B. Kuguru, F. Kyne, T. Langlois, E. J. Lédée, S. Lindfield, Luna-Acosta, J. Maggs, B. M. Manjaji-Matsumoto, A. Marshall, P. Matich, E. McCombs, D. McLean, L. Meggs, S. Moore, S. Mukherji, R. Murray, M. Kaimuddin, S. J. Newman, J. Nogués, C. Obota, O. O’Shea, K. Osuka, Y. P. Papastamatiou, N. Perera, B. Peterson, A. Ponzo, A. Prasetyo, L. S. Quamar, J. Quinlan, A. Ruiz-Abierno, E. Sala, M. Samoilys, M. Schärer-Umpierre, A. Schlaff, N. Simpson, A. N. Smith, L. Sparks, A. Tanna, R. Torres, M. J. Travers, M. van Zinnicq Bergmann, L. Vigliola, J. Ward, A. M. Watts, C. Wen, E. Whitman, A. J. Wirsing, A. Wothke, E. Zarza-Gonzâlez, J. E. Cinner, Global status and conservation potential of reef sharks. Nature. 583, 801–806 (2020).

## SankeyAlluvial.R

![](./figures/fig3b.png)

Figure 3B: Alluvial plot of studied sharks’ ecotypes, ecological roles, and strength of evidence from table S1 studies. Competition and/or bottom-up processes binned due to small sample size. Effect size and strength of evidence rated by 30 investigators’ expert opinions scoring source paper metrics on a low/medium/high scale. * = Macropredatory sharks: Pelagic. BU = bottom up, NVS = nutrient vector / storage, SAF = sharks as food, EAE = excretion and egestion.

Uses "/data/SankeyAlluvial.csv". Written, edited, & packaged by Simon Dedman, simondedman@gmail.com, 2023.

## TraitsRidgeplot.R

![](./figures/fig5b.png)

Figure 5B: Species richness and abundance of sharks decreases along a gradient of market gravity (human impact), along with traits influencing shark movement and trophic interactions. Communities lose wide-ranging individuals that connect habitats, have flexible habitat adaptations that increase resilience, and feed at upper trophic levels. Data from (Simpfendorfer et al 2023, Dulvy et al 2021, Iliou et al 2023, Froese & Pauly 2023).

Uses "/data/Traitsridgeplot.xlsx". Written, edited, & packaged by Simon Dedman, simondedman@gmail.com, 2023. Panels arranged and annotated by Kylene Gilmore, kylenegilmoreart.com.

### Citations

C. A. Simpfendorfer, M. R. Heithaus, M. R. Heupel, M. A. Macneil, M. Meekan, E. Harvey, C. S. Sherman, L. M. Currey-Randall, J. S. Goetze, J. J. Kiszka, M. J. Rees, C. W. Speed, V. Udyawer, M. E. Bond, K. I. Flowers, G. M. Clementi, J. Valentin-Albanese, M. S. Adam, K. Ali, J. Asher, E. Aylagas, O. Beaufort, C. Benjamin, A. T. F Bernard, M. L. Berumen, S. Bierwagen, C. Birrell, E. Bonnema, R. M. K Bown, E. J. Brooks, J. J. Brown, D. Buddo, P. J. Burke, C. Cceres, M. Cambra, D. Cardeosa, J. C. Carrier, S. Casareto, J. E. Caselle, V. Charloo, J. E. Cinner, T. Claverie, E. E. G Clua, J. E. M Cochran, N. Cook, J. E. Cramp, B. M. Dalberto, M. D. Graaf, M. C. Dornhege, M. Espinoza, A. Estep, L. Fanovich, N. F. Farabaugh, D. Fernando, C. E. L Ferreira, C. Y. A Fields, A. L. Flam, C. Floros, V. Fourqurean, L. Gajdzik, L. G. Barcia, R. Garla, K. Gastrich, L. George, T. Giarrizzo, R. Graham, T. L. Guttridge, V. Hagan, R. S. Hardenstine, S. M. Heck, A. C. Henderson, P. Heithaus, H. Hertler, M. H. Padilla, R. E. Hueter, R. W. Jabado, J.-C. Joyeux, V. Jaiteh, M. Johnson, S. D. Jupiter, M. Kaimuddin, D. Kasana, M. Kelley, S. T. Kessel, B. Kiilu, T. Kirata, B. Kuguru, F. Kyne, T. Langlois, F. Lara, J. Lawe, E. J. I Lde, S. Lindfield, A. Luna-Acosta, J. Q. Maggs, B. M. Manjaji-Matsumoto, A. Marshall, L. Martin, D. Mateos-Molina, P. Matich, E. Mccombs, A. Mcivor, D. Mclean, L. Meggs, S. Moore, S. Mukherji, R. Murray, S. J. Newman, J. Nogus, C. Obota, D. Ochavillo, O. O’Shea, K. E. Osuka, Y. P. Papastamatiou, N. Perera, B. Peterson, C. R. Pimentel, F. Pina-Amargs, H. T. Pinheiro, A. Ponzo, A. Prasetyo, L. M. Sjamsul Quamar, J. R. Quinlan, J. A. Reis-Filho, H. Ruiz, A. Ruiz-Abierno, E. Sala, P. S. De-Len, M. A. Samoilys, W. R. Sample, M. S.- Umpierre, A. M. Schlaff, K. Schmid, S. N. Schoen, N. Simpson, A. N. H Smith, J. L. Y Spaet, L. Sparks, T. Stoffers, A. Tanna, R. Torres, M. J. Travers, M. V. Zinnicq Bergmann, L. Vigliola, J. Ward, J. D. Warren, A. M. Watts, C. K. Wen, E. R. Whitman, A. J. Wirsing, A. Wothke, E. Zarza-Gonzlez, D. D. Chapman, Widespread diversity deficits of coral reef sharks and rays. Science. 380, 1155–1160 (2023).

N. K. Dulvy, N. Pacoureau, C. L. Rigby, R. A. Pollom, R. W. Jabado, D. A. Ebert, B. Finucci, C. M. Pollock, J. Cheok, D. H. Derrick, K. B. Herman, C. S. Sherman, W. J. VanderWright, J. M. Lawson, R. H. Walls, J. K. Carlson, P. Charvet, K. K. Bineesh, D. Fernando, G. M. Ralph, J. H. Matsushiba, C. Hilton-Taylor, S. V. Fordham, C. A. Simpfendorfer, Overfishing drives over one-third of all sharks and rays toward a global extinction crisis. Current Biology (2021), doi:10.1016/j.cub.2021.08.062.

A. S. Iliou, W. Vanderwright, L. Harding, D. M. Jacoby, N. L. Payne, N. K. Dulvy, Tail shape and the swimming speed of sharks. Royal Society Open Science. 10, 231127 (2023).

R. Froese & D. Pauly. FishBase version 10/2023. www.fishbase.org. Fisheries Centre, University of British Columbia Vancouver, BC, Canada
