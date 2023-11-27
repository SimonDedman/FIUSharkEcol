# Ecological roles and importance of sharks in the Anthropocene Ocean

Scripts for FIU-led manuscript *Ecological roles and importance of sharks in the Anthropocene Ocean*, Dedman et al 2023/4.

Figure 1A: Erin TO DO.

Figure 1B_Matias.R: powers Figure 1B: Shark landings, relative effort, and CPUE through time. Uses "/data/FAO catch by nation and year.csv"" and "/data/TotalEffortby_FishingCountry_LengthBoat_Gear_Sector.csv". Written by Matias Braccini, matias.Braccini@dpird.wa.gov.au, 2023, edited & packaged by Simon Dedman, simondedman@gmail.com, 2023.

SankeyAlluvial.R: powers Figure 3B, Sankey/Alluvial plot, uses "/data/SankeyAlluvial.csv". Written, edited, & packaged by Simon Dedman, simondedman@gmail.com, 2023.

TraitsRidgeplot.R: powers Figure 5B, traits ridge plot, uses "/data/Traitsridgeplot.xlsx". Written, edited, & packaged by Simon Dedman, simondedman@gmail.com, 2023.

# Counterfactual plots, 1C & 1D

Code and data to reproduce counterfactual model analyses. Written, edited, & packaged by Natalie Klinnard, natalie.klinard@dal.ca & natalie.klinard@gmail.com, 2023.

Main data file is available as part of MacNeil et al. 2020 "Global status and conservation potential of reef sharks" here: https://www.dropbox.com/s/wjwld9lrfl1a7pk/FinPrint_Set_Data.csv?dl=0

"/data/FinPrint_Set_Data.csv" and "/data/Location_Covariates.csv" are from MacNeil et al. 2020 and are used in "/Python/EROS_counterfactual_models.ipynb" to produce final distributions that are used in "/R/EROS_gravity_maxn_plots.R" to plot Figure 1D: *Counterfactual predictions of relative abundance of reef sharks in the presence (status quo) and absence of humans*.

"/data/FinPrint_reefs_gravity.csv" and "/data/totgravity_data.csv" are used in "/R/EROS_gravity_maxN_plots.R" to produce Figure 1C: *Reef shark relative abundance and number of global coral reefs along a gradient of human pressure (total gravity)*. "/data/FinPrint_reefs_gravity.csv" contains the gravity and average MaxN values associated with all 371 reefs sampled as part of FinPrint data associated with MacNeil et al. 2020. "/data/totgravity_data.csv" contains total gravity decimal values for a global distribution of coral reefs created by Dr. Eva Maire, Lancaster University.
