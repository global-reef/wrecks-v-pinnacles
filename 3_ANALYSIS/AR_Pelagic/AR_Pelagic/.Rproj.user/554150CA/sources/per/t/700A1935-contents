### run all scripts to update analysis


# --------------------------
######### 0. Set Analysis Date & Create Output Folder #######
# --------------------------
# Enter the date for this analysis
analysis_date <- "2025_05_15"  # Update these 3 for each analysis run
# file path (adjust date for correct date)
file_path <- "~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/2_DATA/2025.05.15_ArtificialReefs_MASTERdata.csv"
file_path_timed <- "~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/2_DATA/2025.05.15_TimedFishSurveys_Shallow_MASTER.csv"
# --------------------------
raw_fish <- read.csv(file_path, stringsAsFactors=TRUE, strip.white=TRUE)


# Create a folder named with the date inside the working directory
output_dir <- file.path(getwd(), paste0("Analysis_", analysis_date))
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# --------------------------
######### 1. Source all Scripts #######
# --------------------------
# data clean up 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/1_DataClean.R")
# exploratory analysis 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/1.2_Exploratory.R")
# add in timed fish swims 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/1.3_AddTimedFishSwims.R")

# run bayesian multivariate regression # more suited to our analysis than BMNR
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/1.4_BMV_FG.R")
# plot results of MV-Reg
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/1.5FG_plots.R")
# shannon diversity
# source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/1.6-FG_ShDiversity.R")
# run functional group analysis by zones 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/1.8_FG_Zones.R")


# run species-specific bayesian multivariate regression 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/3_Spp_BayesMultiVar.R")
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/3_ANALYSIS/AR_Pelagic/AR_Pelagic/3.1_Spp_BMV_SubsetSpp.R") 
