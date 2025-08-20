# ------------------------------------------------------------------------------
# File: global.R
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: Loads globally shared packages and definitions used across the 
#     mtcars++ Shiny dashboard app. Should be sourced at the top of app.R.
# ------------------------------------------------------------------------------

# LOAD PACKAGES
library(bs4Dash) # Dashboard UI framework
library(shiny) # Shiny functionality
library(wesanderson) # Custom color palette

# DATA INGESTION
"
For now, we're using the cleaned dataset with missing values. Imputed and 
TBD-separated values to be added later.
"
tcc <- readRDS(file = "data/tcc_na.rds")
