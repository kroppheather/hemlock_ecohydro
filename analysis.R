#######################################
## Analysis of Hemlock and Basswood  ##
## sap flux and canopy T with soil   ##
## moisture and weather              ##
#######################################

#### read in libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)

#### set up directories ----
dirScript <- "/Users/hkropp/Documents/GitHub/hemlock_ecohydro"


#### sapflow data ----
## T.L.day: average daily transpiration by species in 
## liters per day per tree and per m2 of leaf area
## sapflow.hour: average hourly sap flow by species in
## L per hour per tree and per m2 of leaf area
## liters per day per tree and per m2 of leaf area
## Tot.tree.L.day: average daily transpiration by tree in
## liters per day per tree and per m2 of leaf area

source(paste0(dirScript, "/sapflux.r"))

