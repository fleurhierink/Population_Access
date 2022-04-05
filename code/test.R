##' Title: Script for the data preparation of input data for AccessMod
##' Author: Fleur Hierink (modified by Pablo Timoner)
##' Organisation: University of Geneva
##' Date: 11 November 2021

# Libraries -----------------------------------------------
# Efficient for shapefiles
if (!require("sf")) install.packages("sf"); library("sf")
# New library raster
if (!require("terra")) install.packages("terra"); library("terra")
# Still useful for some functions
if (!require("raster")) install.packages("raster"); library("raster")
# Downloading openstreetmap data on roads and rivers
if (!require("osmextract")) install.packages("osmextract"); library("osmextract")
# Underlying support for spatial analysis
if (!require("rgdal")) install.packages("rgdal"); library("rgdal")
# Underlying support for spatial analysis
if (!require("rgeos")) install.packages("rgeos"); library("rgeos")
# Zonal statistics
if (!require("exactextractr")) install.packages("exactextractr"); library("exactextractr")
# Rasterize fast
if (!require("fasterize")) install.packages("fasterize"); library("fasterize")
# Reclassifying roads
if (!require("dplyr")) install.packages("dplyr"); library("dplyr")

# Initial parameters -----------------------------------------------
# Country, code and projection reference
main_dir <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/Tests/" # Main working directory
region <- "MALI"
code_region <- "MLI" # https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
ref_syst <- "EPSG:32630" # https://epsg.io/
input_names <- c("rDEM","rPopulation","rLandcover","vRoads","vWaterLines","vWaterPoly","vZones","vFacilities")
resol <- 100 # project resolution

# Create directories if they don't exist and set workspace -----------------------------------------------

dir.create(paste0(main_dir,toupper(region)))
setwd(paste0(main_dir,toupper(region)))
dir.create(paste0(main_dir,toupper(region),"/data"))
for(type in c("raw","processed")){
  dir.create(paste0(main_dir,toupper(region),"/data/",type))
  for(input_name in input_names){
    dir.create(paste0(main_dir,toupper(region),"/data/",type,"/",input_name))
  }
}
