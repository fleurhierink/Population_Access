##' Title: Data preparation digital elevation model
##' Description: In this R-script we clip and project the SRTM digital elevation to GADM country borders for all countries in sub-Saharan Africa
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# libraries
library(dplyr)
library(raster)
library(rgdal)
library(doParallel)
library(foreach)
library(snow)


# load in all datasets 
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
srtm <- rast("data/raw/DEM/srtm_mosaic.tif")
admin <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")
projections <- read.csv("data/raw/projections/projections.csv")

# fetch projection data from EPSG
epsg <- make_EPSG()
projections <- left_join(projections, epsg, by = c("epsg_code"  ="code"))

# only subset country names that have not been processed yet
# create DEM for each country in ssa
admin <- left_join(admin, projections, by = c("GID_0" = "country_code")) # merge the projections to the spatial data
file_name <- admin$NAME_0 # to integrate country name in saving step later

# SRTM 
system.time(for(i in 1:nrow(admin)) {
  d = srtm
  a = admin %>% 
    filter(GID_0 == file_name[i])
  a = vect(a)
  r = terra::crop(d, a)
  m = terra::mask(r, a)
  p = terra::project(m, admin$prj4[i], method = "near")
  writeRaster(p, paste0("data/processed/rDEM/dem_", file_name[i], ".tif"), overwrite=T)
}) 

## THE END ##