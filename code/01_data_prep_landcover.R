##' Title: Data preparation land cover
##' Description: In this R-script we clip and project the copernicus 2019 land cover to GADM country borders for all countries in sub-Saharan Africa
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# libraries
library(dplyr)
library(terra)
library(sf)
library(rgdal)
library(doParallel)
library(foreach)
library(snow)

# load in datasets: 
# the landcover dataset can be obtained from: Buchorn et al (2020) DOI: 10.5281/zenodo.3939050 
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
landcover <- rast("data/raw/land cover/2019/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")
admin <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp") 
projections <- read.csv("data/raw/projections/projections.csv")

# fetch projection data from EPSG
epsg <- make_EPSG()
projections <- left_join(projections, epsg, by = c("epsg_code"  ="code"))

# create landcover for each country in ssa
admin <- left_join(admin, projections, by = c("GID_0" = "country_code")) # merge the projections to the spatial data
file_name <- admin$GID_0 # to integrate country name in saving step later

# apply the  function
system.time(for(i in 1:nrow(admin)) {
  l = landcover
  a = admin %>% 
    filter(GID_0 == file_name[i])
  a = vect(a)
  r = terra::crop(l, a)
  m = terra::mask(r, a)
  p = terra::project(m, admin$prj4[i], method = "near")
  values(p)[values(p) == 0] <- NA
  writeRaster(p, paste0("data/processed/rLandcover/landcover_", file_name[i], ".tif"), overwrite=T)
}) 

## THE END ##