##' Title: Data preparation of the different gridded population products
##' Description: In this R-script we clip the population datasets to country borders
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# libraries
library(dplyr)
library(terra)
library(rgdal)
library(doParallel)
library(foreach)
library(snow)
library(sf)
library(rgdal)
library(gdalUtils)

# load in data sets
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
ghs_pop <- rast("data/raw/rPopulation/ghs_pop/GHS_POP_E2015_GLOBE_R2019A_54009_250_V1_0.tif")
gpwv4 <- rast("data/raw/rPopulation/gpwv4/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_2020_30_sec_tif/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
worldpop_constr <- c(list.files("data/raw/rPopulation/worldpop/constrained/", recursive = T, full.names = T))
worldpop_unconstr <- c(list.files("data/raw/rPopulation/worldpop/unconstrained", recursive = T, full.names = T))
landscan <- rast("data/raw/rPopulation/landscan/landscan_2019.tif")
admin <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")

# HRSL Facebook CIESN dataset was downloaded in tiles from: https://data.humdata.org/dataset/highresolutionpopulationdensitymaps 
# here we mosaic the tiles to one overarching dataset.
facebook_files <- c(list.files("data/raw/rPopulation/rPopulation/facebook/", recursive = T, full.names = T, pattern = "*.tif$"))
mosaic_rasters(gdalfile=facebook_files, dst_dataset = "data/raw/rPopulation/facebook/facebook_mosaic.tiff", of="GTiff")

# subset country names so they can be used for saving and for loop
countries <- admin$GID_0

# set-up the parallel processing
cores <- 2 # not to overload your computer
cl <- makeCluster(cores) 
registerDoParallel(cl)

# the clipping function for landscan, HRSL, GPWv4, GHS-POP
system.time(foreach(i = 1:nrow(admin), .packages= c("terra","foreach", "doParallel", "sf")) %dopar% { 
  p = landscan # get the pop data layer (change name depending on dataset you want to process)
  a = admin[i,] # get the i'th row of `admin_boundaries`.
  a = st_transform(a, crs(p))
  a = vect(a)
  r = terra::crop(p, a) # crop: function to clip to bounding box of each polygon
  m = terra::mask(r, a) # mask: function to clip  to exact country borders
  writeRaster(m, paste0("data/processed/rPopulation_clipped/landscan/landscan_pop_", countries[i], ".tif"), overwrite=T) # change output directory based on population layer
})


# the clipping function for WorldPop top-down unconstrained and top-down constrained
system.time(foreach(i = 1:nrow(admin), .packages= c("terra","foreach", "doParallel", "sf")) %dopar% { 
  p = rast(worldpop_constr_todo[i]) # get the pop data layer, change for worldpop uncostrained 
  a = admin[i,] # get the i'th row of `admin_boundaries`.
  a = st_transform(a, crs(p))
  a = vect(a)
  r = terra::crop(p, a) # crop: function to clip to bounding box of each polygon
  m = terra::mask(r, a) # mask: function to clip  to exact country borders
  writeRaster(m, paste0("data/processed/rPopulation_clipped/worldpop/constrained/wp_popcount_constr_unadj_", countries[i], ".tif"), overwrite=T) # change output directory for worldpop unconstrained
})

## THE END ##
