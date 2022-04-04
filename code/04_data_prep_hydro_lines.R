##' Title: Data download and preparation national hydrography networks (WATER LINES)
##' Description: In this R-script we download OSM data through the osmextract library and project the road network to the right projection system. 
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# libraries
library(dplyr)
library(sf)
library(rgdal)
library(doParallel)
library(foreach)
library(snow)
library(osmextract)

# load in all datasets:
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
admin <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp") # this needs to be readOGR because bounding box is not recognized with st_read in cropping step
projections <- read.csv("data/raw/projections/projections.csv")

# fetch projection data from EPSG
epsg <- make_EPSG()
projections <- left_join(projections, epsg, by = c("epsg_code"  ="code"))

# join projection data to shapefile of boundaries. Subset file names to use in for loop. 
admin <- left_join(admin, projections, by = c("GID_0" = "country_code")) # merge the projections to the spatial data
countries <- admin$NAME_0

# assign cores for parallel processing 
cores <- 3 # not to overload your computer
cl <- makeCluster(cores) 
registerDoParallel(cl)

# download OSM data for each country and project it in the right projection system
system.time(foreach(i = 1:length(admin), .packages= c("sf", "osmextract")) %dopar% {
  water = oe_get(
    paste(countries[i]),
    quiet = FALSE,
    query = "SELECT * FROM 'lines' WHERE waterway IS NOT NULL")
  st_crs(water) = st_crs(admin)
  water = st_transform(water, admin$prj4[i])
  st_write(water, paste0("data/processed/vWater_lines/Water_lines_", file_name[i], ".shp"), append = F)
})

## THE END ##