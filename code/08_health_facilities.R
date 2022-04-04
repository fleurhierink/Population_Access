##' Title: Clip health facilities to each country's borders and project in the country's projection.
##' Description: In this R-script we take the health facilities for sub-Saharan Africa, as collected by Maina et al. (2019)
##' https://doi.org/10.1038/s41597-019-0142-2. And clip them to country borders to construct national facility layers. 
##' of minutes it takes to traverse a meter. Below a step-by-step overview is given. 
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# libraries
library(dplyr)
library(sf)
library(doParallel)
library(foreach)
library(snow)
library(rgdal)

# load in data
# health facilities can be downloaded from Maina et al. (2019) https://doi.org/10.1038/s41597-019-0142-2 
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
admin <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")
facilities <- st_read("data/raw/health facilities/suhsharan_health_facilities/sub-saharan_health_facilities.shp")
projections <- read.csv("data/raw/projections/projections.csv")

# fetch projection data from EPSG
epsg <- make_EPSG()
projections <- left_join(projections, epsg, by = c("epsg_code"  ="code"))

# join projection data to administrative boundaries
admin <- left_join(admin, projections, by = c("GID_0" = "country_code")) # merge the projections to the spatial data

# set parameters for parallel processing
cores <- 3 # not to overload your computer
cl <- makeCluster(cores) 
registerDoParallel(cl)

# subset the country names to save health facilities uniquely 
countries <- admin$GID_0

# parallel processing of facilities per country. We intersect the facility points 
# with the country borders and save the shapefile. 
system.time(foreach(i = 1:length(admin), .packages= c("sf")) %dopar% {
  country <- admin[i,]
  intersects <- st_intersects(facilities, country)
  points <- lengths(intersects) > 0
  facilities_intersects <- facilities[points, ]
  st_crs(facilities_intersects) <- st_crs(country)
  facilities_intersects <- st_transform(facilities_intersects, admin$prj4[i])
  st_write(facilities_intersects, paste0("data/processed/vHealth_facilities/facilities_", countries[i], ".shp"), append = F)
  }) 

## THE END ##
  