##' Title: Data preparation of the merged landcover that is used as input to the accessibility analysis. 
##' Description: In this R-script we merge all the previously prepared topographic layers into one overarching landcover layer. 
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
library(terra)

# load in data layers
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
landcover <- c(list.files("data/processed/rLandcover", recursive = T, full.names = T, pattern = "*.tif$"))
roads <- c(list.files("data/processed/vRoads", recursive = T, full.names = T, pattern = "*.shp$"))
water_lines <- c(list.files("data/processed/vWater_lines", recursive = T, full.names = T, pattern = "*.shp$"))
water_poly <- c(list.files("data/processed/vWater_poly", recursive = T, full.names = T, pattern = "*.shp$"))
admin <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")


# country names
file_name <- admin$GID_0

# Steps for the merged land cover:
# official road classes that can be found in data
road_classes <- c("motorway", "trunk", "primary", "secondary", "tertiary",
                  "unclassified", "residential", "motorway_link", "trunk_link",
                  "primary_link", "secondary_link", "tertiary_link", "living_street",
                  "service", "pedestrian", "track", "road", "footway", "bridleway",
                  "steps", "path", "cycleway", "raceway", "unclassified", "bridge")

# list to reclassify road names into integer values
reclass_roads <- list(
  "trunk" = 1001,
  "trunk_link" = 1002,
  "primary" = 1003,
  "primary_link"= 1004,
  "motorway" = 1005,
  "motorway_link" = 1006,
  "secondary" = 1007,
  "secondary_link" = 1008,
  "tertiary" = 1009,
  "tertiary_link" = 1010,
  "road" = 1011,
  "raceway" = 1012,
  "residential" = 1013,
  "living_street" = 1014,
  "service" = 1015,
  "track" = 1016,
  "pedestrian" = 1017,
  "path" = 1018,
  "footway" = 1019,
  "piste" = 1020,
  "bridleway" = 1021,
  "cycleway" = 1022,
  "steps" = 1023,
  "unclassified" = 1024,
  "bridge" = 1025)

# all data will be combined in one overarching merged land cover
# all vector data obtained in the previous steps will be rasterized at 100m
# water lines serve as barriers and will therefore be classified as NA cells
# here we present the function. Below we parellize the function. 
landcover_merge_function <- function(i) {
  landcov <- rast(landcover[i]) # load landcover raster
  grid_landcov <- rast(ext(landcov)) 
  res(grid_landcov) <- 100
  crs(grid_landcov) <- crs(landcov)
  landcover_resamp  <- resample(landcov, grid_landcov, "ngb") # resample landcover to 100m
  roads_new <- st_read(roads[i]) %>%
    group_by(highway) %>%
    summarize() %>%
    filter(highway %in% as.factor(road_classes)) %>%
    mutate(class = recode(highway, !!!reclass_roads),
           class = as.integer(class)) %>% # give all road classes an integer value, so they can be represented in the raster
    st_transform(st_crs(landcov)) %>%
    as(., "Spatial") %>%
    vect()
  roads_rast <- terra::rasterize(roads_new, grid_landcov, "class", touches = TRUE) # rasterize roads to 100 meters 
  water_lines_new <- st_read(water_lines[i]) %>% # load water lines
    group_by(waterway) %>%
    summarize() %>%
    filter(waterway == "river") %>% # filter only hydrography lines classified as river 
    mutate(class = 1) %>% # give all rivers and integer class 1 so that later they can be classified as barriers
    st_transform(st_crs(landcov)) %>%
    as(., "Spatial") %>%
    vect()
  water_lines_rast <- terra::rasterize(water_lines_new, grid_landcov, field = "class", touches = TRUE) # rasterize water lines
  values(water_lines_rast)[values(water_lines_rast) == 0] <- NA # make background value NA
  water_poly_new <- st_read(water_poly[i]) %>% # load water polygons
    mutate(class = 1) %>% # give all rivers and lakes integer class 1 so that later they can be classified as barriers
    st_transform(st_crs(landcov)) %>%
    as(., "Spatial") %>%
    vect()
  water_poly_rast <- terra::rasterize(water_poly_new, grid_landcov, "class", touches = TRUE) # rasterize water polygons
  values(water_poly_rast)[values(water_poly_rast) == 0] <- NA # make background value NA
  landcover_merge <- terra::merge(water_lines_rast, landcover_resamp)  # add in the following sequence from bottom to top all the raster layers (landcover, water lines, water polygons, roads)
  landcover_merge <- terra::merge(water_poly_rast, landcover_merge)
  landcover_merge <- terra::merge(roads_rast, landcover_merge)
  values(landcover_merge)[values(landcover_merge) == 1] <- NA # all values that are 1 represent waterbodies, remove these values to make them barriers
  writeRaster(landcover_merge, paste0("data/processed/rLandcover_merge/landcover_merge_", file_name[i], ".tif"), overwrite=T)
}

# parallel processing of landcover merge
cores <- 3 # not to overload your computer
cl <- makeCluster(cores) 
registerDoParallel(cl)

terraOptions(tempdir="/temp/") # set temporary folder for data processing, so R environment does not have memory issues
system.time(foreach(i = 1:length(landcover), .packages= c("sf", "dplyr", "terra")) %dopar% {landcover_merge_function(i)})

## THE END ##
