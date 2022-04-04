##' Title: Creation of the friction layer 
##' Description: In this R-script we transform the land cover to a friction layer by reclassifying the landcover classes based 
##' on speeds per category that were adapted from Weiss et al (2020) https://doi.org/10.1038/s41591-020-1059-1. 
##' All landcover and road classes were given a speed in km/h which were recalculated to the number of minutes it takes to 
##' traverse a meter. Below a step-by-step overview is given. 
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# libraries
library(dplyr)
library(plyr)
library(rgdal)
library(terra)
library(readxl)
library(sf)
library(doParallel)
library(foreach)
library(snow)

# read in the datasets required for this script
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
travelspeeds <- read_xlsx("data/raw/tScenario/2020_Weiss/2020_Weiss_supplementary_speeds.xlsx", 
                          sheet = "Sheet1") # speeds coming from Weiss et al (2020)
admin <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")
landcover <- c(list.files("data/processed/rLandcover_merge/", recursive = T, full.names = T, pattern = "*.tif$")) # land cover merge for each country
landcoverspeeds <- read_xlsx("data/raw/tScenario/landcover_classes/scenario landcover.xlsx") # table containing the speeds for the different land cover classes 

# subset the country names, road classes, and land cover classes
countries <- admin$GID_0
road_classes <- c(1001:1025)
landcover_classes <- landcoverspeeds$Class

# populate empty data frame with country names and road classes so a travel scenario can be
# extracted for each country from the Weiss et al travel scenario. 
countries <- rep(countries, each = length(road_classes))
road_classes <- rep(road_classes, length(unique(countries)))
data_roads <- data.frame(country = countries, road_class = road_classes) %>%
  mutate(osm_class = recode(road_class,  
                        '1001' = "trunk",
                        '1002' = "trunk_link",
                        '1003' = "primary",
                        '1004' = "primary_link",
                        '1005' = "motorway",
                        '1006' = "motorway_link",
                        '1007' = "secondary",
                        '1008' = "secondary_link",
                        '1009' = "tertiary",
                        '1010' = "tertiary_link",
                        '1011' = "road",
                        '1012' = "raceway",
                        '1013' = "residential",
                        '1014' = "living_street",
                        '1015' = "service",
                        '1016' = "track",
                        '1017' = "pedestrian",
                        '1018' = "path",
                        '1019' = "footway",
                        '1020' = "piste",
                        '1021' = "bridleway",
                        '1022' = "cycleway",
                        '1023' = "steps",
                        '1024' = "unclassified",
                        '1025' = "bridge")) %>% # reclassify the different road classes
  left_join(., travelspeeds, by = c("country" = "ISO3", "osm_class" = "osm_class")) %>% # merge the empty table with the Weiss et al table
  mutate(weiss_class = ifelse(is.na(speed) == TRUE, "other", osm_class))  %>% # if Weiss table does not have a speed for a certain category, assign the class "other" to a new column
  dplyr::select(-c(GAUL, country_name, osm_tag_name, speed)) %>% # only keep important columns
  left_join(., travelspeeds, by = c("country" = "ISO3", "weiss_class" = "osm_class")) %>% # join travel speeds of weiss again, including the "other" category
  mutate(conversion = ((60*1)/(speed*1000))) %>%  # the input of the accessibility analysis, requires a time cost of traversing one cell per meter. For 100m we change the first 1 to 100 for conversion.
  dplyr::select(c(country, country_name, osm_class, weiss_class, road_class, speed, conversion)) 

# some countries don't have average speeds for specific road categories. Here we calculate the
# the average road speeds for the African continent. 
african_average <- data_roads %>%
  ddply(.,~road_class, summarise, mean=mean(speed, na.rm=TRUE), sd=sd(speed, na.rm=TRUE)) %>%
  na.omit()

# we merge the african averages with our road data frame 
data_roads_mean <- data_roads %>%  
  left_join(., african_average, by = "road_class") %>%
  mutate(speed = ifelse(is.na(speed) == TRUE | weiss_class == "other", mean, speed),
         conversion = ((60*1)/(speed*1000)))  # for calculation of minutes per meter. for 100m we change the first 1 to 100 for conversion

# create data frame for land cover classes
countries <- admin$GID_0
countries <- rep(countries, each = length(landcover_classes))
landcover_classes <- rep(landcover_classes, length(unique(countries)))


data_landcover <- data.frame(country = countries, landcover_class = landcover_classes) %>%
  mutate(landcover_label = recode(landcover_class, 
                            '0'   = "unknown",
                            '20'  = "shrubs",
                            '30'  = "herbaceous vegetation",
                            '40'  = "cultivated/agriculture",
                            '50'  = "urban/built up",
                            '60'  = "bare/sparse vegetation",
                            '70'  = "snow and ice",
                            '80'  = "permanent waterbodies",
                            '81'  = "unclassified",
                            '90'  = "herbaceous wetland",
                            '100' = "moss and lichen",
                            '111' = "closed forest, evergreen needle leaf",
                            '112' = "closed forest, evergreen broad leaf",
                            '113' = "closed forest, deciduous needle leaf",
                            '114' = "closed forest, deciduous broad leaf",
                            '115' = "closed forest, mixed",
                            '116' = "closed forest, not matching other definitions",
                            '121' = "open forest, evergreen needle leaf",
                            '122' = "open forest, evergreen broad leaf",
                            '123' = "open forest, deciduous needle leaf",
                            '124' = "open forest, deciduous needle leaf",
                            '125' = "open forest, mixed",
                            '126' = "open forest, not matching other definitions",
                            '200' = "oceans, seas")) %>%
  left_join(., landcoverspeeds, by = c("landcover_class" = "Class")) %>%
  dplyr::select(-c(Label, source)) %>%
  mutate(conversion = ((60*1)/(Speed*1000))) # for calculation of minutes per meter. for 100m we change the first 1 to 100 for conversion

# split the full travel speed dataframe into separate list items
speeds_roads <- data_roads_mean %>%
  dplyr::select(c(country, road_class, conversion)) %>%
  dplyr::rename(class = "road_class")
speeds_landcover <- data_landcover %>%
  dplyr::select(c(country, landcover_class, conversion)) %>%
  dplyr::rename(class = "landcover_class")
speeds <- rbind(speeds_roads, speeds_landcover)
speeds_list <- list()
speeds_list <- split(speeds , f = speeds$country)
speeds_list <- lapply(speeds_list, function(x) { x["country"] <- NULL; x})

# set the parameters for parallel processing
cores <- 2 # not to overload your computer
cl <- makeCluster(cores) 
registerDoParallel(cl)

# create a friction layer that we save for each country. We load inn each landcover 
# and reclassify it based on the speed conversion so that each cell represents 
# the time it takes to traverse 1 meter. 
system.time(foreach(i = 1:length(landcover), .packages= c("terra")) %dopar% {
  landcover_merge <- rast(landcover[i])
  landcover_friction <- classify(landcover_merge, speeds_list[[i]])
  writeRaster(landcover_friction, paste0("data/processed/rFriction_1m/friction_", countries[i], ".tif"), 
              overwrite = T)})

## THE END ##
