##' Title: Extracting coverage statistics for landscan
##' Description: In this R-script we extract the accessibility coverage statistics for landscan 
##' by intersecting the population layer with the travel time raster. 
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# libraries
library(terra)
library(rgdal)
library(sf)
library(dplyr)
library(data.table)

# read in files
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
population <- c(list.files("data/processed/rPopulation/rPopulation_proj_corr/landscan/", recursive = T,full.names = T, pattern = "*.tif$"))
traveltime <- c(list.files("data/processed/rTravel_times", full.names = T, pattern = "*.tif$", recursive = T))
admin0 <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")
admin1 <-st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm1.shp")
admin2 <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm2.shp")

# some countries do not have administrative 2 level to be able to sum the population 
# within admin 2 boundaries we merge admin1 and admin 2 boundaries 
admin1_islands <- admin1 %>%
  filter(GID_0 %in% c("COM", "CPV", "MUS", "SYC")) %>%
  select(c(1:4)) %>%
  mutate(GID_2 = GID_1,
         NAME_2 = NAME_1)

admin2 <- admin2 %>%
  select(c(1:4, 6:7))

all_admin <- rbind(admin1_islands, admin2)

# The function to calculate coverage statistics. First we select the first country
# to work on and we create a raster that holds the admin boundary information. 
# The population raster is converted to points to overcome any challenges with the resolution.
# The points hold the total population count in each grid cell. The points are then 
# intersected with the travel time raster to extract the travel time for each population point. 
# We also intersect the population points with the administrative boundary raster. Afterwards,
# we convert the population points to a data frame in which we can summarize accessibility statistics
# for different travel time catchments.
for (i in 1:nrow(admin0)) {
  admin <- admin0[i,]
  pop <- rast(population[i])
  access <- rast(traveltime[i])
  iso3 <- admin$GID_0
  country <- all_admin %>% # select country to work on and project in right projection system
    filter(GID_0 == iso3) %>%
    mutate(adm = row_number()) %>%
    st_transform(st_crs(pop))
  countryV <- vect(country)
  countryR <- rasterize(countryV, access, field = "adm") # create raster of admin boundaries
  size <- file.size(population[i])/1000000 # calculate file size in MB
  result <- list() # create empty list in case population data is too big to be processed at once each bit can be added sequentially 
  
  if (size > 100) {
    raster <- rast(nrows = 5, ncols = 5, extent = ext(pop), crs = crs(pop), vals = seq(1:100))
    polygon <- as.polygons(raster)
    
    for (j in 1:length(polygon)) {
      pop_crop <- mask(pop, polygon[j,]) # here we take the first part of the population raster to work on
      print(paste("Working on piece", j, "of population raster for", iso3))
      pop_points <- as.points(pop_crop, values=T, na.rm = T) # the population raster is converted to points 
      pop_tt <- extract(access, pop_points) # the population points are intersected with the travel time raster to extract travel times for each pop grid cell 
      pop_adm <- extract(countryR, pop_points) # the population points are intersected with the admin boundary raster to extract the administrative boundary for each pop grid cell 
      df <- cbind(as.data.frame(pop_points), pop_tt[2]) # the two extracted values 1) travel time and 2) admin boundary are merged in one data frame
      df <- cbind(df, pop_adm[2])
      colnames(df) <- c("landscan", "traveltime", "adm")
      final <- left_join(df, country, by = "adm")
      result[[j]] <- final } # add the result to the list so we can merge all chunks together in a later step
    
    list <- result[sapply(result, function(x) dim(x)[1]) > 0] # remove empty list items for chunks that do not contain population 
    
    final_df <- rbindlist(list) # bind all list items together
    
    # calculate accessibility coverage statistics for different travel time catchments.
    # The statements here indicate that if the travel time for a given population point 
    # is smaller or equal to 30, 60, 90, 120, or 180 minutes, we take the total population count in that grid cell, 
    # if not we indicate 0 to state that no population in that grid cell is able to reach a health facility in those catchments.
    # Afterwards we calculate the relative population covered in the different administrative boundaries.
    landscan_df <- final_df %>%
      mutate(landscan_totpop = landscan,
             landscan_30 = ifelse(traveltime <= 30, landscan_totpop, 0), 
             landscan_60 = ifelse(traveltime <= 60, landscan_totpop, 0), 
             landscan_90 = ifelse(traveltime <= 90, landscan_totpop, 0), 
             landscan_120 = ifelse(traveltime <= 120, landscan_totpop, 0),
             landscan_150 = ifelse(traveltime <= 150, landscan_totpop, 0),
             landscan_180 = ifelse(traveltime <= 180, landscan_totpop, 0),
             landscan_barrier = ifelse(is.na(traveltime), landscan_totpop,0)) %>%
      select(-c(landscan, traveltime, geometry)) %>%
      group_by(adm, GID_0, NAME_0, GID_1, NAME_1, GID_2, NAME_2) %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(landscan_30per = (landscan_30/landscan_totpop)*100, 
             landscan_60per = (landscan_60/landscan_totpop)*100,
             landscan_90per = (landscan_60/landscan_totpop)*100,
             landscan_120per = (landscan_120/landscan_totpop)*100,
             landscan_150per = (landscan_150/landscan_totpop)*100,
             landscan_180per = (landscan_180/landscan_totpop)*100,
             landscan_barrierper = (landscan_barrier/landscan_totpop)*100)
    write.csv(landscan_df, file = paste0("data/processed/tCoverage/landscan/landscan_", iso3, ".csv"))
  } 
  
  # if the population raster is not too heavy we process it all at once, using the steps
  # described above.
  else {
    print(paste("Country small enough to process at once. Working on:", iso3))
    pop_points <- as.points(pop, values=T, na.rm = T)
    pop_tt <- extract(access, pop_points)
    pop_adm <- extract(countryR, pop_points)
    df <- cbind(as.data.frame(pop_points), pop_tt[2])
    df <- cbind(df, pop_adm[2])
    colnames(df) <- c("landscan", "traveltime", "adm")
    final <- left_join(df, country, by = "adm")
    landscan_df <- final %>%
      mutate(landscan_totpop = landscan,
             landscan_30 = ifelse(traveltime <= 30, landscan_totpop, 0), 
             landscan_60 = ifelse(traveltime <= 60, landscan_totpop, 0), 
             landscan_90 = ifelse(traveltime <= 90, landscan_totpop, 0), 
             landscan_120 = ifelse(traveltime <= 120, landscan_totpop, 0),
             landscan_150 = ifelse(traveltime <= 150, landscan_totpop, 0),
             landscan_180 = ifelse(traveltime <= 180, landscan_totpop, 0),
             landscan_barrier = ifelse(is.na(traveltime), landscan_totpop,0)) %>%
      select(-c(landscan, traveltime, geometry)) %>%
      group_by(adm, GID_0, NAME_0, GID_1, NAME_1, GID_2, NAME_2) %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(landscan_30per = (landscan_30/landscan_totpop)*100, 
             landscan_60per = (landscan_60/landscan_totpop)*100,
             landscan_90per = (landscan_60/landscan_totpop)*100,
             landscan_120per = (landscan_120/landscan_totpop)*100,
             landscan_150per = (landscan_150/landscan_totpop)*100,
             landscan_180per = (landscan_180/landscan_totpop)*100,
             landscan_barrierper = (landscan_barrier/landscan_totpop)*100)
    write.csv(landscan_df, file = paste0("data/processed/tCoverage/landscan/landscan_", iso3, ".csv")) 
  }
  gc()
}

## THE END ##
