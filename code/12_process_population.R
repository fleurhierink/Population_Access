##' Title: Data preparation of the different population datasets (projecting and correcting)
##' Description: In this R-script we project all the population datasets. 
##' We correct losses in total population sum by smoothing population within administrative 2 boundaries
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# libraries
library(terra)
library(sf)
library(dplyr)
library(rgdal)
library(doParallel)

# Load in the different datasets
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html
admin0 <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")
admin1 <-st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm1.shp")
admin2 <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm2.shp")
ghs_pop <- c(list.files("data/processed/rPopulation_clipped/ghs_pop/", recursive=  T,  full.names= T, pattern = "*.tif$"))
gpwv4 <- c(list.files("data/processed/rPopulation_clipped/gpwv4/", recursive = T, full.names = T, pattern = "*.tif$"))
worldpop_constr <- c(list.files("data/processed/rPopulation_clipped/worldpop/constrained/", recursive = T, full.names = T, pattern = "*.tif$"))
worldpop_unconstr <- c(list.files("data/processed/rPopulation_clipped/worldpop/unconstrained/", recursive = T, full.names = T, pattern = "*.tif$"))
facebook <- c(list.files("data/processed/rPopulation_clipped/facebook/", recursive = T, full.names = T, pattern = "*.tif$"))
landscan <- c(list.files("data/processed/rPopulation_clipped/landscan/", recursive = T, full.names = T, pattern = "*.tif$"))
projections <- read.csv("/data/raw/projections/projections.csv")

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

# merge projection data to administrative boundaries
epsg <- make_EPSG()
projections <- left_join(projections, epsg, by = c("epsg_code"  ="code"))
admin0 <- left_join(admin0, projections, by = c("GID_0" = "country_code"))

# set parameters for parallelization 
cores <- 1 # not to overload your computer
cl <- makeCluster(cores) 
registerDoParallel(cl)

# The population rasters are loaded per country and are projected in the country's
# projection system. Projected and unprojected population counts are compared and potential 
# differences in population counts are solved by smoothing lost population over the surface of the raster.
system.time(foreach(i = 1:nrow(admin0), .packages= c("terra", "dplyr", "sf")) %dopar% {
  pop <- rast(gpwv4_todo[i]) # change depending on the population dataset 
  admin <- admin0[i,]
  iso3 <- admin$GID_0
  country <- all_admin %>%
    filter(GID_0 == iso3) %>%
    mutate(ID = row_number()) %>%
    st_transform(st_crs(pop))
  countryV <- vect(country)
  countryR <- rasterize(countryV, pop, field = "ID")
  popsum <- zonal(pop, countryR, "sum") # calculate the total population per admin 2 level in the original input raster 
  colnames(popsum)[2] <- "popsum"
  pop_proj <- terra::project(pop, admin$prj4, method = "bilinear") # project the raster layer in the right projection
  country <- st_transform(country, crs=admin$prj4)
  countryV <- vect(country)
  countryR <- rasterize(countryV, pop_proj, field = "ID")
  popsum_proj <- zonal(pop_proj, countryR, "sum") # calculate the total population at admin 2 level for the projected raster
  colnames(popsum_proj)[2] <- "popsum_proj"
  country <- left_join(country, popsum, by = "ID") # add raw zonal stats to country shapefile
  country <- left_join(country, popsum_proj, by = "ID") # add zonal stats of projected raster to country shapefile
  country$pop_diff <- country$popsum/country$popsum_proj # calculate the difference between the unprojected and projected zonal stats
  countryV <- vect(country)
  zonalstat  <- rasterize(countryV, pop_proj, field = "pop_diff") # rasterize the country shapefiles at admin 2 using the difference between the projected and unprojected data as value
  pop_projcorr <- pop_proj*zonalstat # multiply the projected raster with the difference raster to correct any lost population
  pop_projcorrdf <- zonal(pop_projcorr, countryR, "sum")
  writeRaster(pop_proj, paste0("data/processed/rPopulation_proj_corr/gpwv4/gpwv4_popcorr_", iso3, ".tif"), overwrite = T) # change output location for different datasets
})

## THE END ##
