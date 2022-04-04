##' Title: Downloading WorldPop population data
##' Description: In this R-script we download the WorldPop constrained and unconstrained dataset. 
##' All other datasets have been downloaded from the source as indicated in the paper. 
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

# load libraries
library(dplyr)
library(sf)
library(doParallel)
library(RSelenium)
library(rvest)
library(tidyverse)
library(sf)
library(wpgpDownloadR)

# load in the admin boundaries
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
admin <- st_read("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")

# WorldPop top-down constrained
# download the worldpop data for each country
# subset the admin names and the year for which we download the estimates
iso3 <- admin$GID_0
year <- 2020
project <- "global"

# set the parameters to parallelize the process
cores <- 2 # not to overload your computer
cl <- makeCluster(cores) 
registerDoParallel(cl)

# function to download the worldpop constrained data
system.time(foreach(i = 1:length(iso3)) %dopar% {
  wp_ftp <- "ftp://ftp.worldpop.org.uk/GIS/Population"
  wp_project <- ifelse(project=="global", paste0("Global_2000_2020_Constrained"), "na")
  wp_year <- as.numeric(year)
  wp_iso3 <- toupper(iso3[i])
  wp_file <- paste0(tolower(iso3[i]), "_ppp_", wp_year, "_", "UNadj_constrained", ".tif")
  
  download.file(url=paste(wp_ftp, wp_project, wp_year, "maxar_v1", wp_iso3, wp_file, sep="/"),
                destfile = paste0("data/raw/rPopulation/worldpop/constrained/wp_popcount_constr_unadj_", toupper(iso3[i]), ".tif"),
                quiet=F,
                mode="a")})

# WorldPop top-down unconstrained
# download the worldpop data for each country
# set the parameters to parallelize the process
cores <- 1 # not to overload your computer
cl <- makeCluster(cores) 
registerDoParallel(cl)

# subset the country names
iso3 <- admin$GID_0

# download the worldpop unconstrained data using the wpgpDownloadR library
system.time(foreach(i = 1:length(iso3), .packages= c("raster","foreach", "doParallel", "sf", "wpgpDownloadR")) %dopar% {
  raster <- wpgpGetCountryDataset(ISO3 = iso3[i], covariate = "ppp_2020_UNadj", destDir = paste0("data/raw/rPopulation/worldpop/unconstrained/")) 
})

## THE END ##

