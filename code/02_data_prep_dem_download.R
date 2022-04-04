##' Title: Download & mosaic SRTM data 
##' Description: In this R-script we download the different tiles from the Shuttle Radar Topography Mission (SRTM)
##' capturing the digital elevation model, covering all countries in sub-Saharan Africa.
##' Author: Fleur Hierink, (fleur.hierink@unige.ch)
##' Date: 14-02-2022
##' Institute: Institute for Environmental Sciences & Institute of Global Health
##' University: University of Geneva

## libraries
library(dplyr) # data organisation
library(rgdal) # spatial package for shapefiles
library(gdalUtils)
library(raster) # spatial package for rasters
library(foreach) # package for parallel processing
library(doParallel) # package for parallel processing
library(snow) # package for parallel processing

# load datasets. first dataset are the administrative boundaries for Africa. The second represents the SRTM tiles.
# the administrative boundaries can be adapted to personal requirements. Here we used the GADM boundaries:
# available from: https://gadm.org/data.html 
admin <- readOGR("data/raw/admin boundaries/Administrative boundaries/AFR_GADM_2018_Adm0.shp")
srtm_tiles <- readOGR("data/raw/DEM/shape_srtm_tiles/tiles.shp") # shapefile containing polygons of SRTM tiles

# intersect african geometry and tile grid
intersects <- intersect(admin, srtm_tiles) 
tiles      <- srtm_tiles[intersects[,1],] # tiles that intersect with African countries 

# download SRTM tiles (152 tiles)
cores <- 3
cl <- makeCluster(cores) 
registerDoParallel(cl)

srtm_list  <- list()
system.time(foreach(i = 1:length(tiles), .packages = c("raster","foreach", "doParallel")) %dopar% { 
  lon <- extent(tiles[i,])[1]  + (extent(tiles[i,])[2] - extent(tiles[i,])[1]) / 2
  lat <- extent(tiles[i,])[3]  + (extent(tiles[i,])[4] - extent(tiles[i,])[3]) / 2
  
  tile <- getData('SRTM', 
                  lon=lon, 
                  lat=lat,
                  path = "data/raw/DEM/raster_srtm_tiles/")
  
  srtm_list[[i]] <- tile
})

# mosaic tiles 
srtm_files <- c(list.files("data/raw/DEM/raster_srtm_tiles/", recursive = T, full.names = T, pattern = "*.tif$"))
mosaic_rasters(gdalfile=srtm_files, dst_dataset="data/raw/DEM/srtm_mosaic.tif", of="GTiff")


## THE END ##