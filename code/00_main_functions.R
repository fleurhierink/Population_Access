##' Title: Script for the data preparation of input data for AccessMod
##' Author: Fleur Hierink and Pablo Timoner
##' Organisation: University of Geneva
##' Date: 2022

# Libraries --------------------------------------------------------------------------
# Efficient for shapefiles 
if (!require("sf")) install.packages("sf"); library("sf")
# New library raster
if (!require("terra")) install.packages("terra"); library("terra")
# Still useful for some functions
if (!require("raster")) install.packages("raster"); library("raster")
# Downloading openstreetmap data on roads and rivers
if (!require("osmextract")) install.packages("osmextract"); library("osmextract")
# Underlying support for spatial analysis
if (!require("rgdal")) install.packages("rgdal"); library("rgdal")
# Underlying support for spatial analysis
if (!require("rgeos")) install.packages("rgeos"); library("rgeos")
# Zonal statistics
if (!require("exactextractr")) install.packages("exactextractr"); library("exactextractr")
# Rasterize fast
if (!require("fasterize")) install.packages("fasterize"); library("fasterize")
# Reclassifying roads
if (!require("dplyr")) install.packages("dplyr"); library("dplyr")
# Print contents of directories in a tree-like format 
if (!require("fs")) install.packages("fs"); library("fs")
# Menu interface and prompt
if (!require("utils")) install.packages("utils"); library("utils")
# Common string manipulations
if (!require("stringr")) install.packages("stringr"); library("stringr")
# Access FTP servers
if (!require("RCurl")) install.packages("RCurl"); library("RCurl")
# Access remotes to install packages from github
if (!require("remotes")) install.packages("remotes"); library("remotes")
# Access geoboundaries
if (!require("rgeoboundaries")) remotes::install_github("wmgeolab/rgeoboundaries"); library("rgeoboundaries")
# Access Zenodo
if (!require("zen4R")) install.packages("zen4R"); library("zen4R")

# Main parameters --------------------------------------------------------------------------
urlSRTM <- "https://github.com/sikli/srtm_country/archive/master.zip"
doiLC <- "10.5281/zenodo.3939050"
zenodoFileLC <- "PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"

# Main functions --------------------------------------------------------------------------
# Create directories for the project
# Warnings are displayed if the folders already exist, and these are not erased
# Default variables are set for the data input names
project.dir <- function(mainPath,region){
  if(!dir.exists(mainPath)){
    stop("Main folder path does not exist.")
  }
  # Main standard inputs
  inputNames=c("rDEM","rPopulation","rLandcover","vRoads","vWaterLines","vWaterPoly","vZones","vFacilities")
  message(paste("\nThe following input folders will be created:",paste(inputNames,collapse=", ")))
  # Add other data ?
  yn <- menu(c("YES","NO"), title="\nDo you want to add another input (type 1 or 2) ?")
  while(yn==1){
    newName <- readline(prompt="Enter the folder name: ")
    inputNames <- c(inputNames,newName)
    yn <- menu(c("YES","NO"), title="\nDo you want to add another input ?")
  }
  # Create directories
  pathData <- paste0(mainPath,"/",toupper(region),"/data")
  dir.create(pathData,recursive = TRUE)
  for(type in c("raw","processed")){
    dir.create(paste0(pathData,"/",type))
    for(inputName in inputNames){
      dir.create(paste0(pathData,"/",type,"/",inputName))
    }
  }
  # Print directory tree
  dir_tree(paste0(mainPath,"/",toupper(region),"/data"))
  cat("Health facilities shapefile have to be downloaded manually and \ncopied into their respective folders: /data/raw/vFacilities")
  cat("\nOther inputs can be dowloaded automatically using the download.[variable] functions.")
}

# Set project main parameters (reference system and resolution)
set.param <- function(mainPath,region,iso3,epsg,res){
  pathRegion <- paste0(mainPath,"/",toupper(region),"/data")
  if(!dir.exists(pathRegion)){
    stop(paste(pathRegion,"does not exist. Run the project.dir function."))
  }
  fileConn <- file(paste0(pathRegion,"/config.txt"))
  writeLines(c(paste0("ISO:",iso3),paste0("EPSG:",epsg),paste0("Resolution:",res)), fileConn)
  close(fileConn)
  cat("Project parameters are set.")
}

# Access config.txt (created with set.param) and ISO code
# Used in other functions
get.iso <- function(mainPath,region){
  pathRegion <- paste0(mainPath,"/",region,"/data")
  if(!file.exists(paste0(pathRegion,"/config.txt"))){
    stop("Project main parameters have not been set yet. Run the set.param function.")
  }
  fileConn <- file(paste0(pathRegion,"/config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  iso <- config[grepl("ISO",config)]
  iso <- gsub("^[A-Z]*\\:","",iso)
  return(iso)
}

# Write lon/lat from config.txt 
# Used when downloading boundaries
write.lonlat <- function(bound,pathBound){
  lon <- st_bbox(bound)[1]+(st_bbox(bound)[3]-st_bbox(bound)[1])/2
  lat <- st_bbox(bound)[2]+(st_bbox(bound)[4]-st_bbox(bound)[2])/2
  for(i in c("lon","lat")){
    fileConn=file(paste0(pathBound,"/../../config.txt"),open="r")
    configTxt <- readLines(fileConn)
    close(fileConn)
    if(any(grepl(paste0(str_to_title(i),":"),configTxt))){
      newValues <- gsub(paste0(str_to_title(i),":.*"),paste0(str_to_title(i),":",eval(parse(text=i))),configTxt)
      fileConn=file(paste0(pathBound,"/../../config.txt"),open="w")
      writeLines(newValues,fileConn)
      close(fileConn)
    }else{
      write(paste0(str_to_title(i),":",eval(parse(text=i))),file=paste0(pathBound,"/../../config.txt"),append=TRUE)
    }
  }
}

# Download administrative boundaries (recommended)
download.zones.geoboundaries <- function(mainPath,region,adminLevel){
  # Check directory
  pathBound <- paste0(mainPath,"/",toupper(region),"/data/raw/vZones")
  if(!dir.exists(pathBound)){
    stop(paste(pathBound,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  if(!adminLevel %in% c(0,1,2,3,4,5)){
    stop("Administrative level must be an integer from 0 to 5")
  }
  # Get country code
  iso <- get.iso(mainPath,region)
  # Download the data
  bound <- geoboundaries(iso,adm_lvl=adminLevel,quiet=FALSE,)
  boundMeta <- gb_metadata(iso,adm_lvl=adminLevel)
  write.lonlat(bound,pathBound)
  # Save metadata
  write.table(boundMeta,paste0(pathBound,"/metadata.txt"))
  # Save shapefile
  st_write(bound,paste0(pathBound,"/vZones_r.shp"),append=F)
  cat(paste0(pathBound,"/vZones_r.shp"))
}

# Download administrative boundaries (NOT recommended)
download.zones.gadm <- function(mainPath,region,adminLevel){
  # Check directory
  pathBound <- paste0(mainPath,"/",toupper(region),"/data/raw/vZones")
  if(!dir.exists(pathBound)){
    stop(paste(pathBound,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  if(!adminLevel %in% c(0,1,2,3,4,5)){
    stop("Administrative level must be an integer from 0 to 5")
  }
  # Get country code
  iso <- get.iso(mainPath,region)
  # Download the data and save it as a shapefile
  bound <- tryCatch({getData('GADM',country=iso,level=adminLevel,path=pathBound)},error=function(cond){stop(cond)})# this automatically downloads the borders from GADM, the level indicates the sub-national borders (i.e. 0 = country borders, 1 = province, 2 = district, etc.)
  bound <- as(bound, "sf")
  write.lonlat(bound,pathBound)
  st_write(bound,paste0(pathBound,"/vZones_r.shp"),append=F)
  # Remove temporary file
  file.remove(paste0(pathBound,"/",list.files(pathBound)[grepl("*.rds",list.files(pathBound))]))
  cat(paste0(pathBound,"/vZones_r.shp"))
}

# Get lon/lat from config.txt 
# Used to download DEM
get.lonlat <- function(mainPath,region){
  pathRegion <- paste0(mainPath,"/",region,"/data")
  if(!file.exists(paste0(pathRegion,"/config.txt"))){
    stop("Project main parameters have not been set yet. Run the set.param function.")
  }
  fileConn <- file(paste0(pathRegion,"/config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  lonTrue <- any(grepl("Lon:.*",config))
  latTrue <- any(grepl("Lat:.*",config))
  lonlatTrue <- all(lonTrue,latTrue)
  if(!lonlatTrue){
    stop("Lon/lat are missing from the config.txt. Run the download.bound.geoboundaries function.")
  }
  lon <- as.numeric(gsub("Lon:","",config[grepl("Lon:.*",config)]))
  lat <- as.numeric(gsub("Lat:","",config[grepl("Lat:.*",config)]))
  return(list(lon,lat))
}

# Download DEM from SRTM (FABDEM only can be accessed through their website)
download.dem.srtm <- function(mainPath,region){
  # Check directory
  pathDEM <- paste0(mainPath,"/",toupper(region),"/data/raw/rDEM")
  if(!dir.exists(pathDEM)){
    stop(paste(pathDEM,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  if(!file.exists(paste0(mainPath,"/",toupper(region),"/data/raw/vZones/vZones_r.shp"))){
    stop("Administrative boundaries shapefile is missing. Run the download.zones.geoboundaries function.")
  }else{
    bound <- readOGR(paste0(pathBound,"/vZones_r.shp"))
  }
  # Download SRTM tiles shapefile in a temporary folder
  tmpFolder <- paste0(mainPath,"/",toupper(region),"/data/raw/rDEM/temp")
  dir.create(tmpFolder)
  download.file(url=urlSRTM,destfile = paste0(tmpFolder,"/srtm.zip"))
  unzip(zipfile=paste0(tmpFolder,"/srtm.zip"),overwrite=TRUE,exdir=tmpFolder)
  shp <- shapefile(paste0(tmpFolder,"/srtm_country-master/srtm/tiles.shp"))
  #Intersect country geometry with tile grid
  intersects <- gIntersects(bound,shp,byid=T)
  tiles <- shp[intersects[,1],]
  #Download tiles
  srtmList  <- list()
  for(i in 1:length(tiles)){
    lon <- extent(tiles[i,])[1]  + (extent(tiles[i,])[2] - extent(tiles[i,])[1]) / 2
    lat <- extent(tiles[i,])[3]  + (extent(tiles[i,])[4] - extent(tiles[i,])[3]) / 2
    tile <- getData('SRTM',lon=lon,lat=lat,path=paste0(tmpFolder,"/"))
    srtmList[[i]] <- tile
  }
  # Mosaic
  srtmList$fun <- mean 
  srtmMosaic   <- do.call(mosaic, srtmList)
  srtmMosaic
  writeRaster(srtmMosaic,paste0(pathDEM,"/rDEM_r.tif"))
  unlink(tmpFolder,recursive=TRUE)
  cat(paste0(pathDEM,"/rDEM_r.tif"))
}

# Search in FTP folders
# Used in other functions (download.population)
navigate.ftp <- function(folderLst,iso,pathFTP,pathFTP0){
  while(!(any(grepl("\\.tif$",folderLst)))){
    # If there is a folder with our region code, select it
    if(any(grepl(iso,folderLst))){
      pathFTP <- paste0(pathFTP,iso,"/")
      # If not, let the user choose
    }else{
      folderLst <- c(folderLst,"EXIT FUNCTION")
      folderNb <- menu(c(folderLst), title="\nSelect folder (type the corresponding number or zero to get back to the root directory) ?")
      if(folderNb==length(folderLst)){
        return(NULL)
      }else if(folderNb==0){
        pathFTP <- pathFTP0
      }else{
        selectedFolder <- folderLst[folderNb]
        pathFTP <- paste0(pathFTP,selectedFolder,"/")
      }
    }
    gc()
    folderLst <- getURL(pathFTP,verbose=FALSE,ftp.use.epsv=TRUE,dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x=gsub("\\r\\n"," ",folderLst),split=" "))
  }
  return(list(folderLst,pathFTP))
}

# Download population raster
download.population <- function(mainPath,region){
  pathFolder <- paste0(mainPath,"/",toupper(region),"/data/raw/rPopulation")
  iso <- get.iso(mainPath,region)
  pathFTP0 <- "ftp://ftp.worldpop.org.uk/GIS/Population/"
  pathFTP <- pathFTP0
  downloadProcess <- TRUE
  while(downloadProcess){
    # To avoid error message
    gc()
    # Get directories
    folderLst <- getURL(pathFTP,verbose=FALSE,ftp.use.epsv=TRUE,dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x=gsub("\\r\\n"," ",folderLst),split=" "))
    # While we don't exit the function
    out <- navigate.ftp(folderLst,iso,pathFTP,pathFTP0)
    folderLst <- out[[1]]
    pathFTP <- out[[2]]
    if(is.null(folderLst)){
      return(cat("You exit the function. No file has been downloaded"))
    }
    nFile <- 1:length(folderLst)
    indFile <- paste(paste0("\n",nFile,": ",folderLst))
    cat(indFile)
    cat("\n\nSelect file (corresponding number) to be downloaded (if multiple files: on the same line separated by a space).\nType zero to get back to the root directory.\nSkip to exit the function.")
    cat("\n ")
    selInd <- readline(prompt = "Input: ")
    selInd <- as.numeric(unlist(strsplit(x=selInd,split=" ")))
    if(length(selInd)==0){
      return(cat("You exit the function. No file has been downloaded"))
    }else if(0 %in% selInd){
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    }else{
      for(i in selInd){
        filePath <- paste0(pathFTP,folderLst[i])
        download.file(url=filePath,destfile=paste0(pathFolder,"/",folderLst[i]),quiet=FALSE,mode = "wb")
        cat(paste0(pathFolder,"/",folderLst[i]))
      }
      downloadProcess <- FALSE
    }
  }
}

# Download land cover
download.landcover <- function(mainPath,region){
  # Check directory
  pathLandcover <- paste0(mainPath,"/",toupper(region),"/data/raw/rLandcover")
  if(!dir.exists(pathLandcover)){
    stop(paste(pathLandcover,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  if(!file.exists(paste0(mainPath,"/",toupper(region),"/data/raw/vZones/vZones_r.shp"))){
    stop("Administrative boundaries shapefile is missing. Run the download.zones.geoboundaries function.")
  }
  # Download SRTM tiles shapefile in a temporary folder
  tmpFolder <- paste0(mainPath,"/",toupper(region),"/data/raw/rLandcover/temp")
  dir.create(tmpFolder)
  download_zenodo(doi=doiLC,path=tmpFolder,files=zenodoFileLC)
  globalLandcover <- rast(paste0(tmpFolder,"/",zenodoFileLC))
  pathBound <- paste0(mainPath,"/",toupper(region),"/data/raw/vZones")
  bound <- paste0(pathBound,"/vZones_r.shp")
  cropLandcover <- terra::crop(globalLandcover,bound)
  writeRaster(cropLandcover,paste0(pathLandcover,"/rLandcover_r.tif"),overwrite=TRUE)
  unlink(tmpFolder,recursive=TRUE)
  cat(paste0(pathLandcover,"/rLandcover_r.tif"))
}

# Subset based on categories for automatically downloaded shapefiles
# Used in download.osm
select.categories <- function(sfObject,columnName){
  sfDataFrame <- sfObject
  st_geometry(sfDataFrame) <- NULL
  categories <- unique(sfDataFrame[,columnName])
  nCat <- 1:length(categories)
  indCat <- paste(paste0("\n",nCat,": ",categories))
  cat(indCat)
  cat("\n\nEnter all the indices that correspond to categories you want to keep (on the same line separated by a space, or just skip to select all categories)\n")
  selInd <- readline(prompt = "Input: ")
  selInd <- as.numeric(unlist(strsplit(x=selInd,split=" ")))
  if(length(selInd)!=0){
    sfObject <- subset(sfObject,eval(parse(text=columnName)) %in% categories[selInd])
  }
  return(sfObject)
}

# Download shapefile from Open Street Map
downlad.osm <- function(x,mainPath,region){
  if(!(x=="roads"|x=="waterLines"|x=="waterPoly")){
    stop("x must be 'roads', 'waterLines' or 'waterPoly'")
  }
  pathFolder <- paste0(mainPath,"/",toupper(region),"/data/raw/v",str_to_title(x))
  if(!dir.exists(pathFolder)){
    stop(paste(pathFolder,"does not exist. You may run the project.dir function first or check the input parameters."))
  }
  if(x=="roads"){
    querySQL <- "SELECT * FROM 'lines' WHERE highway IS NOT NULL"
    colName <- "highway"
  }else if(x=="waterLines"){
    querySQL <- "SELECT * FROM 'lines' WHERE waterway IS NOT NULL"
    colName <- "waterway"
  }else{
    querySQL <- "SELECT * FROM 'multipolygons' WHERE natural IS NOT NULL"
    colName <- "natural"
  }
  
  # Download 
  shp <- oe_get(region,
                quiet=FALSE,
                query=querySQL,
                download_directory=pathFolder,
                force_download = TRUE)
  
  shp <- select.categories(shp,colName)
  st_write(shp,paste0(pathFolder,"/v",str_to_title(x),"_r.shp"),append=FALSE) # Save the layer
  file.remove(paste0(pathFolder,"/",list.files(pathFolder)[grepl("*.gpkg|*.pbf",list.files(pathFolder))]))
  cat(paste0(pathFolder,"/v",str_to_title(x),"_r.shp"))
}


# Example --------------------------------------------------

mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS"
region <- "Afghanistan"
project.dir(mainPath,region)
set.param(mainPath,region,"AFG",3642,100)
download.zones.geoboundaries(mainPath,region,1)
download.dem.srtm(mainPath,region)
download.landcover(mainPath,region)
download.population(mainPath,region)
downlad.osm("waterPoly",mainPath,region)
downlad.osm("waterLines",mainPath,region)
downlad.osm("roads",mainPath,region)

