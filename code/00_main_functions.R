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
if (!require("zen4R")) install_github("eblondel/zen4R"); library("zen4R")

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
  inputNames=c("rDEM","rPopulation","rLandcover","vRoads","vWaterLines","vWaterPolygons","vBorders","vFacilities")
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
  cat("Health facilities shapefile have to be downloaded manually and copied into their respective folders: /data/raw/vFacilities")
  cat("\nOther inputs can be dowloaded automatically using the download.[variable] functions.")
}

# Set project main parameters (reference system and resolution)
set.param <- function(mainPath,region,iso3,epsg){
  pathRegion <- paste0(mainPath,"/",toupper(region),"/data")
  if(!dir.exists(pathRegion)){
    stop(paste(pathRegion,"does not exist. Run the project.dir function."))
  }
  fileConn <- file(paste0(pathRegion,"/config.txt"))
  writeLines(c(paste0("ISO:",iso3),paste0("EPSG:",epsg)), fileConn)
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
  iso <- gsub("^[A-z]*\\:","",iso)
  return(iso)
}

# Write lon/lat from config.txt 
# Used when downloading boundaries
write.lonlat <- function(border,pathBorder){
  lon <- st_bbox(border)[1]+(st_bbox(border)[3]-st_bbox(border)[1])/2
  lat <- st_bbox(border)[2]+(st_bbox(border)[4]-st_bbox(border)[2])/2
  for(i in c("lon","lat")){
    fileConn=file(paste0(pathBorder,"/../../config.txt"),open="r")
    configTxt <- readLines(fileConn)
    close(fileConn)
    if(any(grepl(paste0(str_to_title(i),":"),configTxt))){
      newValues <- gsub(paste0(str_to_title(i),":.*"),paste0(str_to_title(i),":",eval(parse(text=i))),configTxt)
      fileConn=file(paste0(pathBorder,"/../../config.txt"),open="w")
      writeLines(newValues,fileConn)
      close(fileConn)
    }else{
      write(paste0(str_to_title(i),":",eval(parse(text=i))),file=paste0(pathBorder,"/../../config.txt"),append=TRUE)
    }
  }
}

# Download administrative boundaries (recommended)
download.border.geoboundaries <- function(mainPath,region,adminLevel){
  # Check directory
  pathBorder <- paste0(mainPath,"/",toupper(region),"/data/raw/vBorders")
  if(!dir.exists(pathBorder)){
    stop(paste(pathBorder,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  if(!adminLevel %in% c(0,1,2,3,4,5)){
    stop("Administrative level must be an integer from 0 to 5")
  }
  # Get country code
  iso <- get.iso(mainPath,region)
  # Download the data
  border <- geoboundaries(iso,adm_lvl=adminLevel,quiet=FALSE,)
  borderMeta <- gb_metadata(iso,adm_lvl=adminLevel)
  
  write.lonlat(border,pathBorder)
  # Save metadata
  write.table(borderMeta,paste0(pathBorder,"/",borderMeta$boundaryID,".txt"))
  # Save shapefile
  st_write(border,paste0(pathBorder,"/",borderMeta$boundaryID,".shp"),append=F)
  cat(paste0(pathBorder,"/",borderMeta$boundaryID,".shp","\n"))
}

# Download administrative boundaries (NOT recommended, NOT UPDATED)
download.zones.gadm <- function(mainPath,region,adminLevel){
  # Check directory
  pathBorder <- paste0(mainPath,"/",toupper(region),"/data/raw/vBorders")
  if(!dir.exists(pathBorder)){
    stop(paste(pathBorder,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  if(!adminLevel %in% c(0,1,2,3,4,5)){
    stop("Administrative level must be an integer from 0 to 5")
  }
  # Get country code
  iso <- get.iso(mainPath,region)
  # Download the data and save it as a shapefile
  border <- tryCatch({getData('GADM',country=iso,level=adminLevel,path=pathBorder)},error=function(cond){stop(cond)})# this automatically downloads the borders from GADM, the level indicates the sub-national borders (i.e. 0 = country borders, 1 = province, 2 = district, etc.)
  border <- as(border, "sf")
  write.lonlat(border,pathBorder)
  st_write(border,paste0(pathBorder,"/vBorders_r.shp"),append=F)
  # Remove temporary file
  file.remove(paste0(pathBorder,"/",list.files(pathBorder)[grepl("*.rds",list.files(pathBorder))]))
  cat(paste0(pathBorder,"/vBorders_r.shp"))
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
    stop("Lon/lat are missing from the config.txt. Run the download.border.geoboundaries function.")
  }
  lon <- as.numeric(gsub("Lon:","",config[grepl("Lon:.*",config)]))
  lat <- as.numeric(gsub("Lat:","",config[grepl("Lat:.*",config)]))
  return(list(lon,lat))
}

get.boundaries <- function(mainPath,region){
  # Check directory
  pathBorder <- paste0(mainPath,"/",toupper(region),"/data/raw/vBorders")
  if(!dir.exists(pathBorder)){
    stop(paste(pathBorder,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  vBordersFolder <- list.files(pathBorder)
  vBordersShp <- grepl(".shp",vBordersFolder)
  if(sum(vBordersShp)==0){
    stop("Administrative boundaries shapefile is missing. Run the download.border.geoboundaries function.")
  }else if(sum(vBordersShp)>1){
    selectedIndex <- menu(vBordersFolder[vBordersShp],title="\nSelect the shapefile you will use for boundaries ?")
    shp <- vBordersFolder[vBordersShp][selectedIndex]
    border <- readOGR(paste0(pathBorder,"/",shp))
    message("Loading boundaries...")
    return(border)
  }else{
    message("Loading boundaries...")
    border <- readOGR(paste0(pathBorder,"/",vBordersFolder[vBordersShp]))
    return(border)
  }
}

# Download DEM from SRTM (FABDEM only can be accessed through their website)
download.dem.srtm <- function(mainPath,region){
  # Check directory
  pathDEM <- paste0(mainPath,"/",toupper(region),"/data/raw/rDEM")
  if(!dir.exists(pathDEM)){
    stop(paste(pathDEM,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  border <- get.boundaries(mainPath,region)
  border <- gUnaryUnion(border)
  # Download SRTM tiles shapefile in a temporary folder
  tmpFolder <- paste0(mainPath,"/",toupper(region),"/data/raw/rDEM/temp")
  dir.create(tmpFolder)
  download.file(url=urlSRTM,destfile = paste0(tmpFolder,"/srtm.zip"))
  unzip(zipfile=paste0(tmpFolder,"/srtm.zip"),overwrite=TRUE,exdir=tmpFolder)
  shp <- shapefile(paste0(tmpFolder,"/srtm_country-master/srtm/tiles.shp"))
  #Intersect country geometry with tile grid
  intersects <- gIntersects(border,shp,byid=TRUE)
  tiles <- shp[intersects[,1],]
  
  #Download tiles
  srtmList  <- list()
  for(i in 1:length(tiles)){
    print(i)
    lon <- extent(tiles[i,])[1]  + (extent(tiles[i,])[2] - extent(tiles[i,])[1]) / 2
    lat <- extent(tiles[i,])[3]  + (extent(tiles[i,])[4] - extent(tiles[i,])[3]) / 2
    tile <- getData('SRTM',lon=lon,lat=lat,path=paste0(tmpFolder,"/"))
    print(tile)
    srtmList[[i]] <- tile
  }
  # Mosaic
  srtmList$fun <- mean 
  srtmMosaic   <- do.call(mosaic, srtmList)
  writeRaster(srtmMosaic,paste0(pathDEM,"/srtm.tif"),overwrite=TRUE)
  unlink(tmpFolder,recursive=TRUE)
  cat(paste0(pathDEM,"/srtm.tif","\n"))
}

# Search in FTP folders
# Used in other functions (download.population)
navigate.ftp <- function(folderLst,iso,pathFTP,pathFTP0){
  while(!(any(grepl("\\.tif$",folderLst)))){
    # If there is a folder with our region code, select it
    isoProp <- sum(grepl("^[A-Z]{3}$",folderLst))/length(folderLst)
    if(any(grepl(iso,folderLst))){
      pathFTP <- paste0(pathFTP,iso,"/")
      # If not, let the user choose
    }else{
      if(isoProp==1 & !any(grepl(iso,folderLst))){
        pathFTP <- paste0(pathFTP,"../")
        message(paste(iso,"is not available in this dataset."))
      }else{
        if(!pathFTP==pathFTP0){
          folderLst <- c(folderLst,"PREVIOUS DIRECTORY","EXIT FUNCTION")
        }else{
          folderLst <- c(folderLst,"EXIT FUNCTION")
        }
        folderNb <- menu(c(folderLst), title="\nSelect folder (type the corresponding number or zero to get back to the root directory) ?")
        if(folderNb==length(folderLst)){
          return(NULL)
        }else if(folderNb==(length(folderLst)-1)){
          pathFTP <- paste0(pathFTP,"../")
        }else if(folderNb==0){
          pathFTP <- pathFTP0
        }else{
          selectedFolder <- folderLst[folderNb]
          pathFTP <- paste0(pathFTP,selectedFolder,"/")
        }
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
    selInd <- readline(prompt = "Selection: ")
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
        cat(paste0(pathFolder,"/",folderLst[i],"\n"))
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
  border <- get.boundaries(mainPath,region)
  # Download SRTM tiles shapefile in a temporary folder
  tmpFolder <- paste0(mainPath,"/",toupper(region),"/data/raw/rLandcover/temp")
  dir.create(tmpFolder)
  download_zenodo(doi=doiLC,path=tmpFolder,files=zenodoFileLC)
  globalLandcover <- tryCatch({rast(paste0(tmpFolder,"/",zenodoFileLC))},error=function(e){stop("\nDownload issue: try to download the land cover raster manually at:\n https://doi.org/10.5281/zenodo.3939050")})
  cropLandcover <- terra::crop(globalLandcover,border)
  writeRaster(cropLandcover,paste0(pathLandcover,"/",zenodoFileLC),overwrite=TRUE)
  unlink(tmpFolder,recursive=TRUE)
  cat(paste0(pathLandcover,"/",zenodoFileLC,"\n"))
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
  selInd <- readline(prompt = "Selection: ")
  selInd <- as.numeric(unlist(strsplit(x=selInd,split=" ")))
  if(length(selInd)!=0){
    sfObject <- subset(sfObject,eval(parse(text=columnName)) %in% categories[selInd])
  }
  return(sfObject)
}

# Download shapefile from Open Street Map
downlad.osm <- function(x,mainPath,region){
  if(!(x=="roads"|x=="waterLines"|x=="waterPolygons")){
    stop("x must be 'roads', 'waterLines' or 'waterPolygons'")
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
  shapeName <- gsub(".gpkg","",list.files(pathFolder)[grepl("*.gpkg",list.files(pathFolder))])
  st_write(shp,paste0(pathFolder,"/v",str_to_title(x),"_",shapeName,".shp"),append=FALSE) # Save the layer
  file.remove(paste0(pathFolder,"/",list.files(pathFolder)[grepl("*.gpkg|*.pbf",list.files(pathFolder))]))
  cat(paste0(pathFolder,"/v",str_to_title(x),"_",shapeName,".shp","\n"))
}

# Access config.txt (created with set.param) and get EPSG
# Used in other functions
get.epsg <- function(mainPath,region){
  pathRegion <- paste0(mainPath,"/",region,"/data")
  if(!file.exists(paste0(pathRegion,"/config.txt"))){
    stop("Project main parameters have not been set yet. Run the set.param function.")
  }
  fileConn <- file(paste0(pathRegion,"/config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  epsg <- config[grepl("EPSG",config)]
  return(epsg)
}

select.DirFile <- function(x,msg){
  n <- 1:length(x)
  indInput <- paste(paste0("\n",n,": ",x))
  cat(indInput)
  cat(paste0("\n\n",msg,"\n"))
  selInd <- readline(prompt = "Selection: ")
  selInd <- as.numeric(unlist(strsplit(x=selInd,split=" ")))
  if(length(selInd)!=0){
    selectedX <- x[selInd]
  }else{
    selectedX <- x
  }
  return(selectedX)
}


cropMaskProject.raster <- function(ras,borderInit,epsg){
  if(!compareCRS(ras, as(borderInit,"SpatVector"))){
  border <- st_transform(as(borderInit,"sf"),crs(ras))
  }else{
  border <- borderInit
  }
  cat(paste("\nCropping:\n",ras %>% sources()))
  rasCrop <- terra::crop(ras,border)
  cat(paste("\nMasking:\n",ras %>% sources()))
  rasMask <- terra::mask(rasCrop,as(border,"SpatVector"))
  reprojectionMethod <- c("near","bilinear","cubic","cubicspline")
  bn <- menu(reprojectionMethod, title=cat(paste0("\nSelect reprojection method for:\n",ras %>% sources(),"\nSee terra::project function help for more details.")))
  if(bn==0){
    return(0)
  }else{
    cat(paste("\nReprojecting:\n",ras %>% sources()))
    rasProject <- terra::project(rasMask,epsg,method=reprojectionMethod[bn])
  }
  return(rasProject)
}

clipProject.shapefile <- function(shp,borderInit,epsg,inputName){
  if(!compareCRS(shp,borderInit)){
    border <- st_transform(as(borderInit,"sf"),crs(shp))
  }else{
    border <- borderInit
  }
  cat(paste("\nClipping:",inputName,"shapefile"))
  shpInter <- st_intersects(shp,as(border,"sf")) 
  shpInter <- lengths(shpInter)>0
  shpClip <- shp[shpInter,]
  shpProject <- st_transform(shpClip,st_crs(epsg))
  return(shpProject)
}

resample.raster <- function(ras1,ras0,rasInit,population=FALSE){
  if(!population){
    resamplingMethod <- c("near","bilinear","cubic","cubicspline","lanczos","sum","min","q1","q3","max","average","mode","rms")
    bn <- menu(resamplingMethod, title=cat(paste("\nSelect reprojection method for:\n",rasInit %>% sources(),"\nSee terra::resample function help for more details.")))
    if(bn==0){
      return(0)
    }else{
      cat(paste("\nResampling:\n",rasInit %>% sources()))
      rasResamp <- terra::resample(ras1,ras0,method=resamplingMethod[bn])
    }
  }else{
    cat(paste("\nResampling:\n",rasInit %>% sources()))
    rasResamp <- terra::resample(ras1,ras0,method="bilinear")
  }
}
 
check.already.processed <- function(mainPath,region,inputName){
  prFolder <- paste0(mainPath,"/",toupper(region),"/data/processed/",inputName)
  prFiles <- list.files(prFolder)
  if(length(prFiles)>0){
    if(grepl(paste0(inputName,".tif"),prFiles)){
      yn <- menu(c("YES","NO"),title=cat(paste("\n",inputName,"was already processed. Do you want to reprocess it ?")))
      if(yn==1){
        process <- TRUE
      }else{
        process <- FALSE
      }
    }else{
      process <- TRUE
    }
    return(process)
  }else{
    return(TRUE)
  }
}

check.input <- function(folder,stopMsg,multiMsg){
  rasterLayer <- FALSE
  vectorLayer <- FALSE
  files <- list.files(folder)
  if(length(files)>0){
    filesTif <- files[grepl("*.tif",files)]
    if(length(filesTif)>0){
      rasterLayer <- TRUE
    }
    filesShp <- files[grepl("*.shp",files)]
    if(length(filesShp)>0){
      vectorLayer <- TRUE
    }
  }else{
    stop(stopMsg)
  }
  if(rasterLayer){
    if(length(filesTif)>1){
      fileInd <- menu(filesTif,multiMsg)
      file <- filesTif[fileInd]
    }else{
      file <- filesTif
    }
    ras <- rast(paste0(folder,"/",file))
  }else{
    ras <- NULL
  }
  if(vectorLayer){
    if(length(filesShp)>1){
      fileInd <- menu(filesShp,multiMsg)
      file <- filesShp[fileInd]
    }else{
      file <- filesTif
    }
    shp <- st_read(folder,file,quiet=TRUE)
  }else{
    shp <- NULL
  }
  return(list(ras,shp))
}
 
process.inputs <- function(mainPath,region){
  epsg <- get.epsg(mainPath,region)
  pathRaw <- paste0(mainPath,"/",region,"/data/raw")
  if(!dir.exists(pathRaw)){
    stop(paste(pathRaw,"does not exist. You may run the project.dir function first or check the input parameters."))
  }
  rawFolders <- list.dirs(pathRaw,recursive=FALSE)
  if(length(rawFolders)==0){
    stop(paste(pathRaw,"is empty. You may run the project.dir function first or check the input parameters."))
  }else{
    borderInit <- get.boundaries(mainPath,region)
    folders <- gsub("^.*/raw/","",rawFolders)
    selectedFolders <- select.DirFile(folders,"Enter all the indices that correspond to the inputs you want to process (on the same line separated by a space, or just skip to select all inputs)")
    # Check if some rasters to be processed
    filesRasTrue <- NULL
    for(i in 1:length(selectedFolders)){
      files <- list.files(paste0(pathRaw,"/",selectedFolders[i]))
      filesRasTrue <- c(filesRasTrue,any(grepl(".tif",files)))
    }
    if(any(filesRasTrue)){
      processPop <- check.already.processed(mainPath,region,"rPopulation")
      if(processPop){
        popFolder <- paste0(mainPath,"/",toupper(region),"/data/raw/rPopulation")
        stopMsg <- "Population raster required for resampling is missing. Run the download.population function."
        multipleFilesMsg <- "Select the population raster that you want use for resampling."
        popRas <- check.raster(popFolder,stopMsg,multipleFilesMsg)
        popReproj <- cropMaskProject.raster(popRas,borderInit,epsg)
        if(popReproj==0){
          stop("You exit the script.")
        }
        # Initial resolution
        resInit <- terra::res(popReproj)
        yn <- menu(c("YES","NO"), title=paste("\nThe resolution of the population raster is",round(resInit[1],2),"m. Would you like to modify it ?"))
        if(yn==0){
          stop("You exit the script.")
        }else if(yn==1){
          cat("\n\nEnter the new resolution (m)\n")
          newRes <- readline(prompt = "Selection: ")
          newRes <- as.numeric(newRes)
          if(is.na(newRes)){
            message("Invalid resolution !")
          }
          popReprojNew <- popReproj
          res(popReproj) <- newRes
          popResampled <- resample.raster(popReprojNew,popReproj,popRas,population=TRUE)
          # Multiplying the resulting raster by the pixel surface ratio
          popFinal <- popResampled*((100/resInit[1])^2)
        }else{
          popFinal <- popProcessed
        }
        # Reprojecting a raster always causes some (small) distortion in the grid of a raster
        # correcting for loss of population due to this distortion is necessary
        popSum <- exact_extract(popRas,st_transform(as(borderInit,"sf"),crs(popRas)),"sum")
        popFinalSum <- exact_extract(popFinal,st_transform(as(borderInit,"sf"),crs(popFinal)),"sum")
        border <- st_transform(as(borderInit,"sf"),crs(popFinal))
        border$pop_diff <- popSum/popFinalSum
        zonalStat  <- fasterize(border,as(popFinal,"Raster"),"pop_diff") # make a raster of the correction factors to correct loss of population
        popOut <- popFinal*as(zonalStat,"SpatRaster") # final population layer
        plot(popOut) # check if it worked
        res(popOut) # check resolution
        popOutFolder <- gsub("raw","processed",popFolder)
        writeRaster(popResampled,paste0(popOutFolder,"/rPopulation.tif"),overwrite=TRUE)
      }
    }
    selectedFolders <- selectedFolders[!grepl("rPopulation",selectedFolders)]
    if(length(selectedFolders)==0){
      stop("No more input to be processed !")
    }
    for(i in 1:length(selectedFolders)){
      cat("\n")
      message(selectedFolders[i])
      stopMsg <- "Raw input is missing."
      multipleFilesMsg <- "Select the input that you want to process."
      inputLayers <- check.input(paste0(pathRaw,"/",selectedFolders[i]),stopMsg,multipleFilesMsg)
      if(!is.null(inputLayers[[1]])){
        rasReproj <- cropMaskProject.raster(inputLayers[[1]],borderInit,epsg)
        rasPop <- rast(paste0(mainPath,"/",toupper(region),"/data/processed/rPopulation/rPopulation.tif"))
        rasResampled <- resample.raster(rasReproj,rasPop,inputLayers[[1]],population=FALSE)
        outFolder <- gsub("raw","processed",paste0(pathRaw,"/",selectedFolders[i]))
        writeRaster(rasResampled,paste0(outFolder,"/",selectedFolders[i],".tif"),overwrite=TRUE)
      }
      if(!is.null(inputLayers[[2]])){
        shpProcessed <- clipProject.shapefile(inputLayers[[2]],borderInit,epsg,selectedFolders[i])
        outFolder <- gsub("raw","processed",paste0(pathRaw,"/",selectedFolders[i]))
        st_write(shpProcessed,paste0(outFolder,"/",selectedFolders[i],".shp"),append=FALSE)
      }
    }
  }
  cat("nDone !\n")
}

# CHECK IF ALL DATA READY

# Example --------------------------------------------------

mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS"
region <- "Afghanistan"

project.dir(mainPath,region)
set.param(mainPath,region,"AFG",32642,100)
download.border.geoboundaries(mainPath,region,1)
download.dem.srtm(mainPath,region)
download.population(mainPath,region)
download.landcover(mainPath,region)
downlad.osm("waterPolygons",mainPath,region)
downlad.osm("waterLines",mainPath,region)
downlad.osm("roads",mainPath,region)
process.inputs(mainPath,region)

