##' Title: Script for the data preparation of input data for AccessMod
##' Author: Fleur Hierink and Pablo Timoner
##' Organisation: University of Geneva
##' Date: 2022

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
  cat("WARNING: Population, DEM, landcover rasters and health facilities shapefile have to be downloaded manually and \ncopied into their respective folders: /data/raw/[variable]")
  cat("WARNING: Other main inputs can be dowloaded automatically using the download.[variable] functions.")
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

# Subset based on categories for automatically downloaded shapefiles
# Used in other functions
selectCategories <- function(sfObject,columnName){
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


# While we don't have any tif file
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
        download.file(url=filePath,destfile = paste0(pathFolder,"/",folderLst[i]),quiet=FALSE,mode="a")
      }
      downloadProcess <- FALSE
    }
  }
}



# Download administrative boundaries
download.zones <- function(mainPath,region,adminLevel){
  # Check directory
  pathBound <- paste0(mainPath,"/",toupper(region),"/data/raw/vZones")
  if(!dir.exists(pathBound)){
    stop(paste(pathBound,"does not exist. Run the project.dir function first or check the input parameters."))
  }
  # Get country code
  iso <- get.iso(mainPath,region)
  # Download the data and save it as a shapefile
  bound <- tryCatch({getData('GADM',country=iso,level=adminLevel,path=pathBound)},error=function(cond){stop(cond)})# this automatically downloads the borders from GADM, the level indicates the sub-national borders (i.e. 0 = country borders, 1 = province, 2 = district, etc.)
  bound <- as(bound, "sf")
  st_write(bound,paste0(pathBound,"/vZones_r.shp"),append=F)
  # Remove temporary file
  file.remove(paste0(pathBound,"/",list.files(pathBound)[grepl("*.rds",list.files(pathBound))]))
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
  
  shp <- selectCategories(shp,colName)
  st_write(shp,paste0(pathFolder,"/v",str_to_title(x),"_r.shp"),append=FALSE) # Save the layer
  file.remove(paste0(pathFolder,"/",list.files(pathFolder)[grepl("*.gpkg|*.pbf",list.files(pathFolder))]))
}


mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS"
region <- "Afghanistan"

project.dir(mainPath,region)
set.param(mainPath,region,"AFG",3642,100)
get.iso("C:/Users/timoner/Documents/GeoHealth/HeRAMS","Afghanistan")
downlad.osm("waterPoly",mainPath,region)
download.population(mainPath,region)
