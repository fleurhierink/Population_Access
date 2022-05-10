##' Title: Main functions for pre-processing the input data for AccessMod
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
if (!require("gdalUtils")) install.packages("gdalUtilsg"); library("gdalUtils")
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
# Get suggestion of projection based on a shapefile
if (!require("crsuggest")) install.packages("crsuggest"); library("crsuggest")
# Get ISO code
if (!require("countrycode")) install.packages("countrycode"); library("countrycode")
# Remove accent for country names
if (!require("stringi")) install.packages("stringi"); library("stringi")

# Main parameters --------------------------------------------------------------------------
# DEM
urlSRTM <- "https://github.com/sikli/srtm_country/archive/master.zip"
ftpWorldPop <- "ftp://ftp.worldpop.org.uk/GIS/Population/"
# Land cover
# First alternative: Download the global LC and then crop it. Slow, and bugs with Zenodo download
# doiLC <- "10.5281/zenodo.3939050"
# zenodoFileLC <- "PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"
# Second alternative: Based on the boundary extent, download the corresponding tiles from AWS and mosaic
awsLCFolder <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/"
awsLCSuffix <- "_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


# Main functions --------------------------------------------------------------------------
# Initiate the project. Create directories, get the ISO code
# Warnings are displayed if the folders already exist, and these are not erased
initiate_project <- function (mainPath) {
  if (!dir.exists(mainPath)) {
    stop(paste(mainPath, "folder does not exist."))
  }
  # Select the country from the codelist dataframe from the countrycode package
  if (!exists("codelist")) {
    stop("'codelist' table from 'countrycode' is missing. Load the 'countrycode' package.")
  }
  countryLst <- codelist$country.name.en[!is.na(codelist$un.name.en)]
  countryInd <- menu(countryLst, title="Select the country", graphics=TRUE)
  if (countryInd == 0) {
    message("You exit the function.")
    stop_quietly()
  }
  # Store the original name
  regionOriginalName <- countryLst[countryInd]
  # Store the ISO code
  iso3 <- as.character(codelist[codelist$country.name.en == regionOriginalName, "iso3c"])
  # Modify the name if necessary for directory name
  region <- gsub(" \\(.*\\)|\\(.*\\)", "", regionOriginalName)
  region <- gsub("[^[[:alnum:]]", " ", region)
  region <- str_squish(region)
  region <- gsub("[[:space:]]", "_", region)
  region <- stri_trans_general(str = region, id = "Latin-ASCII")
  # Main standard inputs
  inputNames=c("rDEM", "rPopulation", "rLandcover", "vRoads", "vWaterLines", 
               "vWaterPolygons", "vBorders","vFacilities")
  message(paste("\nThe following input folders will be created:", paste(inputNames,collapse=", ")))
  # Add other data ?
  yn <- menu(c("YES","NO"), title="\nDo you want to add another input folder (type 1 or 2) ?")
  if (yn == 0) {
    message("You exit the function.")
    stop_quietly()
  }
  while (yn == 1) {
    newName <- readline(prompt = "Enter the folder name: ")
    inputNames <- c(inputNames, newName)
    yn <- menu(c("YES", "NO"), title="\nDo you want to add another input folder ?")
    if (yn == 0) {
      message("You exit the function.")
      stop_quietly()
    }
  }
  # Create directories
  pathData <- paste0(mainPath, "/", toupper(region), "/data")
  dir.create(pathData, recursive = TRUE)
  for (type in c("raw", "processed")) {
    dir.create(paste0(pathData, "/", type))
    for (inputName in inputNames) {
      dir.create(paste0(pathData, "/" ,type, "/", inputName))
    }
  }
  # Create config.txt for ISO code, and then EPSG as well
  pathRegion <- paste0(mainPath, "/", region, "/data")
  fileConn <- file(paste0(pathRegion, "/config.txt"))
  writeLines(c(paste0("COUNTRY:", regionOriginalName), paste0("ISO:", iso3)), fileConn)
  close(fileConn)
  # Create log.txt for operation tracking
  fileConn <- file(paste0(pathRegion, "/log.txt"))
  writeLines(regionOriginalName, fileConn)
  writeLines(paste(Sys.time(), ": Project initiated"), fileConn)
  close(fileConn)
  # Print directory tree
  dir_tree(paste0(mainPath, "/", region, "/data"))
}



# Access config.txt (created with initiate_project)
# Used in other functions
get_iso <- function (mainPath, region) {
  pathRegion <- paste0(mainPath, "/", region, "/data")
  if (!file.exists(paste0(pathRegion,"/config.txt"))) {
    stop("Project main parameters have not been set yet. Run the initiate_project function.")
  }
  fileConn <- file(paste0(pathRegion, "/config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  iso <- config[grepl("ISO",config)]
  iso <- gsub("^[A-z]*\\:","",iso)
  return(iso)
}

get_param <- function (mainPath, region, param) {
  pathRegion <- paste0(mainPath, "/", region, "/data")
  if (!file.exists(paste0(pathRegion, "/config.txt"))) {
    stop("Project main parameters have not been set yet. Run the initiate_project function.")
  }
  fileConn <- file(paste0(pathRegion, "/config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  param <- config[grepl(param, config)]
  param <- gsub("^[A-z]*\\:", "", param)
  return(param)
}

# Access config.txt (created with initiate_project)
# Used in other functions
get_countryName <- function (mainPath,region) {
  pathRegion <- paste0(mainPath, "/", region, "/data")
  if (!file.exists(paste0(pathRegion,"/config.txt"))) {
    stop("Project main parameters have not been set yet. Run the initiate_project function.")
  }
  fileConn <- file(paste0(pathRegion,"/config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  country <- config[grepl("COUNTRY",config)]
  country <- gsub("^[A-z]*\\:","",country)
  return(country)
}

# Download administrative boundaries from geoBoundaries
download_boundaries <- function (mainPath, region, adminLevel) {
  # Check directory
  pathBorder <- paste0(mainPath,"/", region, "/data/raw/vBorders")
  if (!dir.exists(pathBorder)) {
    stop(paste(pathBorder,"does not exist. Run the initiate_project function first or check the input parameters."))
  }
  if (!adminLevel %in% c(0,1,2,3,4,5)) {
    stop("Administrative level must be an integer from 0 to 5")
  }
  # Get country code
  iso <- get_param(mainPath, region, "ISO")
  # Download the data
  border <- NULL
  adminLevelTry <- adminLevel
  while (is.null(border) & adminLevelTry >= 0) {
    message(paste("Trying", region, "administrative level", adminLevelTry))
    border <- tryCatch({geoboundaries(iso, adm_lvl = adminLevelTry, quiet = FALSE)}, error = function(e){NULL})
    adminLevelTry <- adminLevelTry - 1
  }
  if (is.null(border)) {
    stop("No available shapefile from geoBoundaries for this country/region. You might have to download it manually.\n\n")
  }
  borderMeta <- gb_metadata(iso, adm_lvl = adminLevelTry)
  # Save metadata
  write.table(borderMeta,paste0(pathBorder, "/", borderMeta$boundaryID, ".txt"))
  # Save shapefile
  st_write(border, paste0(pathBorder, "/", borderMeta$boundaryID, ".shp"), append = FALSE)
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  write(paste0(Sys.time(), ": Boundaries downloaded (admin level ", adminLevelTry, ")"), file = logTxt, append = TRUE)
  cat(paste0(pathBorder, "/", borderMeta$boundaryID, ".shp", "\n"))
}


get_boundaries <- function (mainPath, region) {
  # Check directory
  pathBorder <- paste0(mainPath, "/", region, "/data/raw/vBorders")
  if (!dir.exists(pathBorder)) {
    stop(paste(pathBorder,"does not exist. Run the initiate_project function first or check the input parameters."))
  }
  vBordersFolder <- list.files(pathBorder)
  vBordersShp <- grepl(".shp", vBordersFolder)
  if (sum(vBordersShp) == 0) {
    stop("Administrative boundaries shapefile is missing. Run the download_boundaries function.")
  }else if (sum(vBordersShp) > 1) {
    selectedIndex <- menu(vBordersFolder[vBordersShp], title="\nSelect the shapefile you will use for boundaries.")
    shp <- vBordersFolder[vBordersShp][selectedIndex]
    # border <- readOGR(paste0(pathBorder, "/", shp))
    message("Loading boundaries...")
    border <- st_read(paste0(pathBorder, "/", shp))
    return(border)
  }else{
    message("Loading boundaries...")
    # border <- readOGR(paste0(pathBorder, "/", vBordersFolder[vBordersShp]))
    border <- st_read(paste0(pathBorder, "/", vBordersFolder[vBordersShp]))
    return(border)
  }
}

# Set the projection that will be used for the entire project
set_projection <- function (mainPath, region) {
  # Get the admin boundaries
  border <- get_boundaries(mainPath, region)
  if (!exists("crs_sf")) {
    stop("'crs_sf' table from 'crsuggest' is missing. Load the 'crsuggest' package.")
  }
  validEPSG <- crs_sf$crs_code[!is.na(crs_sf$crs_units) & crs_sf$crs_units=="m" & crs_sf$crs_type == "projected"]
  # Select projection
  cat(paste("\nESPG:", suggest_top_crs(border), "seems to be the best projected coordinate reference for this region/country."))
  suggestedCRS <- tryCatch({suggest_crs(input = border, type = "projected", limit = 10, units = "m")}, error=function(e){NULL})
  if (is.null(suggestedCRS)) {
    cat("\nNo reference system can be suggested. Enter the EPSG code that you want to use for this project.")
    cat("\n ")
    selInd <- readline(prompt = "Selection: ")
    epsg <- as.numeric(selInd)
    if (!epsg %in% validEPSG) {
      stop("EPSG not valid !")
    }
  }else{
    suggestedCRS <- paste(paste("EPSG:", suggestedCRS$crs_code), gsub(" .*$", "", suggestedCRS$crs_proj4))
    suggestedCRS <- c(suggestedCRS,"Other")
    valid <- FALSE
    while (!valid) {
      selectedProj <- menu(suggestedCRS, title = "Select projection for this project", graphics=TRUE)
      if (selectedProj==0) {
        message("You exit the function.")
        stop_quietly()
      }
      if (selectedProj == length(suggestedCRS)) {
        cat("\n\nEnter the EPSG code that you want to use for this project.")
        cat("\n ")
        selInd <- readline(prompt = "Selection: ")
        epsg <- selInd
        if (!epsg %in% validEPSG) {
          message("EPSG not valid !")
          valid <- FALSE
        }else{
          valid <- TRUE
        }
      }else{
        epsg <- unlist(str_split(string = suggestedCRS[selectedProj], pattern = " "))[2]
        valid <- TRUE
      }
    }
  }
  # Write the EPSG in the config.txt file
  fileConn=file(paste0(mainPath, "/", region, "/data/config.txt"), open = "r")
  configTxt <- readLines(fileConn)
  close(fileConn)
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  if(any(grepl(paste0("EPSG:"), configTxt))){
    newValues <- gsub("EPSG:.*", paste0("EPSG:", epsg), configTxt)
    fileConn <- file(paste0(mainPath, "/", region, "/data/config.txt"), open = "w")
    writeLines(newValues, fileConn)
    close(fileConn)
    write(paste0(Sys.time(), ": Projection parameter changed (", epsg, ")"), file = logTxt, append = TRUE)
  }else{
    write(paste0("EPSG:", epsg), file = paste0(mainPath, "/", region, "/data/config.txt"), append = TRUE)
    write(paste0(Sys.time(), ": Projection parameter set (", epsg, ")"), file = logTxt, append = TRUE)
  }
  message("\n\nProjection has been set.")
}

# Download DEM from SRTM (FABDEM only can be accessed through their website)
download_dem <- function (mainPath,region) {
  # Check directory
  pathDEM <- paste0(mainPath, "/", region, "/data/raw/rDEM")
  if (!dir.exists(pathDEM)) {
    stop(paste(pathDEM, "does not exist. Run the initiate_project function first or check the input parameters."))
  }
  border <- get_boundaries(mainPath, region)
  border <- as(border, "Spatial")
  border <- gUnaryUnion(border)
  # Download SRTM tiles shapefile in a temporary folder
  tmpFolder <- paste0(mainPath, "/" ,region, "/data/raw/rDEM/temp")
  dir.create(tmpFolder)
  download.file(url = urlSRTM, destfile = paste0(tmpFolder, "/srtm.zip"))
  unzip(zipfile = paste0(tmpFolder, "/srtm.zip"), overwrite = TRUE, exdir= tmpFolder)
  shp <- shapefile(paste0(tmpFolder, "/srtm_country-master/srtm/tiles.shp"))
  intersects <- gIntersects(border, shp, byid=TRUE)
  tiles <- shp[intersects[,1],]
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  #Download tiles
  if (length(tiles) > 1) {
    srtmList  <- list()
    for (i in 1:length(tiles)) {
      cat(paste0("Downloading tile ", i, "/", length(tiles), "...\n"))
      lon <- extent(tiles[i,])[1]  + (extent(tiles[i,])[2] - extent(tiles[i,])[1]) / 2
      lat <- extent(tiles[i,])[3]  + (extent(tiles[i,])[4] - extent(tiles[i,])[3]) / 2
      tile <- getData('SRTM', lon = lon, lat = lat, path = paste0(tmpFolder,"/"))
      srtmList[[i]] <- tile
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    # Gdal mosaic (faster)
    files <- list.files(tmpFolder, pattern = "tif", full.names = TRUE)
    mosaic_rasters(gdalfile = files, dst_dataset = paste0(pathDEM, "/srtm.tif") ,of = "GTiff")
    write(paste0(Sys.time(), ": Multiple DEM tiles downloaded and mosaicked"), file = logTxt, append = TRUE)
  }else{
    lon <- extent(tiles[1,])[1]  + (extent(tiles[1,])[2] - extent(tiles[1,])[1]) / 2
    lat <- extent(tiles[1,])[3]  + (extent(tiles[1,])[4] - extent(tiles[1,])[3]) / 2
    tile <- getData('SRTM', lon = lon, lat = lat, path = pathDEM)
    write(paste0(Sys.time(), ": Single DEM tile downloaded"), file = logTxt, append = TRUE)
  }
  unlink(tmpFolder, recursive = TRUE)
  files <- list.files(pathDEM)
  filesTif <- files[grepl("*.tif", files)]
  mtime <- file.info(list.files(path = pathDEM, pattern="*.tif", full.names = TRUE))[,"mtime"]
  mostRecent <- which(order(as.POSIXct(mtime)) == 1)
  cat(paste0("\n", pathDEM, "/", filesTif[mostRecent], "\n"))
}

# Search in FTP folders
# Used in other functions (download_population)
navigate_ftp <- function (folderLst, iso, pathFTP, pathFTP0) {
  while (!(any(grepl("\\.tif$", folderLst)))) {
    # If there is a folder with our region code, select it
    isoProp <- sum(grepl("^[A-Z]{3}$", folderLst)) / length(folderLst)
    if (any(grepl(iso, folderLst))) {
      pathFTP <- paste0(pathFTP, iso,"/")
      # If not, let the user choose
    }else{
      if (isoProp == 1 & !any(grepl(iso, folderLst))) {
        pathFTP <- paste0(pathFTP,"../")
        message(paste(iso, "is not available in this dataset."))
      }else{
        if (!pathFTP == pathFTP0) {
          folderLst <- c(folderLst, "PREVIOUS DIRECTORY", "EXIT FUNCTION")
        }else{
          folderLst <- c(folderLst, "EXIT FUNCTION")
        }
        folderNb <- menu(c(folderLst), title="\nSelect folder (type the corresponding number or zero to get back to the root directory)?")
        if (folderNb == length(folderLst)) {
          return(NULL)
        }else if (folderNb == (length(folderLst)-1)) {
          pathFTP <- paste0(pathFTP, "../")
        }else if (folderNb==0) {
          pathFTP <- pathFTP0
        }else{
          selectedFolder <- folderLst[folderNb]
          pathFTP <- paste0(pathFTP, selectedFolder, "/")
        }
      }
    }
    gc()
    folderLst <- getURL(pathFTP, verbose=FALSE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x = gsub("\\r\\n", " ", folderLst), split=" "))
  }
  return(list(folderLst, pathFTP))
}

# Download population raster
download_population <- function (mainPath, region) {
  # Check directory
  pathPop <- paste0(mainPath, "/", region, "/data/raw/rPopulation")
  if (!dir.exists(pathPop)) {
    stop(paste(pathPop, "does not exist. Run the initiate_project function first or check the input parameters."))
  }
  iso <- get_param(mainPath, region, "ISO")
  pathFTP0 <- ftpWorldPop
  pathFTP <- pathFTP0
  downloadProcess <- TRUE
  while (downloadProcess) {
    # To avoid error message
    gc()
    # Get directories
    folderLst <- getURL(pathFTP, verbose = FALSE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x = gsub("\\r\\n", " ", folderLst), split=" "))
    # While we don't exit the function
    out <- navigate_ftp(folderLst, iso, pathFTP, pathFTP0)
    folderLst <- out[[1]]
    pathFTP <- out[[2]]
    if (is.null(folderLst)) {
      message("You exit the function. No file has been downloaded.")
      stop_quietly()
    }
    nFile <- 1:length(folderLst)
    indFile <- paste(paste0("\n", nFile, ": ", folderLst))
    cat(indFile)
    cat("\n\nSelect file (corresponding number) to be downloaded (if multiple files: on the same line separated by a space).\nType zero to get back to the root directory.\nSkip to exit the function.")
    cat("\n ")
    selInd <- readline(prompt = "Selection: ")
    selInd <- as.numeric(unlist(strsplit(x = selInd, split = " ")))
    if (length(selInd) == 0) {
      message("You exit the function. No file has been downloaded.")
      stop_quietly()
    }else if (0 %in% selInd) {
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    }else{
      logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
      for (i in selInd) {
        filePath <- paste0(pathFTP, folderLst[i])
        download.file(url = filePath, destfile = paste0(pathPop, "/", folderLst[i]), quiet=FALSE, mode="wb", method = "libcurl")
        write(paste0(Sys.time(), ": Population raster downloaded from ", filePath), file = logTxt, append = TRUE)
        cat(paste0(pathPop, "/", folderLst[i], "\n"))
      }
      downloadProcess <- FALSE
    }
  }
}

# Download land cover
download_landcover <- function (mainPath, region) {
  # Check directory
  pathLandcover <- paste0(mainPath, "/", region, "/data/raw/rLandcover")
  if (!dir.exists(pathLandcover)) {
    stop(paste(pathLandcover, "does not exist. Run the initiate_project function first or check the input parameters."))
  }
  border <- get_boundaries(mainPath,region)
  # Based on https://lcviewer.vito.be/download and the names of available files for downloading
  # Coordinate intervals
  seqCoord <- list(X = seq(from = -180, to = 180, by = 20), Y = seq(from = -40, to = 80, by = 20))
  # Get extent of the boundaries
  minMax <- list(X = c(extent(border)[1], extent(border)[2]), Y = c(extent(border)[3], extent(border)[4]))
  # The file name: lower X limit, upper Y limit (see tiles at https://lcviewer.vito.be/download)
  # findInterval function: upper limit
  adjustTile <- c(X = -1, y = 0)
  prefixes <- list(c("E", "N") ,c("W", "S"))
  partialTileDeg <- vector(mode = "list", length = 2)
  tileName <- NULL
  for (i in 1:length(seqCoord)) {
    seqDeg <- seqCoord[[i]]
    coords <- minMax[[i]]
    for (j in 1:length(coords)) {
      coord <- coords[j]
      if (coord %in% seqDeg) {
        if (j==1) {
          coord <- coord+1
        }else{
          coord <- coord-1
        }
      }
      if (coord > max(seqDeg) | coord < min(seqDeg)) {
        stop("Region outside the limits of the Land Cover availability.")
      }
      getPosition <- findInterval(seqDeg, vec=coord)
      partialTileDeg[[i]][j] <- seqDeg[min(which(getPosition == 1)) + adjustTile[i]]
    }
  }
  seqTilesLst <- vector(mode = "list", length = 2)
  for (i in 1:length(partialTileDeg)) {
    seqTilesLst[[i]] <- seq(partialTileDeg[[i]][1], partialTileDeg[[i]][2], by = 20)
  }
  urls <- NULL
  codeFiles <- NULL
  for (i in seqTilesLst[[1]]) {
    if (i < 0) {
      pref <- prefixes[[2]][1]
    }else{
      pref <- prefixes[[1]][1]
    }
    missing0 <- (nchar(as.character(max(seqCoord[[1]]))) - nchar(as.character(abs(i))))
    if (missing0 == 2) {
      charX <- paste0(pref, "00", abs(i))
    }else if (missing0==1) {
      charX <- paste0(pref, "0", abs(i))
    }else{
      charX <- paste0(pref, abs(i))
    }
    for (j in seqTilesLst[[2]]) {
      if (j < 0) {
        pref <- prefixes[[2]][2]
      }else{
        pref <- prefixes[[1]][2]
      }
      missing0 <- (nchar(as.character(max(seqCoord[[2]]))) - nchar(as.character(abs(j))))
      if (missing0 == 1) {
        charY <- paste0(pref, "0", abs(j))
      }else{
        charY <- paste0(pref, abs(j))
      }
      codeFiles <- c(codeFiles, paste0(charX, charY))
      urls <- c(urls, paste0(awsLCFolder, charX, charY, "/", charX, charY, awsLCSuffix))
    }
  }
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  if (length(urls) == 1) {
    download.file(urls, destfile = paste0(pathLandcover, "/", region, awsLCSuffix, ".tif"), mode = "wb")
    write(paste0(Sys.time(), ": Single landcover tile downloaded"), file = logTxt, append = TRUE)
  }else{
    # Download SRTM tiles shapefile in a temporary folder
    tmpFolder <- paste0(mainPath, "/", region, "/data/raw/rLandcover/temp")
    dir.create(tmpFolder)
    for (i in 1:length(urls)) {
      cat(paste0("Downloading tile ", i, "/", length(urls), "...\n"))
      download.file(urls[i], destfile = paste0(tmpFolder, "/", codeFiles[i], ".tif"), mode = "wb")
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    files <- list.files(tmpFolder, pattern = "*.tif", full.names=TRUE)
    # Gdal mosaic
    mosaic_rasters(gdalfile = files, dst_dataset = paste0(pathLandcover, "/", region, awsLCSuffix, ".tif"), of="GTiff")
    write(paste0(Sys.time(), ": Multiple landcover tiles downloaded and mosaicked"), file = logTxt, append = TRUE)
    unlink(tmpFolder, recursive = TRUE)
  }
  cat(paste0(pathLandcover, "/", region, awsLCSuffix, "\n"))
}

# Subset based on categories for automatically downloaded shapefiles
# Used in download_osm
select_categories <- function (sfObject, columnName) {
  sfDataFrame <- sfObject
  st_geometry(sfDataFrame) <- NULL
  categories <- unique(sfDataFrame[, columnName])
  nCat <- 1:length(categories)
  indCat <- paste(paste0("\n", nCat, ": ", categories))
  cat(indCat)
  cat("\n\nEnter all the indices that correspond to categories you want to keep (on the same line separated by a space, or just skip to select all categories)\n")
  selInd <- readline(prompt = "Selection: ")
  selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
  if (length(selInd) != 0) {
    categ <- categories[selInd]
    sfObject <- subset(sfObject,eval(parse(text=columnName)) %in% categ)
  } else {
    categ <- categories
  }
  return(list(sfObject, categ))
}

# Download shapefile from Open Street Map
download_osm <- function (x, mainPath, region, countryName = TRUE) {
  if (!(x == "roads" | x == "waterLines" | x == "waterPolygons")) {
    stop("x must be 'roads', 'waterLines' or 'waterPolygons'")
  }
  if (!is.logical(countryName)) {
    stop("countryName must be 'logical'")
  }
  pathFolder <- paste0(mainPath, "/", region, "/data/raw/v", str_to_title(x))
  if (!dir.exists(pathFolder)) {
    stop(paste(pathFolder, "does not exist. You may run the initiate_project function first or check the input parameters."))
  }
  if (x == "roads") {
    querySQL <- "SELECT * FROM 'lines' WHERE highway IS NOT NULL"
    colName <- "highway"
  }else if (x == "waterLines") {
    querySQL <- "SELECT * FROM 'lines' WHERE waterway IS NOT NULL"
    colName <- "waterway"
  }else{
    querySQL <- "SELECT * FROM 'multipolygons' WHERE natural IS NOT NULL"
    colName <- "natural"
  }
  # Download
  if (countryName) {
    countryName <- get_param(mainPath, region, "COUNTRY")
    shp <- oe_get(countryName,
                  quiet = FALSE,
                  query = querySQL,
                  download_directory = pathFolder,
                  force_download = TRUE)
  }else{
    border <- get_boundaries(mainPath, region)
    # Download 
    shp <- oe_get(st_bbox(border),
                  quiet = FALSE,
                  query = querySQL,
                  download_directory = pathFolder,
                  force_download = TRUE)
  }
  shpCat <- select_categories(shp, colName)
  shp <- shpCat[[1]]
  categ <- shpCat[[2]]
  shapeName <- gsub(".gpkg", "", list.files(pathFolder)[grepl("*.gpkg", list.files(pathFolder))])
  st_write(shp, paste0(pathFolder, "/v", str_to_title(x), "_", shapeName, ".shp"), append=FALSE) # Save the layer
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  write(paste0(Sys.time(), ": ", x, " dowloaded from OSM; ", paste(categ, collapse = ", ")), file = logTxt, append = TRUE)
  file.remove(paste0(pathFolder, "/", list.files(pathFolder)[grepl("*.gpkg|*.pbf", list.files(pathFolder))]))
  cat(paste0(pathFolder, "/v", str_to_title(x), "_", shapeName,".shp", "\n"))
}

# Select directories to be processed
select_DirFile <- function (x, msg) {
  n <- 1:length(x)
  indInput <- paste(paste0("\n", n, ": ", x))
  cat(indInput)
  cat(paste0("\n\n", msg, "\n"))
  selInd <- readline(prompt = "Selection: ")
  selInd <- as.numeric(unlist(strsplit(x = selInd, split = " ")))
  if (length(selInd) != 0) {
    selectedX <- x[selInd]
  }else{
    selectedX <- x
  }
  return(selectedX)
}

# Process raster: crop, mask and project
process_raster <- function (ras, borderInit, epsg) {
  if (!compareCRS(ras, as(borderInit, "SpatVector"))) {
  border <- st_transform(as(borderInit, "sf"), crs(ras))
  }else{
  border <- borderInit
  }
  cat(paste("Cropping:\n", ras %>% sources()))
  rasCrop <- terra::crop(ras, border)
  cat(paste("\n\nMasking:\n", ras %>% sources()))
  rasMask <- terra::mask(rasCrop, as(border, "SpatVector"))
  reprojectionMethod <- c("near", "bilinear","cubic", "cubicspline")
  pm <- menu(reprojectionMethod, title = cat(paste0("\n\nSelect projection method for:\n", ras %>% sources(),"\nSee terra::project function help for more details.")))
  if (pm == 0) {
    return(NULL)
  }else{
    cat(paste("\nProjecting:\n", ras %>% sources(), "\n"))
    rasProject <- terra::project(rasMask, epsg, method = reprojectionMethod[pm])
    return(list(rasProject, reprojectionMethod[pm]))
  }
}

# Resample raster
resample_raster <- function (ras1, ras0, rasInit) {
  resamplingMethod <- c("near", "bilinear", "cubic", "cubicspline", "lanczos", "sum", "min", "q1", "q3", "max", "average", "mode", "rms")
  resm <- menu(resamplingMethod, title = cat(paste("\n\nSelect resampling method for:\n", rasInit %>% sources(),"\nSee terra::resample function help for more details.")))
  if (resm == 0) {
    return(NULL)
  }else{
    cat(paste("\nResampling:\n", rasInit %>% sources(),"\n"))
    rasResamp <- terra::resample(ras1, ras0, method = resamplingMethod[resm])
    return(list(rasResamp, resamplingMethod[resm]))
  }
}

# Process shapefile: clip and project
process_shapefile <- function (shp, borderInit, epsg, inputName) {
  cat(paste("\nProjecting:", inputName, "shapefile\n"))
  shp <- st_transform(shp, st_crs(epsg))
  if (!compareCRS(shp, borderInit)) {
    border <- st_transform(as(borderInit, "sf"), crs(shp))
  } else {
    border <- borderInit
  }
  cat(paste("\nClipping:", inputName, "shapefile\n\n"))
  shpInter <- st_intersects(shp, as(border, "sf")) 
  shpInter <- lengths(shpInter) > 0
  shpClip <- shp[shpInter, ]
  return(shpClip)
}

# Procees again ?
already_processed <- function (mainPath,region,inputName) {
  prFolder <- paste0(mainPath, "/", region, "/data/processed/", inputName)
  prFiles <- list.files(prFolder)
  if (length(prFiles) > 0) {
    if (any(grepl(paste0(inputName, ".tif"), prFiles))) {
      yn <- menu(c("YES", "NO"), title = cat(paste("\n", inputName, "was already processed. Do you want to reprocess it ?")))
      if (yn==1) {
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

# Check whether the input is a shapefile or a raster
load_layer <- function (folder, stopMsg, multiMsg) {
  rasterLayer <- FALSE
  vectorLayer <- FALSE
  files <- list.files(folder)
  if (length(files) > 0) {
    filesTif <- files[grepl("*.tif", files)]
    if (length(filesTif) > 0) {
      rasterLayer <- TRUE
    }
    filesShp <- files[grepl("*.shp", files)]
    if (length(filesShp) > 0) {
      vectorLayer <- TRUE
    }
  }else{
    message(stopMsg)
    ras <- NULL
    shp <- NULL
  }
  if (rasterLayer) {
    if (length(filesTif) > 1) {
      fileInd <- menu(filesTif, multiMsg)
      file <- filesTif[fileInd]
    }else{
      file <- filesTif
    }
    ras <- rast(paste0(folder, "/", file))
  }else{
    ras <- NULL
  }
  if (vectorLayer) {
    if (length(filesShp) > 1) {
      fileInd <- menu(filesShp, multiMsg)
      file <- filesShp[fileInd]
    }else{
      file <- filesTif
    }
    shp <- st_read(folder, file, quiet=TRUE)
  }else{
    shp <- NULL
  }
  return(list(ras, shp))
}
 
# Main function to process the inputs
process_inputs <- function (mainPath,region) {
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  epsg <- get_param(mainPath = mainPath, region = region, "EPSG")
  epsg <- paste0("EPSG:", epsg)
  if (length(epsg) == 0) {
    stop("EPSG for projection is not set. Run the set_projection function.")
  }
  pathRaw <- paste0(mainPath, "/", region, "/data/raw")
  if (!dir.exists(pathRaw)) {
    stop(paste(pathRaw, "does not exist. You may run the initiate_project function first or check the input parameters."))
  }
  rawFolders <- list.dirs(pathRaw, recursive=FALSE)
  if (length(rawFolders) == 0) {
    stop(paste(pathRaw, "is empty. You may run the initiate_project function first or check the input parameters."))
  }else{
    borderInit <- get_boundaries(mainPath, region)
    folders <- gsub("^.*/raw/", "", rawFolders)
    selectedFolders <- select_DirFile(folders, "Enter all the indices that correspond to the inputs you want to process (on the same line separated by a space, or just skip to select all inputs).")
    # Check if some rasters to be processed
    filesRasTrue <- NULL
    for (i in 1:length(selectedFolders)) {
      files <- list.files(paste0(pathRaw, "/", selectedFolders[i]))
      filesRasTrue <- c(filesRasTrue, any(grepl(".tif", files)))
    }
    if (any(filesRasTrue)) {
      popFolder <- paste0(mainPath, "/", region, "/data/raw/rPopulation")
      stopMsg <- "Population raster required. Run the download_population function."
      multipleFilesMsg <- "Select the population raster that you want use for resampling."
      popRas <- load_layer(popFolder, stopMsg, multipleFilesMsg)[[1]]
      processPop <- already_processed(mainPath, region, "rPopulation")
      if (processPop) {
        message(paste0("\nPopulation raster..."))
        popReprojMeth <- process_raster(popRas, borderInit, epsg)
        popReproj <- popReprojMeth[[1]]
        projMeth <- popReprojMeth[[2]]
        write(paste0(Sys.time(), ": Population raster cropped, masked and projected using the '", projMeth, "' method"), file = logTxt, append = TRUE)
        if (is.null(popReproj)) {
          message("You exit the function.")
          stop_quietly()
        }
        # Initial resolution
        resInit <- terra::res(popReproj)
        yn <- menu(c("YES", "NO"), title = paste("\nThe resolution of the population raster is", round(resInit[1], 2), "m. Would you like to modify it?"))
        if (yn == 0) {
          message("You exit the function.")
          stop_quietly()
        }else if (yn == 1) {
          cat("\nEnter the new resolution (m)\n")
          newRes <- readline(prompt = "Selection: ")
          newRes <- as.numeric(newRes)
          k <- 0
          while ((is.na(newRes) | newRes < 0) & k < 3) {
            message("Resultion must be a real positive number.")
            k <- k + 1
            newRes <- readline(prompt = "Selection: ")
            newRes <- as.numeric(newRes)
          }
          if ((is.na(newRes) | newRes < 0) & k == 3) {
            stop("Invalid resolution!")
          }
          popReprojNew <- popReproj
          res(popReproj) <- newRes
          popFinalMeth <- resample_raster(popReprojNew, popReproj, popRas)
          popFinal <- popFinalMeth[[1]]
          resampMeth <- popFinalMeth[[2]]
          write(paste0(Sys.time(), ": Population raster resampled using the '", resampMeth, "' method"), file = logTxt, append = TRUE)
        }else{
          popFinal <- popReproj
        }
        yn <- menu(c("YES", "NO"), title = "Reprojecting a raster always causes some (small) distortion in the grid of a raster.\nWould you like to correct it (see 'help' for more details)?")
        if (yn == 1) {
          cat("\nEnter the resolution of the grid for the zonal statistic used for correcting the raster (m)\n")
          gridRes <- readline(prompt = "Selection: ")
          gridRes <- as.numeric(gridRes)
          k <- 0
          while ((is.na(gridRes) | gridRes < 0) & k < 3) {
            message("Resultion must be a real positive number.")
            k <- k + 1
            gridRes <- readline(prompt = "Selection: ")
            gridRes <- as.numeric(gridRes)
          }
          if ((is.na(gridRes) | gridRes < 0) & k == 3) {
            stop("Invalid resolution!")
          }
          # Transform border for following process
          border <- st_transform(as(borderInit, "sf"), crs(popFinal))
          grd <- st_as_sf(st_make_grid(border, cellsize = gridRes))
          # We don't do that, because then, partial cells are not pixelized with fasterize
          # So border areas may become NA, leading to a loss of population when multiplied by the zonalStat raster
          # grdInter <- gIntersection(gUnaryUnion(as(border, "Spatial")), as(grd, "Spatial"), byid = TRUE)
          # grdInterPoly <- st_cast(as(grdInter, "sf"), "MULTIPOLYGON")
          cat("\nSumming values of the original population raster per grid cell\n")
          popSum <- exact_extract(popRas, st_transform(as(grd, "sf"), crs(popRas)), "sum")
          cat("\nSumming values of the processed population raster per grid cell before correction\n")
          popFinalSum <- exact_extract(popFinal, grd, "sum")
          # Ratio per grid cell
          grd$pop_diff <- popSum / popFinalSum
          # The only zones that are not going to be corrected are the ones that
          # initially had some population but that lost them with projection.
          # Ratio is infinite, which became NA in R.
          zonalStat  <- fasterize(grd, as(popFinal, "Raster"), "pop_diff")
          popOut <- popFinal * as(zonalStat, "SpatRaster")
          write(paste0(Sys.time(), ": Population raster corrected using a grid of ", gridRes, " x ", gridRes, " m cells"), file = logTxt, append = TRUE)
          cat("\nSumming values of the processed population raster per grid cell after correction\n")          
          popOutSum <- exact_extract(popOut, grd, "sum")
          message(paste0("Mean difference per grid cell (", gridRes, " x ", gridRes, ") ", "before correction: ", round(mean(popFinalSum - popSum), 2)))
          message(paste0("Mean difference per grid cell (", gridRes, " x ", gridRes, ") ", "after correction: ", round(mean(popOutSum - popSum), 2)))
          write(paste0(Sys.time(), ": Mean difference per grid cell before correction: ", round(mean(popFinalSum - popSum), 2)), file = logTxt, append = TRUE)
          write(paste0(Sys.time(), ": Mean difference per grid cell after correction: ", round(mean(popOutSum - popSum), 2)), file = logTxt, append = TRUE)
          popOutFolder <- gsub("raw", "processed", popFolder)
          writeRaster(popOut, paste0(popOutFolder, "/rPopulation.tif"), overwrite=TRUE)
        }
      }
    }
    selectedFolders <- selectedFolders[!grepl("rPopulation", selectedFolders)]
    if (length(selectedFolders) == 0) {
      message("No more input to be processed!")
      stop_quietly()
    }
    for (i in 1:length(selectedFolders)) {
      cat("\n")
      message(selectedFolders[i])
      processLayer <- already_processed(mainPath, region, selectedFolders[i])
      if (!processLayer) {
        if (i == length(selectedFolders)) {
          message("No more input to be processed!")
          stop_quietly()
        }else{
          next
        }
      }
      stopMsg <- paste("Raw input is missing for", selectedFolders[i])
      multipleFilesMsg <- "Select the input that you want to process."
      inputLayers <- load_layer(paste0(pathRaw, "/", selectedFolders[i]), stopMsg, multipleFilesMsg)
      if (!is.null(inputLayers[[1]])) {
        rasReprojMeth <- process_raster(popRas, borderInit, epsg)
        rasReproj <- rasReprojMeth[[1]]
        projMeth <- rasReprojMeth[[2]]
        write(paste0(Sys.time(), ": ", selectedFolders[i], " raster cropped, masked and projected using the '", projMeth, "' method"), file = logTxt, append = TRUE)
        rasPop <- rast(paste0(mainPath, "/", region, "/data/processed/rPopulation/rPopulation.tif"))
        rasResampledMeth <- resample_raster(rasReproj, rasPop, inputLayers[[1]])
        rasResampled <- rasResampledMeth[[1]]
        resampMeth <- rasResampledMeth[[2]]
        write(paste0(Sys.time(), ": ", selectedFolders[i], " raster resampled using the '", resampMeth, "' method"), file = logTxt, append = TRUE)
        outFolder <- gsub("raw", "processed", paste0(pathRaw, "/", selectedFolders[i]))
        writeRaster(rasResampled,paste0(outFolder, "/", selectedFolders[i], ".tif"), overwrite=TRUE)
        write(paste0(Sys.time(), ": Processed ", selectedFolders[i], " raster saved"), file = logTxt, append = TRUE)
      }
      if (!is.null(inputLayers[[2]])) {
        shpProcessed <- process_shapefile(inputLayers[[2]], borderInit, epsg, selectedFolders[i])
        write(paste0(Sys.time(), ": ", selectedFolders[i], " shapefile projected and clipped"), file = logTxt, append = TRUE)
        outFolder <- gsub("raw", "processed", paste0(pathRaw, "/", selectedFolders[i]))
        st_write(shpProcessed, paste0(outFolder, "/", selectedFolders[i], ".shp"), append=FALSE)
        write(paste0(Sys.time(), ": Processed ", selectedFolders[i], " shapefile saved"), file = logTxt, append = TRUE)
      }
    }
  }
  cat("\nDone!\n")
}

# Check which inputs have already been proceessed
check_input <- function (mainPath, region, type=NULL) {
  if (is.null(type)) {
    stop("'type' argument is missing'")
  }
  if (!type %in% c("raw","processed")) {
    stop("Type must be 'raw' or 'processed'")
  }
  # Check directory
  path <- paste0(mainPath, "/", region, "/data/", type)
  if (!dir.exists(path)) {
    stop(paste(path,"does not exist. Run the initiate_project function first or check the input parameters."))
  }
  dirs <- list.dirs(path)
  for (dir in dirs[-1]) {
    empty <- length(list.files(dir)) < 1
    if (empty) {
      cat(paste(str_to_title(type), gsub("^.*/[A-z]*/", "", dir), "is empty.\n"))
    }
  }
}


