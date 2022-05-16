source("C:/Users/timoner/Documents/GeoHealth/Scripts/Population_Access/code/00_main_functions.R")

# User argement
mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS"
mainPath <- "C:/Users/timoner/Documents/GeoHealth/GIS/Population_raster_distortion"

# To select the country, create the directories, and store the ISO code
# mainPath: path of the working directory; a new folder with the country name will be created in there
initiate_project(mainPath)

# User argument 
# The country name that corresponds to the created folder in the working directory
region <- "Afghanistan"
region <- "LIECHTENSTEIN"
region <- "Central_African_Republic"
region <- "Malta"

# Download boundaries from the geoBoundaries (https://www.geoboundaries.org/)
# region: the country name that corresponds to the country folder
# adminLevel: administraive level
download_boundaries(mainPath, region, adminLevel=4, alwaysDownload = FALSE)

# Set the projection for the entire project
set_projection(mainPath = mainPath, region = region, mostRecent = FALSE, alwaysSet = FALSE, bestCRS = FALSE)

# Download the DEM (SRTM); if large region, it will download multiple tiles and mosaic them
download_dem(mainPath = mainPath, region = region, alwaysDownload = FALSE, mostRecent = TRUE)

# Download the population raster; it allow to navigate through the WorldPop folders
download_population(mainPath = mainPath, region = region, alwaysDownload = FALSE)

# Download the land cover; if large region, it will download multiple tiles and mosaic them
download_landcover(mainPath = mainPath, region = region, alwaysDownload = FALSE, mostRecent = FALSE)

# Tells which input folders are still empty (raw or processed)
check_input(mainPath, region, "raw", print = TRUE)

# Download shapefiles from Open Street Map
# countryName: if TRUE search by country name as it was displayed when initiating the project (full name)
# If complex name, maybe better to set countryName=FALSE (slower but safer!)
# First argument depends on which data is targeted
download_osm(x = "waterPolygons", mainPath = mainPath, region = region, countryName = TRUE, alwaysDownload = FALSE, mostRecent = NULL)
download_osm(x = "waterLines", mainPath = mainPath, region = region, countryName = TRUE)
download_osm(x = "roads", mainPath = mainPath, region = region, countryName = TRUE)

# Process the inputs (crop, mask, project, resample, etc.)
process_inputs(mainPath, region, mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = TRUE, changeRes = TRUE, newRes = 100, popCorrection = TRUE, gridRes = 3000, all = FALSE)
process_inputs(mainPath, region, mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = FALSE, changeRes = TRUE, newRes = NULL, popCorrection = TRUE, gridRes = NULL)

# Tells which input folders are still empty (raw or processed)
check_input(mainPath = mainPath, region = region, type = "processed" , print = TRUE)

# Check directory tree
dir_tree(paste0(mainPath, "/", region, "/data"))

# Copy all processed inputs to a "toAccessMod" folder
compile_processed_data (mainPath = mainPath, region = region, mostRecent = TRUE)
