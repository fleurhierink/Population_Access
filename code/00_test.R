devtools::install_github("ptimoner/inAccMod", build_vignettes = TRUE, build_manual = TRUE)

library(inAccessMod)

mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS"
mainPath <- "C:/Users/timoner/Documents/GeoHealth/GIS/Population_raster_distortion"
mainPath <- "C:/inAccMod"

# To select the country, create the directories, and store the ISO code
# mainPath: path of the working directory; a new folder with the country name will be created in there
initiate_project(mainPath)

# User argument 
# The country name that corresponds to the created folder in the working directory
region <- "Afghanistan"
country <- "LIECHTENSTEIN"
region <- "Mali"
country <- "Switzerland"

xlsx <- "HeRAMS_sample_data_with_multiple_responses.xlsx"
xlsx <- "HeRAMS_sample_data_set.xlsx"

pathTable <- paste0("C:/Users/timoner/Documents/GeoHealth/HeRAMS/Tests/HeRAMS_sample_data_with_multiple_responses.xlsx")
copy_input(mainPath, region, file)

filter_hf(mainPath, country, pathTable = NULL, scenario = NULL, mostRecentObs = NULL)


filter_hf(mainPath, country, pathTable = NULL, scenario = "002", mostRecentObs = NULL)

filter_hf_2(mainPath, country, pathTable = NULL, scenario = NULL, mostRecentObs = NULL)

filter_hf_3(mainPath, country, pathTable = NULL, scenario = NULL, mostRecentObs = NULL, barriers = FALSE)
filter_hf_3(mainPath, country, pathTable = NULL, scenario = "002", mostRecentObs = NULL, barriers = TRUE)
# Download boundaries from the geoBoundaries (https://www.geoboundaries.org/)
# region: the country name that corresponds to the country folder
# adminLevel: administraive level
download_boundaries(mainPath, country, adminLevel = 1, alwaysDownload = FALSE)

# Set the projection for the entire project
set_projection(mainPath = mainPath, country, mostRecent = FALSE, alwaysSet = FALSE, bestCRS = FALSE)

create_hf_shapefile(mainPath, country, mostRecentBoundaries = FALSE)

# Download the DEM (SRTM); if large region, it will download multiple tiles and mosaic them
download_dem(mainPath = mainPath, country = country, alwaysDownload = FALSE, mostRecent = TRUE)

# Download the population raster; it allow to navigate through the WorldPop folders
download_population(mainPath = mainPath, country = country, alwaysDownload = FALSE)

# Download the land cover; if large region, it will download multiple tiles and mosaic them
download_landcover(mainPath = mainPath, country = country, alwaysDownload = FALSE, mostRecent = FALSE)

# Tells which input folders are still empty (raw or processed)
check_inputs(mainPath, country, "raw", onlyPrint = TRUE)

# Download shapefiles from Open Street Map
# countryName: if TRUE search by country name as it was displayed when initiating the project (full name)
# If complex name, maybe better to set countryName=FALSE (slower but safer!)
# First argument depends on which data is targeted
download_osm(x = "naturalPolygons", mainPath = mainPath, country = country, countryName = TRUE, alwaysDownload = TRUE, mostRecent = TRUE, defaultClasses = TRUE)
download_osm(x = "waterLines", mainPath = mainPath, country = country, countryName = TRUE, alwaysDownload = TRUE, mostRecent = TRUE, defaultClasses = TRUE)
download_osm(x = "roads", mainPath = mainPath, country = country, countryName = TRUE, alwaysDownload = TRUE, mostRecent = TRUE, defaultClasses = TRUE)

# Process the inputs (crop, mask, project, resample, etc.)
process_inputs(mainPath, country, selectedInputs = "All", mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = TRUE, changeRes = TRUE, newRes = 100, popCorrection = TRUE, gridRes = 3000)
process_inputs(mainPath, country, selectedInputs = c("roads", "naturalPolygons", "rivers"), mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = TRUE, changeRes = TRUE, newRes = 100, popCorrection = TRUE, gridRes = 3000)


process_inputs(mainPath, region, mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = FALSE, changeRes = TRUE, newRes = NULL, popCorrection = TRUE, gridRes = NULL)

process_inputs(mainPath, country, selectedInputs = c("vFacilities/subProj001"), mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = FALSE, changeRes = TRUE, newRes = NULL, popCorrection = TRUE, gridRes = NULL)

process_inputs(mainPath, region, mostRecent = F, alwaysProcess = F, defaultMethods = FALSE, changeRes = TRUE, newRes = NULL, popCorrection = TRUE, gridRes = NULL)

# Tells which input folders are still empty (raw or processed)
check_inputs(mainPath, country, type = "raw", onlyPrint = TRUE)

# Check directory tree
fs::dir_tree(paste0(mainPath, "/", country, "/data"))
?dir_tree
# Copy all processed inputs to a "toAccessMod" folder
compile_processed_data (mainPath = mainPath, country = country, mostRecent = TRUE)


pathFacilities <- paste0(mainPath, "/", country, "/data/vFacilities")
tempDir <- paste0(pathFacilities, "/temp")
dir.create(tempDir)
analysis_scenario(inAccMod::hf_attributes)

