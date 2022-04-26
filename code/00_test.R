source("./code/00_main_functions.R")

# User argement
mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/Tests"
mainPath <- "C:/Users/timoner/Documents/GeoHealth/GIS/Population_raster_distortion"

# To select the country, create the directories, and store the ISO code
# mainPath: path of the working directory; a new folder with the country name will be created in there
initiate_project(mainPath)

# User argument 
# The country name that corresponds to the created folder in the working directory
region <- "Afghanistan"
region <- "Switzerland"
region <- "Timor_Leste"

# Download boundaries from the geoBoundaries (https://www.geoboundaries.org/)
# region: the country name that corresponds to the country folder
# adminLevel: administraive level
download_boundaries(mainPath,region,adminLevel=2)

# Set the projection for the entire project
set_projection(mainPath,region)

# Download the DEM (SRTM); if large region, it will download multiple tiles and mosaic them
download.dem(mainPath,region)

# Download the population raster; it allow to navigate through the WorldPop folders
download_population(mainPath,region)

# Download the land cover; if large region, it will download multiple tiles and mosaic them
download.landcover(mainPath,region)

# Tells which input folders are still empty (raw or processed)
check.input(mainPath,region,"raw")

# Download shapefiles from Open Street Map
# countryName: if TRUE search by country name as it was displayed when initiating the project (full name)
# If complex name, maybe better to set countryName=FALSE (slower but safer!)
# First argument depends on which data is targeted
downlad.osm("waterPolygons",mainPath,region,countryName=TRUE)
downlad.osm("waterLines",mainPath,region,countryName=TRUE)
downlad.osm("roads",mainPath,region,countryName=TRUE)

# Process the inputs (crop, mask, project, resample, etc.)
process.inputs(mainPath,region)

# Tells which input folders are still empty (raw or processed)
check.input(mainPath,region,"processed")
