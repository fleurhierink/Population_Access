# Population_Access
Population Access is the github repository for spatial data preparation and processing for any type of geographic accessibility analysis. Here we focus specifically on geographic access to health care in sub-Saharan Africa. In this project we compared the difference in accessibility coverage estimates for six gridded population datasets: 1) WorldPop top-down constrained, 2) WorldPop top-down unconstrained, 3) HRSL, 4) GPWv4, 5) Landscan, and 6) Global Human Settlement Population (GHS-POP). 

This github page presents the automatized workflow to process all the data for the 50 sub-Saharan African countries. We always advocate for more contextualized outputs, using local input data to estimate accessibility to health care as realistically as possible. Our team uses [AccessMod 5](https://github.com/fxi/AccessMod_shiny) for most accessibility analyses because the modules allow more specific adjustments of input parameters, bringing more realism to the model. Here we share an automatized workflow that can be used for data preparation on a large scale. 

## Contents
The R scripts are made to clip, project and prepare all input data for a geographic accessibility analysis. In addition it uses code to extract the most recent OpenStreetMap layers. The data preparation includes:
* **Landcover**: clipping and projection (R-script, 01_data_prep_landcover.R)
* **Digital Elevation Model**: data fetching, clipping, and projecting (R-script, 02_data_prep_dem_download.R & 02_data_prep_dem_process.R)
* **Roads**: data fetching and projecting (R-script, 03_data_prep_roads.R)
* **Hydrography**: data fetching of line and polygon features (R-script, 04_data_prep_hydro_lines.R & 05_data_prep_hydro_poly.R)
* **Landcover merge**: combining all input data in a merged land cover to which a travel scenario can be applied (R-script, 06_data_prep_merge_landcover.R)
* **Friction layer**: the transformation of a land cover merge to a friction layer that presents the cost of traversing a cell (R-script, 07_friction_layer.R)
* **Health facility location**: clipping point features to countries and projecting (R-script, 08_health_facilities.R)
* **Accessibility analyis**: cost-distance algorithm in arcpy that appliesa the eight directional least-cost path to the friction layer overlaid with health facility location (09_accessibility_analysis.py).
* **Gridded population data**: the fetching of some and preparation of several gridded population datasets (R-scripts, 10_download_population_worldpop.R & 11_clip_population.R)
* **Accessibility coverage statistics**: calculation of the population covered in several travel time catchments (i.e., 30, 60, 120, 150, and 180 minutes) (R-scripts, 13_extract_coverage_X.R)

## Citation
The paper in which these scripts have been developed and applied is currently in preparation. Please cite this github page when using code.

