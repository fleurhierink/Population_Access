#!/usr/bin/env python
import arcpy
import os,sys
from arcpy.sa import *
from arcpy import env
arcpy.CheckOutExtension("Spatial")

# Enable overwrite
arcpy.env.overwriteOutput = True

facilitiesStr = "data\\processed\\vHealth_facilities\\"
facilityfiles = [f for f in os.listdir(facilitiesStr) if f.endswith('.shp')]
facilityfiles = map(lambda name: os.path.join(facilitiesStr, name), facilityfiles)
print(facilityfiles)
cost_layerStr = "data\\processed\\rFriction_1m\\"
costfiles = [f for f in os.listdir(cost_layerStr) if f.endswith('.tif')]
costfiles = map(lambda name: os.path.join(cost_layerStr, name), costfiles)
print(costfiles)

# Create the output extension
output_traveltime = "data\\processed\\rTravel_times\\"

# 55 sub-Saharan countries
for i in range(0,56):
	print(i)
	cost_layer = arcpy.MakeRasterLayer_management(costfiles[i])
	facilities = arcpy.MakeFeatureLayer_management(facilityfiles[i])
	arcpy.env.extent = "MAXOF"
# 	# Execute CostDistance
	outCostDistance = CostDistance(facilities, cost_layer,"99999","")
	outCostDistance.save("%stravel_time_%s.tif" % (output_traveltime,i))
    
## THE END ##
