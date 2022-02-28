import subprocess
import os
import ee
ee.Initialize()

from tagee import terrainAnalysis
from util import run_export_tasks, get_asset_list

SCALE = 30  # m
fabdem = ee.ImageCollection("projects/sat-io/open-datasets/FABDEM")

# smooth the DEM
gaussianFilter = ee.Kernel.gaussian(
  radius=3, sigma=2, units='pixels', normalize=True
)
smoothDEM = fabdem.map(
    lambda image: image.convolve(gaussianFilter)
)

superReducer = ee.Reducer.median().combine(ee.Reducer.minMax(), "", True)

def doOneExport(asset):
    global smoothDEM
    asset = ee.FeatureCollection(asset)
    terrainMetrics = smoothDEM.filterBounds(asset)\
        .map(
            lambda image: terrainAnalysis(image, asset.geometry().bounds())
        ).mosaic()

    return terrainMetrics.reduceRegions(
        asset,
        superReducer,
        scale=SCALE,
        tileScale=4
    ).map(lambda f: f.setGeometry(None))

# collect all of the collections to be reduced over
assets = get_asset_list("projects/lagos-lakes/assets/lagos_us_100m_strips_huc8")
tasks = [
    ee.batch.Export.table.toDrive(
        doOneExport(ee.FeatureCollection(asset).select(
            ["lagoslakei", "hu8_zoneid"])),
        description=os.path.split(asset)[1],
        folder="lagos_us_curvature_fabdem_100m_strips_by_huc8"
    ) for asset in ["projects/lagos-lakes/assets/lagos_us_100m_strips_huc8/hu8_zoneid_hu8_1973"]
]

# send everything to the server
#tasks[0].start()
run_export_tasks(tasks)