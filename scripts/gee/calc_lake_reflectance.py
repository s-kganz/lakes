import os
import ee
ee.Initialize()

from util import get_asset_list, run_export_tasks

landsat = ee.ImageCollection("LANDSAT/LC08/C01/T1_TOA")

def maskL8sr(image):
  # Bit 4 is for clouds.
  cloudsBitMask = (1 << 4)
  # Bits 7-8 are cloud shadow. This can either be 00 (clear)
  # or 01 (low confidence)
  cloudShadowBitMask = (3 << 7)
  # Get the pixel QA band.
  qa = image.select('BQA')
  # Cloud flag should be set to zero.
  mask = (
      qa.bitwiseAnd(cloudsBitMask).eq(0)
        # acceptable w/ either 0 or 284
        .And(qa.bitwiseAnd(cloudShadowBitMask).lte(284))
  )
  return image.updateMask(mask)

landsat = (landsat.filter(ee.Filter.calendarRange(5, 7, "month"))
  .map(maskL8sr).median().select(["B1", "B2", "B3", "B4", "B5"]))

def doOneExport(asset):
    global landsat
    return ee.batch.Export.table.toDrive(
        landsat.reduceRegions(
          collection=ee.FeatureCollection(asset).select(["lagoslakei", "hu8_zoneid"]),
          reducer=ee.Reducer.first(),
          scale=30, # m
          # tileScale chunks the operation, which may take more time. BUT, it has no effect
          # on the resolution of the output and avoids going over the memory limit.
          # https://gis.stackexchange.com/questions/373250/understanding-tilescale-in-earth-engine
          tileScale=4
        ).map(lambda f: f.setGeometry(None)),
        description=os.path.split(asset)[1],
        folder="lagos_us_reflectance"
    )

assets = get_asset_list("projects/lagos-lakes/assets/lagos_us_points_huc8")
tasks = [doOneExport(a) for a in assets]
run_export_tasks(tasks)
    