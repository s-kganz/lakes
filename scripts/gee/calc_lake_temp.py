import ee
import subprocess
import pprint
import os

from util import get_asset_list, create_export_tasks, run_export_tasks

ee.Initialize()

landsat = ee.ImageCollection("LANDSAT/LC08/C01/T1")
modis   = ee.Image("OpenLandMap/CLM/CLM_LST_MOD11A2-DAY_M/v01")

def calibrateRadWithProperties(img):
  return ee.Algorithms.Landsat.calibratedRadiance(img) \
    .set("DATE_ACQUIRED", img.get("DATE_ACQUIRED"))

def inversePlanck(img):
  # convert radiance to blackbody temperature
  # https:#ncc.nesdis.noaa.gov/data/planck.html
  um = ee.Image.constant(10.9) # wavelength in um for B10
  c1     = ee.Image.constant(1.191042e8)
  c2     = ee.Image.constant(1.4387752e4)
  
  lst = c2.divide(
    um.multiply(
      c1.divide(um.pow(ee.Image.constant(5)).multiply(img)).add(1).log()
    )
  )
  
  lst = lst.set("system:time_start", img.get("DATE_ACQUIRED"))
  return lst

def cloudMask(img):
  # the cloud bit is #4, 2^4 = 16
  return img.updateMask(
    img.select("BQA").bitwiseAnd(ee.Image.constant(16)).eq(0)
  )

# Spring turnover anomaly
lst_anomaly = landsat.filter(ee.Filter.calendarRange(3, 5, "month")) \
  .map(cloudMask) \
  .map(
    calibrateRadWithProperties
  ).select("B10").map(
    inversePlanck
  ).mean() \
  .subtract(
    modis.select(["may", "apr", "mar"]).reduce(ee.Reducer.mean())\
      .multiply(0.02).focal_median(3) # kernel radius in px, effectively 3km
  )

def do_one_export(asset):
    global lst_anomaly
    features = ee.FeatureCollection(asset)

    return lst_anomaly.reduceRegions(
        collection=features,
        reducer=ee.Reducer.first(),
        scale=30
    ).map(lambda feature: feature.setGeometry(None))

assets = get_asset_list("projects/lagos-lakes/assets/lagos_us_points_huc8")
tasks  = create_export_tasks(
    assets, ee.FeatureCollection, 
    do_one_export, "lagos_us_temp_by_huc8"
)
run_export_tasks(tasks)