import ee
import subprocess
import pprint
import os
from datetime import datetime
import time

from util import get_asset_list, create_export_tasks, run_export_tasks

ee.Initialize()

landsat = ee.ImageCollection("LANDSAT/LC08/C01/T1")
era5    = ee.ImageCollection("ECMWF/ERA5/MONTHLY")

def calibrateRadWithProperties(img):
  return ee.Algorithms.Landsat.calibratedRadiance(img)\
    .set({
      "system:time_start"  : img.get("system:time_start"),
      "K1_CONSTANT_BAND_10": img.get("K1_CONSTANT_BAND_10"),
      "K2_CONSTANT_BAND_10": img.get("K2_CONSTANT_BAND_10"),
      "K1_CONSTANT_BAND_11": img.get("K1_CONSTANT_BAND_11"),
      "K2_CONSTANT_BAND_11": img.get("K2_CONSTANT_BAND_11"),
    })

def brightnessTemp(img):
  # get brightness temperature from bands 10 and 11
  # K1 and K2 from the landsat metadata
  k1_b10 = img.get("K1_CONSTANT_BAND_10")
  k1_b11 = img.get("K1_CONSTANT_BAND_11")
  k2_b10 = img.get("K2_CONSTANT_BAND_10")
  k2_b11 = img.get("K2_CONSTANT_BAND_11")
  
  
  bt10 = ee.Image.constant(k2_b10) \
    .divide(
      ee.Image.constant(k1_b10).divide(img.select("B10")).add(1).log()
    ).rename("bt10")
  
  bt11 = ee.Image.constant(k2_b11) \
    .divide(
      ee.Image.constant(k1_b11).divide(img.select("B11")).add(1).log()
    ).rename("bt11")
    
  return img.addBands([bt10, bt11])

def atmosphericCorrection(img):
  # constants derived from the whole water column fit in Du et al. (2015)
  # and band-specific water emissivities in Vanhellemont (2020)
  b0 = ee.Number(-0.41165)
  b7 = ee.Number(0.24468)
  # c1 = b1 + b2 * ((1 - ebar) / ebar) + b3 * deltae * ebar**-2
  c1 = ee.Number(1.005302438559043)
  # c2 = b4 + b5 * ((1 - ebar) / ebar) + b6 * deltae * ebar**-2
  c2 = ee.Number(3.906322944083171)
  meanBT   = img.select("bt10").add(img.select("bt11")).divide(2)
  diffBT   = img.select("bt10").subtract(img.select("bt11")).divide(2)
  sqdiffBT = img.select("bt10").subtract(img.select("bt11")).pow(2)
  
  # water surface temperature
  wst = ee.Image.constant(b0).add(
    meanBT.multiply(ee.Image.constant(c1))
  ).add(
    diffBT.multiply(ee.Image.constant(c2))
  ).add(
    sqdiffBT.multiply(ee.Image.constant(b7))
  ).rename("wst")
  
  return img.addBands(wst)

def cloudMask(img):
  # the cloud bit is #4
  return img.updateMask(
    img.select("BQA").bitwiseAnd(ee.Image.constant(16)).eq(0)
  )

def waterIndex(img):
  return img.addBands(img.normalizedDifference(["B3", "B6"]).rename("water_index"))

def addDate(image):
  date = ee.Image(image).date()
  yearBand = ee.Image.constant(date.get("year")).uint16().rename("year")
  doyBand  = ee.Image.constant(date.getRelative("day", "year")).uint16().rename("doy")
  
  return image.addBands([yearBand, doyBand])

def monthly_wst_anomaly(asset, month):
  features = ee.FeatureCollection(asset)
  landsat_wst = landsat \
    .filter(ee.Filter.calendarRange(month, month, "month"))\
    .map(cloudMask)\
    .map(
      calibrateRadWithProperties
    ).map(
      brightnessTemp
    ).map(
      atmosphericCorrection
    ).map(
      waterIndex
    ).select("wst").mean()

  monthly_mean_air_t = era5\
    .filter(ee.Filter.rangeContains("month", month, month))\
    .select("mean_2m_air_temperature")\
    .mean()
    
  wst_anomaly = landsat_wst.subtract(monthly_mean_air_t)

  reduction = wst_anomaly.reduceRegions(
    collection=features,
    reducer=ee.Reducer.first(),
    scale=30
  ).map(lambda feature: feature.setGeometry(None).set("month", month))

  return ee.batch.Export.table.toDrive(
            reduction,
            # force unique names
            description=os.path.split(asset)[1] + "_{}".format(month),
            folder="lagos_us_temperature_anomaly"
        )

assets = get_asset_list("projects/lagos-lakes/assets/lagos_us_points_huc8")
tasks = [
  monthly_wst_anomaly("projects/lagos-lakes/assets/lagos_us_points_huc8/hu8_zoneid_hu8_331", 4),
  monthly_wst_anomaly("projects/lagos-lakes/assets/lagos_us_points_huc8/hu8_zoneid_hu8_1086", 1),
  monthly_wst_anomaly("projects/lagos-lakes/assets/lagos_us_points_huc8/hu8_zoneid_hu8_1097", 5)
]
run_export_tasks(tasks)
