import ee
import subprocess
import pprint
import os

from util import get_asset_list, create_export_tasks, run_export_tasks

ee.Initialize()

# for shifting LST into Kelvin
scale  = 0.00341802
offset = 149

landsat = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")

def make_lst_image(img):
    global scale, offset
    # mask out clouds
    lst_mask = img.select("QA_PIXEL").bitwiseAnd(ee.Image.constant(64)).gt(0)

    return img.select("ST_B10").multiply(scale).add(offset).updateMask(lst_mask)

def do_one_export(asset):
    global landsat
    features = ee.FeatureCollection(asset)
    mean_lst = landsat.filter(ee.Filter.eq("PROCESSING_LEVEL", "L2SP")) \
    .filter(ee.Filter.calendarRange(10, 11, "month")) \
    .filterBounds(features) \
    .map(make_lst_image) \
    .reduce(ee.Reducer.mean())

    return ee.Image(mean_lst).reduceRegions(
        collection=features,
        reducer=ee.Reducer.first(),
        scale=30
    ).map(lambda feature: feature.setGeometry(None))

assets = get_asset_list("projects/lagos-lakes/assets/lagos_us_points_huc4")
tasks  = create_export_tasks(
    assets, ee.FeatureCollection, 
    do_one_export, "lagos_us_temp_by_huc4"
)
run_export_tasks(tasks)