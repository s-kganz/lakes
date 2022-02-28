import ee
import subprocess
import pprint
import os
ee.Initialize()

from util import get_asset_list, run_export_tasks

SCALE = 30  # m
TILESCALE = 4
fabdem = ee.ImageCollection("projects/sat-io/open-datasets/FABDEM")

# terrain ruggedness index
# for each cell = consider the 8-neighborhood and return the mean square
# elevation difference compared with the center cell
moore_kernel = ee.Kernel.fixed(
    3, 3,
    [
        [1, 1, 1],
        [1, 0, 1],
        [1, 1, 1]
    ],
    1, 1,
    False
)


def tri(image):
    image = ee.Image(image)
    # 8-neighborhood kernel
    img_square = image.pow(2)
    focal_sum_square = img_square.convolve(moore_kernel)
    focal_sum = image.convolve(moore_kernel)
    # TRI = sqrt(1/8 * (FSS + 8*img_square - 2 * image * FS))
    return (
        focal_sum_square
        .add(img_square.multiply(8))
        .subtract(focal_sum.multiply(image).multiply(2))
        .multiply(0.125)
        .sqrt()
    )

# largest elevation difference between the central pixel and the neighborhood
# the pixel with which there is the largest difference will either be the
# minimum or the maximum


def relief(image, kernel):
    minMax = image.reduceNeighborhood(ee.Reducer.minMax(), kernel)
    diff = image.subtract(minMax).abs()
    return diff.reduce(ee.Reducer.max())

# deviation = sqrt((X - average)^2 / SD)


def deviation(image, kernel):
    focalMean = image.focalMean(kernel=kernel)
    focalSD = image.reduceNeighborhood(ee.Reducer.sampleStdDev(), kernel)

    return image.subtract(focalMean).divide(focalSD)


def removeGeometry(feature):
    return feature.setGeometry(None)


# define the reducer for reduceRegions calls
superReducer = ee.Reducer.median().combine(
    ee.Reducer.minMax(), "", True
).combine(
    ee.Reducer.sampleStdDev(), "", True
)

# factory function for making reduceRegion calls
def doReduceRegion(imageCollection, collection, fun, kernel, stat):
    global superReducer
    imageCollection = imageCollection.filterBounds(collection)
    if fun is None:
        # this is just an identity
        return (imageCollection.map(
                lambda image: image.resample("bilinear")
            ).mosaic().reduceRegions(
                collection=collection,
                reducer=superReducer,
                scale=SCALE,
                tileScale=TILESCALE
                )
                .map(removeGeometry)
                .map(lambda x: x.set("stat", stat)))
    elif kernel is None:
        # this is a function that doesn't need a kernel
        return (
            imageCollection.map(
                lambda image: fun(image.resample("bilinear"))
            ).mosaic().reduceRegions(
                collection=collection,
                reducer=superReducer,
                scale=SCALE,
                tileScale=TILESCALE
            )
            .map(removeGeometry)
            .map(lambda x: x.set("stat", stat))
        )
    else:
        # this is a function that needs a kernel
        return (
            imageCollection.map(
                lambda image: fun(image.resample("bilinear"), kernel)
            ).mosaic()
            .reduceRegions(
                collection=collection,
                reducer=superReducer,
                scale=SCALE,
                tileScale=TILESCALE
            )
            .map(removeGeometry)
            .map(lambda x: x.set("stat", stat))
        )


# make a kernel for each length scale
k50 = ee.Kernel.circle(50, "meters")
k100 = ee.Kernel.circle(100, "meters")
k500 = ee.Kernel.circle(500, "meters")
k1000 = ee.Kernel.circle(1000, "meters")


def doOneExport(collection):
    # big list of reduceRegions calls
    reductions = ee.List([
        # scale-dependent functions (needs a scale)
        doReduceRegion(fabdem, collection, deviation, k50, "dev50"),
        doReduceRegion(fabdem, collection, deviation, k100, "dev100"),
        doReduceRegion(fabdem, collection, deviation, k500, "dev500"),
        doReduceRegion(fabdem, collection, deviation, k1000, "dev1000"),
        doReduceRegion(fabdem, collection, relief, k50, "relief50"),
        doReduceRegion(fabdem, collection, relief, k100, "relief100"),
        doReduceRegion(fabdem, collection, relief, k500, "relief500"),
        doReduceRegion(fabdem, collection, relief, k1000, "relief1000"),
        # scale-independent functions
        doReduceRegion(fabdem, collection, ee.Terrain.slope, None, "slope"),
        doReduceRegion(fabdem, collection, tri, None, "tri"),
        # identity on things that are already calculated
        doReduceRegion(fabdem, collection, None, None, "elev"),
    ])
    return ee.FeatureCollection(reductions).flatten()


# collect all of the collections to be reduced over
assets = get_asset_list("projects/lagos-lakes/assets/lagos_us_100m_strips_huc8")
# prepare all the exports
tasks = [
    ee.batch.Export.table.toDrive(
        doOneExport(ee.FeatureCollection(asset).select(
            ["lagoslakei", "hu8_zoneid"])),
        description=os.path.split(asset)[1],
        folder="lagos_us_terrain_fabdem_100m_by_huc8"
    ) for asset in [
        'projects/lagos-lakes/assets/lagos_us_100m_strips_huc8/hu8_zoneid_hu8_130a',
        'projects/lagos-lakes/assets/lagos_us_100m_strips_huc8/hu8_zoneid_hu8_130b'
    ]
]

# send everything to the server
#tasks[0].start()
run_export_tasks(tasks)
