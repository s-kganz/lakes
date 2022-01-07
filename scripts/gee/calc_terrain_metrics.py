import ee
import subprocess
import pprint
import os
ee.Initialize()

SCALE = 30  # m
srtm = ee.Image("USGS/SRTMGL1_003").resample("bilinear")

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


def doReduceRegion(image, collection, fun, kernel, stat):
    global superReducer
    if fun is None:
        # this is just an identity
        return (image.reduceRegions(
                collection=collection,
                reducer=superReducer,
                scale=SCALE
                )
                .map(removeGeometry)
                .map(lambda x: x.set("stat", stat)))
    elif kernel is None:
        # this is a function that doesn't need a kernel
        return (
            fun(image).reduceRegions(
                collection=collection,
                reducer=superReducer,
                scale=SCALE
            )
            .map(removeGeometry)
            .map(lambda x: x.set("stat", stat))
        )
    else:
        # this is a function that needs a kernel
        return (
            fun(image, kernel).reduceRegions(
                collection=collection,
                reducer=superReducer,
                scale=SCALE
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
        doReduceRegion(srtm, collection, deviation, k50, "dev50"),
        doReduceRegion(srtm, collection, deviation, k100, "dev100"),
        doReduceRegion(srtm, collection, deviation, k500, "dev500"),
        doReduceRegion(srtm, collection, deviation, k1000, "dev1000"),
        doReduceRegion(srtm, collection, relief, k50, "relief50"),
        doReduceRegion(srtm, collection, relief, k100, "relief100"),
        doReduceRegion(srtm, collection, relief, k500, "relief500"),
        doReduceRegion(srtm, collection, relief, k1000, "relief1000"),
        # scale-independent function
        doReduceRegion(srtm, collection, ee.Terrain.slope, None, "slope"),
        # identity on things that are already calculated
        doReduceRegion(srtm, collection, None, None, "elev"),
    ])
    return ee.FeatureCollection(reductions).flatten()


# collect all of the collections to be reduced over
assets = subprocess.Popen(
    "earthengine ls projects/lagos-lakes/assets/lagos_us_200m_strips",
    shell=True, stdout=subprocess.PIPE
).communicate()[0].decode("utf-8").split("\n")
print(os.path.split(assets[1])[1])
# prepare all the exports
tasks = [
    ee.batch.Export.table.toDrive(
        doOneExport(ee.FeatureCollection(asset).select(
            ["lagoslakei", "hu8_zoneid"])),
        description=os.path.split(asset)[1],
        folder="lagos_us_by_huc8"
    ) for asset in assets
]

# send them all to the server
for t in tasks:
    t.start()
