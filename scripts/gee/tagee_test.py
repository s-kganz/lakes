from tagee import terrainAnalysis
import ee
ee.Initialize()

srtm = ee.Image("USGS/SRTMGL1_003")
geom = ee.FeatureCollection(ee.Geometry.Rectangle(-111, 40, -110.9, 40.1))

# smooth the DEM
gaussianFilter = ee.Kernel.gaussian(
  radius=3, sigma=2, units='pixels', normalize=True
)
srtmSmooth = srtm.convolve(gaussianFilter).resample("bilinear")
terrainMetrics = terrainAnalysis(srtmSmooth, geom.geometry())

# define the reducer
superReducer = ee.Reducer.median().combine(ee.Reducer.minMax(), "", True)

reduction = terrainMetrics.reduceRegions(
  geom,
  superReducer
)

print(geom.getInfo())
print(reduction.getInfo())