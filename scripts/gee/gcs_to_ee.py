from glob import glob
import subprocess
import os

bucket_base = "gs://lagos-200m-strip-shp/by_huc8/"
asset_base  = "projects/lagos-lakes/assets/lagos_us_200m_strips/"

shps = map(
    lambda x: os.path.split(x)[1].replace(".shp", ""), 
    glob("data_working/lagosus/by_huc8/*.shp")
)

shps = ['hu8_zoneid_hu8_1638', 'hu8_zoneid_hu8_1777']
for shp in shps:
    # create the earthengine call
    shp_source = bucket_base + shp + ".shp"
    shp_dest   = asset_base + shp
    cmd = "earthengine upload table --asset_id={} {}".format(shp_dest, shp_source)
    subprocess.run(cmd, shell=True)