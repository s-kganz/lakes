from glob import glob
import subprocess
import os

bucket_base = "gs://huc8_points/"
asset_base  = "projects/lagos-lakes/assets/lagos_us_points_huc8/"
local_shp_path = "data_working/lagosus/points_by_huc8/*.shp"

shps = map(
    lambda x: os.path.split(x)[1].replace(".shp", ""), 
    glob(local_shp_path)
)

for shp in shps:
    # create the earthengine call
    shp_source = bucket_base + shp + ".shp"
    shp_dest   = asset_base + shp
    cmd = "earthengine upload table --asset_id={} {}".format(shp_dest, shp_source)
    subprocess.run(cmd, shell=True)