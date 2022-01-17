# Helper functions for GEE scripts
import subprocess
import ee
import os
ee.Initialize()


def get_asset_list(folder):
    # Return a list of assets from a folder
    return subprocess.Popen(
        "earthengine ls " + folder,
        shell=True, stdout=subprocess.PIPE
        # last item is always blank, trim it
    ).communicate()[0].decode("utf-8").split("\n")[:-1]


def create_export_tasks(assets, cast_func, export_func, folder=""):
    return [
        ee.batch.Export.table.toDrive(
            export_func(cast_func(asset)),
            description=os.path.split(asset)[1],
            folder=folder
        ) for asset in assets
    ]


def run_export_tasks(tasks, verbose=True):
    if verbose: print("Starting {} tasks...".format(len(tasks)))
    for i in range(len(tasks)):
        tasks[i].start()
        if verbose and (i % 10 == 0):
            print("{}...".format(i))