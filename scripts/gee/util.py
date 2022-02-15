# Helper functions for GEE scripts
import subprocess
import os
import sys
import io
import pandas as pd
import ee
ee.Initialize()


def get_asset_list(folder):
    # Return a list of assets from a folder
    return subprocess.Popen(
        "earthengine ls " + folder,
        shell=True, stdout=subprocess.PIPE
        # last item is always blank, trim it
    ).communicate()[0].decode("utf-8").split("\n")[:-1]


def create_export_tasks(assets, cast_func, export_func, folder="", **kwargs):
    return [
        ee.batch.Export.table.toDrive(
            export_func(cast_func(asset), **kwargs),
            description=os.path.split(asset)[1],
            folder=folder
        ) for asset in assets
    ]


def run_export_tasks(tasks, verbose=True):
    if verbose: print("\tStarting {} tasks...".format(len(tasks)))
    for i in range(len(tasks)):
        tasks[i].start()
        if verbose and (i % 10 == 0):
            print("\t\t{}...".format(i))


def is_active(task):
    return task["metadata"]["state"] in ["PENDING", "RUNNING"]


def cancel_running_tasks():
    # Cancels all tasks that are READY or RUNNING
    tasks = ee.data.listOperations()
    to_cancel = [t for t in tasks if is_active(t)]
    if len(to_cancel) == 0:
        print("No tasks to cancel.")
        return
    
    print("Cancelling {} tasks...".format(len(to_cancel)))
    for t in to_cancel:
        ee.data.cancelOperation(t["name"])


def count_running_tasks():
    return sum(is_active(t) for t in ee.data.listOperations())

# Running this file will kill all tasks
if __name__ == "__main__":
    cancel_running_tasks()