#!/usr/bin/env python3
##
import os
import platform
import json
import subprocess
import torch
import socket
import psutil
import humanize
import sys
import traceback
from pynight.common_package import packages_commit_get


def traceback_print():
    traceback.print_exc()


def system_info_get():
    hostname = socket.gethostname()
    total_ram = psutil.virtual_memory().total
    total_ram_humanized = humanize.naturalsize(total_ram, binary=True)

    system_info = {
        "hostname": hostname,
        "total_ram": total_ram_humanized,
    }

    return system_info


def gpu_info_get():
    all_device_metadata = []

    for device_idx in range(torch.cuda.device_count()):
        device = torch.device(f"cuda:{device_idx}")
        metadata = dict()

        device_name = str(device)
        metadata["device_name"] = device_name

        try:
            device_properties = torch.cuda.get_device_properties(device)
            device_properties_dict = {
                "name": device_properties.name,
                "major": device_properties.major,
                "minor": device_properties.minor,
                "total_memory": humanize.naturalsize(
                    device_properties.total_memory, binary=True
                ),
                "multi_processor_count": device_properties.multi_processor_count,
            }
            metadata["device_properties"] = device_properties_dict
        except:
            traceback_print()
            print("Continuing despite the error ...", file=sys.stderr)

        all_device_metadata.append(metadata)

    return all_device_metadata


def get_installed_python_libraries():
    try:
        # Using pip to fetch installed libraries
        result = subprocess.check_output(["pip", "list"]).decode()
        lines = result.split("\n")[2:]
        libraries = {}
        for line in lines:
            if line:
                name, version = line.strip().split()[:2]
                libraries[name] = version
        return libraries
    except:
        # If pip is not found or there's an error, return None
        return None


def computing_infrastructure_get():
    infrastructure = system_info_get()
    infrastructure.update(
        {
            "CPU": platform.processor(),
            "OS": platform.system() + " " + platform.release(),
            "GPU_info": gpu_info_get(),
            "python_libraries": get_installed_python_libraries(),
            "package_commits": packages_commit_get(
                [
                    "captum",
                    "timm",
                    "decompv",
                    "pynight",
                ],
                import_p=True,
            ),
        },
    )
    return infrastructure


if __name__ == "__main__":
    info = computing_infrastructure_get()
    print(json.dumps(info, indent=4))
