#!/usr/bin/env python3
#: @forked from https://github.com/PrivacyDevel/OneDrive-MassDL-Cli
#: AGPL-3.0 license
##

import json
import sys
import urllib.parse
import os
import argparse
import itertools
from icecream import ic

import requests


parser = argparse.ArgumentParser()
parser.add_argument(
    "URL", help="https://onedrive.live.com/?authkey=XXXXX&id=XXXXX&cid=XXXXX"
)
parser.add_argument(
    "-f",
    "--force-dirs",
    action="store_true",
    help="crawls through directories even if they are unchanged",
)
args = parser.parse_args()


def download_onedrive_folder(authkey, id, cid, folder):

    data = {
        "gb": "0,1,2",
        "d": 1,
        "authKey": authkey,
        "id": id,
        "cid": cid,
        "ps": 2500,  # max page size
    }

    headers = {"AppId": "1141147648", "Accept": "application/json"}

    # for count in itertools.count(data['ps'], data['ps']):
    for count in [1]:
        response = requests.get(
            "https://skyapi.onedrive.live.com/API/2/GetItems",
            params=data,
            headers=headers,
        )
        response.raise_for_status()

        items = response.json()["items"]
        # ic(len(items))

        for item in items:
            # ic(item)

            itemType = item.get("itemType", None)

            if "folder" in item:
                children = item["folder"]["children"]
            elif itemType == 32:
                children = item["children"]
            else:
                children = [item]

            for child in children:
                # if folder
                if child["itemType"] == 32:
                    path = folder + "/" + item["name"] + "/" + child["name"]
                    os.makedirs(path, exist_ok=True)
                    ic(path)
                    download_onedrive_folder(
                        data["authKey"],
                        child["id"],
                        child["resourcePartitionCid"],
                        path,
                    )

                # if file
                elif "urls" in child and "download" in child["urls"]:
                    url = child["urls"]["download"]

                    if True:
                        print(url)
                    else:
                        path = folder + "/" + child["name"]
                        if "extension" in child:
                            path += child["extension"]
                        ic(path)
                        response = requests.get(url)
                        response.raise_for_status()
                        with open(path, "wb") as f:
                            f.write(response.content)
                else:
                    raise Exception("Unknown itemType " + str(item["itemType"]))

            # if count < item["folder"]["childCount"]:
            #     data["si"] = count
            # else:
            #     return


url = urllib.parse.urlparse(args.URL)
params = urllib.parse.parse_qs(url.query)

data = {
    "authKey": urllib.parse.unquote(params["authkey"][0]),
    "id": urllib.parse.unquote(params["id"][0]),
    "cid": params["cid"],
}
download_onedrive_folder(data["authKey"], data["id"], data["cid"], "downloads")
