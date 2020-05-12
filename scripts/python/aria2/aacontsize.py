#!/usr/bin/env python3
import os, sys
file_name = sys.argv[1]
from IPython import embed

with open(file_name, "rb") as f:
    # https://aria2.github.io/manual/en/html/technical-notes.html


    # f.seek(0)  # Go to beginning, read VER
    # version = f.read(2)
    # i = int.from_bytes(version, 'big')
    # print(f'version is {i}')

    # skip  EXT, find info  hash_binary length
    f.seek(6)
    length = f.read(4)
    hash_length = int.from_bytes(length, 'big')
    # print(f"hash length is {hash_length}")

    # read next hash_length
    # f.seek(10)  # Go to info hash
    # hash_binary = f.read(hash_length)
    # info_hash = hash_binary.hex().upper()

    # magnet_link = "magnet:?xt=urn:btih:" + info_hash
    # print(f'file name: {os.path.basename(file_name)}, {magnet_link}')

    f.seek(10+hash_length+4)
    total_length = f.read(8)
    # embed()
    print(int.from_bytes(total_length, 'big'))
