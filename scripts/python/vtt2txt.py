#!/usr/bin/env python
""" Outputs lots of duplicates. Use vtt2txt2.py instead."""


import webvtt, sys

for caption in webvtt.read(sys.argv[1]):
    # print(caption.start)
    # print(caption.end)
    print(caption.text)
