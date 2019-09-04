#!/usr/bin/env python3
import sys, json, os
import libgenapi
l = libgenapi.Libgenapi(["http://gen.lib.rus.ec/", "http://libgen.io/"])
query = " ".join(sys.argv[1:])
if os.getenv('DEBUGME', '') != '':
    print("query: " + query, file=sys.stderr)
print(json.dumps(l.search(query)))
# l.search("Michael","author")
