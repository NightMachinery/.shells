#!/usr/bin/env python3

import aria2p, os, sys
from IPython import embed

cwd = os.getcwd()

# initialization, these are the default values
a2 = aria2p.API(
    aria2p.Client(
        host="http://localhost",
        port=6800,
        secret=os.environ.get('ARIA_SECRET','')
    )
)

## TODO don't add duplicate download, just resume the previous one
# list downloads
downloads = a2.get_downloads()

for download in downloads:
    print(download.__dict__)
    # embed()
    pass

dl = a2.add_uris([sys.argv[1]], { 'dir' : cwd, 'out' : sys.argv[2] })
print(dl.gid)
# embed()
