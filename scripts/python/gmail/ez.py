#!/usr/bin/env python3
import os.path
from IPython import embed
from plumbum import local
tokenpath = local.env['HOME'] + '/.gmail.token'
credpath = local.env['HOME'] + '/.gmail.credentials.json'
import ezgmail as g
g.init(tokenFile=tokenpath, credentialsFile=credpath)
embed()
