#!/usr/bin/env python

##
# Using the object idletime you can delete all keys that have not been used since three months. It is not exactly what you ask. If you created a key 6 months ago, but the key is accessed everyday, then idletime is updated and this script will not delete it. I hope the script can help:
##

import os
add_ttl = bool(os.environ.get("redis_add_ttl", ''))
if add_ttl:
    print("ttl mode")

import redis
r = redis.StrictRedis(host='localhost', port=6379, db=0)
for key in r.scan_iter("*"):
    key_str = str(key, 'utf-8')
    idle = r.object("idletime", key) # restarting redis resets all idletimes to zero
    if idle < 3600:
        print(f"{key_str} idletime: {idle}")
    if idle > 7776000:
    # idle time is in seconds. This is 90days
        print(f"Deleting {key_str}")
        r.delete(key)
    ## Add expiration to all keys that don't have it.
    if add_ttl and r.ttl(key) == -1:
        print(f"Adding ttl to {key_str}")
        r.expire(key, 60 * 60 * 24 * 7)
    #     # This would clear them out in a week
    ##
