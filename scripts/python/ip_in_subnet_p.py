#!/usr/bin/env python3
##
import ipaddress
import sys

ip = ipaddress.IPv4Address(sys.argv[1])
subnet = ipaddress.IPv4Network(sys.argv[2])

sys.exit(0 if ip in subnet else 1)
