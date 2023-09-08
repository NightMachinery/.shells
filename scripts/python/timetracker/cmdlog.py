#!/usr/bin/env python3
##
from datetime import datetime, timezone
import pytz
import email.utils
import sys

tz = pytz.timezone("Asia/Tehran")
now_local = datetime.now(tz)

cmds = "\n".join(sys.argv[1:])

print(f":::{email.utils.format_datetime(now_local)}\n{cmds}")
