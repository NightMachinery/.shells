#!/usr/bin/env python3
import random
import pexpect

if random.random() < 0.5:
    pexpect.run("tecast.py 'https://t.me/joinchat/C6Vw2UhtHonAKvbWfDd30w' --log-chat 'Orphicality' --limit 1 --additional-receivers 'AliceNews Csmathlibrary '")
