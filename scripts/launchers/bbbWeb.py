#!/usr/bin/env python3
import random
import pexpect

if random.random() < 0.6:
    print(pexpect.run("tecast.py 'https://t.me/joinchat/C6Vw2UhtHonAKvbWfDd30w' --log-chat 'Orphicality' --limit 1 --additional-receivers 'AliceNews math_sut_refs meiotumultuado marinavondrich Dr_Raeesi murakamitype +966596247086 N_Dhghn  '").decode())
else:
    print("Not this time ;-'")
