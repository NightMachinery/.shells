#!/usr/bin/env python3
"""telegram-send
Usage:
  tsend.py [--file=<file>] [ --force-document ] [--] <receiver> <message> 
  tsend.py (-h | --help)
  tsend.py --version

Options:
  -f <file> --file=<file>   Sends a file, with message as its caption.
  --force_document Whether to send the given file as a document or not.
  -h --help     Show this screen.
  --version     Show version.

Examples:
  tsend.py some_friend "I love you ^_^" --file ~/pics/big_heart.png

Created by Fereidoon Mehri. I release my contribution to this program to the public domain (CC0).
"""
from docopt import docopt
import os, sys
from pathlib import Path
import asyncio
from telethon import TelegramClient, events
from IPython import embed
# import logging
# logging.basicConfig(level=logging.DEBUG)

arguments = docopt(__doc__, version='telegram-send 0.1')

def p2int(p):
    try:
        return int(p)
    except:
        return p

async def main():
    # os.chdir(os.path.dirname(os.path.realpath(sys.argv[0]))) #Changes pwd to real path, useful for using symlinks for the script.
    # This behavior was disabled because it made sending files inconvenient.
    
    with open(str(Path.home()) + '/.telegram-config') as f:
        api_id = int(f.readline())
        api_hash = f.readline().rstrip()
        token = f.readline().rstrip()
        backend = int(f.readline())
        #print(f"id: {api_id} hash: {api_hash} token: {token} backend: {backend}")
        #embed()
        if backend == 2:
            #print("backend 2 used")
            import telegram
            bot = telegram.Bot(token=token)
            bot.send_message(p2int(arguments['<receiver>']), arguments['<message>'])
        else:
            #print("telethon used")
            async with TelegramClient(
                str(Path.home()) + '/alice_is_happy',
                api_id,
                api_hash) as client:
            # print(arguments)
            # if arguments['--file'] is not none:
                await client.send_message(p2int(arguments['<receiver>']), arguments['<message>'], file=arguments['--file'], force_document=arguments['--force-document'])

loop = asyncio.get_event_loop()
loop.run_until_complete(main())
