#!/usr/bin/env python3
"""telegram-send
Usage:
  tsend.py <receiver> <message> [--file=<file>] [ --force-document ]
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
import asyncio
from telethon import TelegramClient, events
# import logging
# logging.basicConfig(level=logging.DEBUG)

arguments = docopt(__doc__, version='telegram-send 0.1')

async def main():
    # os.chdir(os.path.dirname(os.path.realpath(sys.argv[0]))) #Changes pwd to real path, useful for using symlinks for the script.
    # This behavior was disabled because it made sending files inconvenient.
    
    with open(os.path.dirname(os.path.realpath(sys.argv[0])) + '/config') as f:
        api_id = f.readline()
        api_hash = f.readline()
        async with TelegramClient(
                    os.path.dirname(os.path.realpath(sys.argv[0])) + '/alice_is_happy',
                    api_id,
                    api_hash) as client:
                # print(arguments)
                # if arguments['--file'] is not none:
                await client.send_message(arguments['<receiver>'], arguments['<message>'], file=arguments['--file'], force_document=arguments['--force-document'])

loop = asyncio.get_event_loop()
loop.run_until_complete(main())
