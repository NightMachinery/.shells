#!/usr/bin/env python3
"""tecast; A simple Telegram broadcaster.

tecast gets a configurable number of the latest messsages in a chat (supergroup or channel) and sends them to the entities specified in the chat's description, which should be separated by whitespace. (We use Python's split method with no arguments.) tecast deletes the messages it broadcasts, so preferably also specify an archive location in the receivers.

Usage:
  tecast.py <input-chat> [--log-chat=<lc> --limit=<lim> --additional-receivers=<ar>]
  tecast.py (-h | --help)
  tecast.py --version

Options:
  -l <lc> --log-chat=<lc> The place where log messages are sent. [default: me]
  -n <lim> --limit=<lim> The number of messages to broadcast. [defualt: 1]
  -a <ar> --additional-receivers=<ar> Additional receivers; This string just gets concatenated to the start of the chat's description, so end it with whitespace. [defualt: ]
  -h --help     Show this screen.
  --version     Show version.

Examples:
   tecast.py 'https://t.me/joinchat/someHash' --limit 5
   tecast.py 'https://t.me/joinchat/someHash' --log-chat 'someUser' --additional-receivers 'Newlibrary https://t.me/joinchat/someHash anotherUser '

Created by Fereidoon Mehri. Feel free to use the code with proper attribution."""
from docopt import docopt
from telethon import TelegramClient, sync, functions
import traceback
import IPython
from pathlib import Path

arguments = docopt(__doc__, version='tecast 0.1')
with open(str(Path.home()) + '/.telegram-config') as f:
    api_id = f.readline()
    api_hash = f.readline()
    with TelegramClient(
            str(Path.home()) + '/alice_is_happy', api_id, api_hash) as client:
        bw = client.get_entity(arguments['<input-chat>'])
        bwf = client(functions.channels.GetFullChannelRequest(bw)).full_chat
        limit = int(arguments['--limit'])
        log_chat = arguments['--log-chat']
        additional_receivers = arguments['--additional-receivers']
        all_recs = additional_receivers + bwf.about
        for msg in client.get_messages(bw, limit=limit):
            for rec in all_recs.split():
                try:
                    client.send_message(rec, msg)
                except TypeError:
                    pass
                except Exception as e:  #ValueError as err:
                    er_msg = traceback.format_exc()
                    print(er_msg)
                    client.send_message(log_chat, er_msg)
            msg.delete()
