#!/usr/bin/env python3

"""telegram-send
Usage:
  tsend.py [--file=<file>]... [--no-album --force-document --link-preview --parse-mode=<parser>] [--] <receiver> <message>
  tsend.py (-h | --help)
  tsend.py --version

Options:
  -f <file> --file=<file>   Sends a file, with message as its caption. (Can be specified multiple times, and sends all the files as an album. So they have to be the same kind of 'media'.)
  --force_document Whether to send the given file as a document or not.
  --link_preview Whether to show a preview of web links.
  --parse_mode <parser> Which parser to use for the message.
  -h --help     Show this screen.
  --version     Show version.

Examples:
  tsend.py some_friend "I love you ^_^" --file ~/pics/big_heart.png

Dependencies:
  pip install -U docopt telethon python-telegram-bot

Created by Fereidoon Mehri. I release my contribution to this program to the public domain (CC0).
"""
from docopt import docopt
import os
from os import getenv
import sys
from pathlib import Path
import asyncio
from IPython import embed
import re

try:
    from icecream import ic, colorize as ic_colorize

    ic.configureOutput(outputFunction=lambda s: print(ic_colorize(s)))
except ImportError:
    ic = lambda *a: None if not a else (a[0] if len(a) == 1 else a)


# import logging
# logging.basicConfig(level=logging.DEBUG)


# os.chdir(os.path.dirname(os.path.realpath(sys.argv[0]))) #Changes pwd to real path, useful for using symlinks for the script.
# This behavior was disabled because it made sending files inconvenient.
api_id = getenv("TELEGRAM_API_ID", None)
api_hash = getenv("TELEGRAM_API_HASH", None)
token = getenv("TSEND_TOKEN", None)
backend = getenv("TSEND_BACKEND", None)
#: backend 2: ptb (python-telegram-bot)
if backend is not None:
    backend = int(backend)

# ic(token, backend)
if not ((backend == 2 and token) or (api_id and api_hash and token and backend)):
    with open(str(Path.home()) + "/.telegram-config") as f:
        api_id = int(f.readline())
        api_hash = f.readline().rstrip()
        token = f.readline().rstrip()
        backend = int(f.readline())

# print(f"id: {api_id} hash: {api_hash} token: {token} backend: {backend}")


def p2int(p):
    try:
        return int(p)
    except:
        return p


async def discreet_send(
    client,
    receiver,
    message,
    file=None,
    force_document=False,
    parse_mode=None,
    reply_to=None,
    link_preview=False,
    album_mode=True,
):
    if file and len(file) > 1 and album_mode == False:
        res = None
        for f in file:
            res = await discreet_send(
                client,
                receiver,
                message,
                f,
                force_document,
                parse_mode,
                reply_to,
                link_preview,
                album_mode=True,
            )

        return res

    message = message.strip()
    last_msg = reply_to

    if file and len(file) == 1:
        file = file[0]

    if len(message) == 0:
        if file:
            last_msg = await client.send_file(
                receiver, file, reply_to=(last_msg), allow_cache=False
            )
        return last_msg
    else:
        length = len(message)
        if length <= 12000:
            s = 0
            e = 4000
            while length > s:
                last_msg = await client.send_message(
                    receiver,
                    message[s:e],
                    file=file,
                    force_document=force_document,
                    parse_mode=parse_mode,
                    link_preview=link_preview,
                    reply_to=(last_msg),
                )

                s = e
                e = s + 4000
        else:
            from brish import z

            f = z(
                """
            local f="$(gmktemp --suffix .txt)"
            ec {message} > "$f"
            ec "$f"
            """
            ).outrs
            last_msg = await client.send_file(
                receiver,
                f,
                reply_to=last_msg,
                allow_cache=False,
                caption="This message is too long, so it has been sent as a text file.",
            )
            z("command rm {f}")
            if file:
                last_msg = await client.send_file(
                    receiver, file, reply_to=(last_msg), allow_cache=False
                )
        return last_msg


async def ptb_send_file(bot, arguments, max_retries=20):
    #: @todo read parse mode from arguments
    ##
    from telegram.constants import ParseMode

    chat_id = p2int(arguments["<receiver>"])
    caption = arguments["<message>"]

    for f in arguments["--file"]:
        for attempt in range(max_retries):
            is_image = False
            try:
                file_type = os.path.splitext(f)[1].lower()  # get file extension
                is_image = file_type in [".jpg", ".jpeg", ".png"]
            except:
                pass

            try:
                with open(f, "rb") as file:
                    if is_image:
                        await bot.send_photo(
                            chat_id=chat_id,
                            photo=file,
                            caption=caption,
                            parse_mode=ParseMode.MARKDOWN,
                        )
                    else:
                        await bot.send_document(
                            chat_id=chat_id,
                            document=file,
                            caption=caption,
                            parse_mode=ParseMode.MARKDOWN,
                        )

                    break
            except Exception as e:
                print(f"Error sending {f} (attempt {attempt + 1}): {e}")
                if attempt == max_retries - 1:  # if it's the last attempt
                    print(f"Failed to send {f} after {max_retries} attempts.")
                else:
                    await asyncio.sleep(
                        1
                    )  # sleep for a while before retrying, you can adjust the time as needed


async def tsend(arguments):
    arguments["<message>"] = str(arguments["<message>"])
    if backend == 2:
        # print("backend 2 used")
        import telegram
        from telegram.ext import ApplicationBuilder

        proxy_url = os.environ.get("HTTP_PROXY")
        if proxy_url:
            app = (
                ApplicationBuilder()
                .token(token)
                .proxy_url(proxy_url)
                .get_updates_proxy_url(proxy_url)
                .build()
            )
            bot = app.bot
        else:
            bot = telegram.Bot(token)

        async with bot:
            if arguments["--file"]:
                await ptb_send_file(bot, arguments)
            else:
                await bot.send_message(
                    chat_id=p2int(arguments["<receiver>"]), text=arguments["<message>"]
                )
    else:
        from telethon import TelegramClient

        # print("telethon used")
        async with TelegramClient(
            str(Path.home()) + "/alice_is_happy", api_id, api_hash
        ) as client:

            # print(arguments)
            if arguments["--parse-mode"] == "html":
                arguments["<message>"] = re.sub(
                    r"(<(br|p)\s*/?>)", r"\1" + "\n", arguments["<message>"]
                )

            await discreet_send(
                client,
                p2int(arguments["<receiver>"]),
                arguments["<message>"],
                file=(arguments["--file"] or None),
                force_document=arguments["--force-document"],
                parse_mode=arguments["--parse-mode"],
                link_preview=arguments["--link-preview"],
                album_mode=(not arguments["--no-album"]),
            )


def parse_tsend(argv):
    return docopt(__doc__, version="telegram-send 0.1", argv=argv)


if __name__ == "__main__":
    argv = sys.argv[1:]
    arguments = parse_tsend(argv)

    # loop = asyncio.get_event_loop()
    loop = asyncio.new_event_loop()
    loop.run_until_complete(tsend(arguments))
