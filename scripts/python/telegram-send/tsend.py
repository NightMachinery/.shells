#!/usr/bin/env python3

"""telegram-send
Usage:
  tsend.py [--file=<file>]... [--no-album --force-document --link-preview --parse-mode=<parser>] [--lock-timeout=<seconds>] [--lock-path=<lockpath>] [--album | --no-album] [--] <receiver> <message>
  tsend.py (-h | --help)
  tsend.py --version

Options:
  -f <file> --file=<file>  Sends a file, with message as its caption. (Can be specified multiple times, and sends all the files as an album. So they have to be the same kind of 'media'.)
  --force_document  Whether to send the given file as a document or not.
  --link_preview  Whether to show a preview of web links.
  --parse_mode <parser>  Which parser to use for the message.
  --lock-timeout <seconds>  How long to wait for lock file to be released, in seconds [default: 30].
  --lock-path <lockpath>  Path to lock file.
  --album  Send files as an album. (This flag has not been implemented for the first backend!)
  -h --help  Show this screen.
  --version  Show version.

Examples:
  tsend.py some_friend "I love you ^_^" --file ~/pics/big_heart.png

Dependencies:
  pip install -U pynight IPython aiofile docopt telethon python-telegram-bot

Created by Fereidoon Mehri. I release my contribution to this program to the public domain (CC0).
"""
from docopt import docopt
from bs4 import BeautifulSoup
from urllib.parse import urlparse
from urllib.request import urlopen
import mimetypes
import tempfile
import os
from os import getenv
import sys
import asyncio
from pathlib import Path
import asyncio
from IPython import embed
import re
from pynight.common_lock_async import (
    lock_acquire,
    lock_release,
)

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


def sanitize_telegram_html(message):
    allowed_tags = ["b", "i", "u", "s", "code", "pre", "a"]

    soup = BeautifulSoup(message, "html.parser")
    saved_images = []

    # Save images to temp files
    for img_tag in soup.find_all("img"):
        img_url = img_tag["src"]

        # Determine the file extension from the URL or from the content-type
        file_extension = os.path.splitext(urlparse(img_url).path)[1]
        if not file_extension:
            response = urlopen(img_url)
            content_type = response.headers.get("content-type")
            file_extension = mimetypes.guess_extension(content_type)
            img_data = response.read()
        else:
            with urlopen(img_url) as response:
                img_data = response.read()

        with tempfile.NamedTemporaryFile(
            delete=False, suffix=file_extension
        ) as tmp_file:
            tmp_file.write(img_data)
            saved_images.append(tmp_file.name)

        img_tag.decompose()  # Remove the img tag

    # Remove all tags that are not in the allowed list
    for tag in soup.find_all(True):
        if tag.name not in allowed_tags:
            tag.unwrap()

    # Special case: Ensure 'a' tags have 'href' attribute
    for a_tag in soup.find_all("a"):
        if "href" not in a_tag.attrs:
            a_tag.unwrap()

    return dict(
        html=str(soup),
        image_files=saved_images,
    )


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
                receiver,
                file,
                reply_to=(last_msg),
                allow_cache=False,
                force_document=force_document,
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


async def ptb_send(
    bot,
    chat_id,
    parse_mode,
    message="",
    files=None,
    max_retries=20,
    verbosity=2,
    album_p=True,
    force_document=False,
):
    from telegram import InputMediaPhoto, InputMediaDocument

    async def handle(e, attempt):
        if verbosity >= 2:
            print(f"Error sending (attempt {attempt + 1}): {e}")

        if attempt == max_retries - 1:  # if it's the last attempt
            if verbosity >= 1:
                print(f"Failed after {max_retries} attempts.")
        else:
            await asyncio.sleep(10)

    # If no files are provided, just send the message with retry logic
    if not files:
        for attempt in range(max_retries):
            try:
                await bot.send_message(
                    chat_id=chat_id, text=message, parse_mode=parse_mode
                )
                break
            except Exception as e:
                await handle(e, attempt)
        return

    media_group_photos = []
    media_group_docs = []

    async def send_media_group(media_group, is_image):
        attempt = 0

        if 2 <= len(media_group) <= 10 and album_p:
            for attempt in range(max_retries):
                try:
                    await bot.send_media_group(
                        chat_id=chat_id,
                        media=media_group,
                        caption=message,
                        parse_mode=parse_mode,
                    )
                    break
                except Exception as e:
                    await handle(e)
        else:  # if album_p is False or there's only one file, send files individually
            for media in media_group:
                for attempt in range(max_retries):
                    try:
                        if not force_document and is_image:
                            await bot.send_photo(
                                chat_id=chat_id,
                                photo=media.media,
                                caption=message,
                                parse_mode=parse_mode,
                            )
                        else:
                            await bot.send_document(
                                chat_id=chat_id,
                                document=media.media,
                                caption=message,
                                parse_mode=parse_mode,
                            )
                        break
                    except Exception as e:
                        await handle(e, attempt)

        media_group.clear()

    for f in files:
        is_image = False
        try:
            file_type = os.path.splitext(f)[1].lower()  # get file extension
            is_image = file_type in [".jpg", ".jpeg", ".png"]
        except:
            pass

        with open(f, "rb") as file:
            if is_image:
                media = InputMediaPhoto(
                    media=file, caption=message, parse_mode=parse_mode
                )
                media_group_photos.append(media)
            else:
                media = InputMediaDocument(
                    media=file, caption=message, parse_mode=parse_mode
                )
                media_group_docs.append(media)

        # When media_group reaches 10 items, send them as a group
        if len(media_group_photos) == 10:
            await send_media_group(media_group_photos, is_image=True)
        if len(media_group_docs) == 10:
            await send_media_group(media_group_docs, is_image=False)

    # If media_groups contains between 1 and 9 items, send them as a group or individually
    if media_group_photos:
        await send_media_group(media_group_photos, is_image=True)
    if media_group_docs:
        await send_media_group(media_group_docs, is_image=False)


def get_parse_mode(mode_str):
    if mode_str:
        mode_str = mode_str.lower()

    from telegram.constants import ParseMode

    parse_modes = {
        # "markdown": ParseMode.MARKDOWN_V2,
        "markdown": ParseMode.MARKDOWN,
        "html": ParseMode.HTML,
        "none": None,
    }

    return parse_modes.get(mode_str, parse_modes["markdown"])


async def ptb_send_files_v1(
    bot,
    arguments,
    message,
    parse_mode=None,
    **kwargs,
):
    chat_id = p2int(arguments["<receiver>"])
    files = arguments["--file"]
    album_p = not arguments["--no-album"]

    await ptb_send(
        bot=bot,
        files=files,
        chat_id=chat_id,
        message=message,
        parse_mode=parse_mode,
        album_p=album_p,
        force_document=arguments["--force-document"],
        **kwargs,
    )


async def tsend(arguments):
    parse_mode_str = arguments.get("--parse-mode", "markdown")

    arguments["<message>"] = str(arguments["<message>"])
    message = arguments["<message>"]

    lock_timeout = float(arguments.get("--lock-timeout") or 10)
    lock_path = arguments.get("--lock-path")
    lock_name = None
    lock = None
    if lock_path:
        if not lock_path.startswith("/"):
            lock_name = lock_path
            lock_path = None

        lock = await lock_acquire(
            lock_path=lock_path,
            lock_name=lock_name,
            timeout=60,
            verbose_p=False,
            force_after_timeout_p=True,
        )
        # ic(lock.lock_path)

    try:
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
                parse_mode = get_parse_mode(parse_mode_str)

                # ic(parse_mode_str)
                if parse_mode_str == "html":
                    #: Sanitize the message to contain only HTML tags supported by Telegram
                    res = sanitize_telegram_html(message)
                    message = res["html"]
                    for img_file in res["image_files"]:
                        arguments["--file"].append(img_file)  # Add saved images to the list of files to send


                    # ic(message)

                if arguments["--file"]:
                    await ptb_send_files_v1(
                        bot,
                        arguments,
                        message=message,
                        parse_mode=parse_mode,
                    )
                else:
                    await ptb_send(
                        bot,
                        chat_id=p2int(arguments["<receiver>"]),
                        message=message,
                        parse_mode=parse_mode,
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
    finally:
        if lock:
            await lock_release(
                lock_path=lock.lock_path,
                # check_pid_p=True,
                check_pid_p=False,
                verbose_p=False,
            )


def parse_tsend(argv):
    return docopt(__doc__, version="telegram-send 0.1", argv=argv)


if __name__ == "__main__":
    argv = sys.argv[1:]
    arguments = parse_tsend(argv)

    # loop = asyncio.get_event_loop()
    loop = asyncio.new_event_loop()
    loop.run_until_complete(tsend(arguments))
