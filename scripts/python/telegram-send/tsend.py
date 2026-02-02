#!/usr/bin/env python

"""telegram-send
Usage:
  tsend.py poll <receiver> <question> --option=<option>... [--allow-multiple] [--poll-type=<type>] [--correct-index=<index>] [--explanation=<text>] [--open-period=<seconds>] [--close-date=<timestamp>] [--close-in=<when>] [--anonymous] [--disable-notification] [-v...] [--lock-timeout=<seconds>] [--lock-path=<lockpath>]
  tsend.py [--file=<file>]... [--no-album --force-document --link-preview --parse-mode=<parser>] [-v...] [--lock-timeout=<seconds>] [--lock-path=<lockpath>] [--album | --no-album] [--] <receiver> <message>
  tsend.py (-h | --help)
  tsend.py --version

Options:
  Global:
    -v  Increase verbosity. Repeat for more detail (e.g., -vv).
    -h --help  Show this screen.
    --version  Show version.
    --lock-timeout <seconds>  How long to wait for lock file to be released, in seconds [default: 30].
    --lock-path <lockpath>  Path to lock file.

  Message command:
    -f <file> --file=<file>  Sends a file, with message as its caption. (Can be specified multiple times, and sends all the files as an album. So they have to be the same kind of 'media'.)
    --force_document  Whether to send the given file as a document or not.
    --link_preview  Whether to show a preview of web links.
    --parse_mode <parser>  Which parser to use for the message.
    --album  Send files as an album. (This flag has not been implemented for the first backend!)
    --no-album  Do not send files as an album.

  Poll command:
    --option <option>  Adds an option to the poll. Use multiple times for more options. (poll command)
    -m --allow-multiple  Allow voters to pick more than one option. (poll command)
    --poll-type <type>  Poll type, either "regular" or "quiz". [default: regular]
    --correct-index <index>  Zero-based index of the correct option for quiz polls.
    --explanation <text>  Explanation shown after answering a quiz poll.
    --open-period <seconds>  Auto-close the poll after this many seconds (5-600).
    --close-date <timestamp>  Unix timestamp when the poll should close. Prefix with @ for UTC epoch or + for relative seconds.
    --close-in <when>  Human-readable relative time (local timezone), e.g. "15m", "2h", "tomorrow 9am".
    --anonymous  Send the poll anonymously (default is public voters).
    --disable-notification  Send poll without a push notification.

Examples:
  tsend.py some_friend "I love you ^_^" --file ~/pics/big_heart.png

Dependencies:
  pip install -U pynight IPython aiofile docopt PySocks telethon python-telegram-bot dateparser

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
import traceback
from pathlib import Path
import asyncio
from IPython import embed
import re
from datetime import datetime, timezone
import dateparser
from pynight.common_proxy import pysocks_proxy_from_env
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


def _local_now():
    return datetime.now().astimezone()


def _close_date_dt_from_ts(close_date_ts):
    return datetime.fromtimestamp(close_date_ts, tz=timezone.utc)


def _parse_close_date_raw(close_date_raw):
    raw = str(close_date_raw or "").strip()
    if not raw:
        raise SystemExit("--close-date cannot be empty.")

    if raw.startswith("@"):
        ts_raw = raw[1:].strip()
        if not ts_raw.isdigit():
            raise SystemExit("--close-date @<epoch> must be an integer.")
        return int(ts_raw)

    if raw.startswith("+"):
        seconds_raw = raw[1:].strip()
        if not seconds_raw.isdigit():
            raise SystemExit("--close-date +<seconds> must be an integer.")
        return int(_local_now().timestamp()) + int(seconds_raw)

    try:
        return int(raw)
    except ValueError:
        raise SystemExit(
            "--close-date must be a Unix timestamp (integer), @<epoch>, or +<seconds>."
        )


def _parse_close_in_raw(close_in_raw):
    raw = str(close_in_raw or "").strip()
    if not raw:
        raise SystemExit("--close-in cannot be empty.")

    base = _local_now()
    if raw.isdigit():
        return int(base.timestamp()) + int(raw)

    candidate = raw
    if re.fullmatch(
        r"\d+\s*[smhdw]",
        raw,
        flags=re.IGNORECASE,
    ) or re.fullmatch(
        r"\d+\s*(seconds|minutes|hours|days|weeks)",
        raw,
        flags=re.IGNORECASE,
    ):
        candidate = f"in {raw}"

    settings = dict(
        RELATIVE_BASE=base,
        PREFER_DATES_FROM="future",
        RETURN_AS_TIMEZONE_AWARE=True,
    )
    parsed = dateparser.parse(candidate, settings=settings)
    if parsed is None and candidate != raw:
        parsed = dateparser.parse(raw, settings=settings)
    if parsed is None:
        raise SystemExit(
            "--close-in could not be parsed. Examples: 15m, 2h, tomorrow 9am."
        )

    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=base.tzinfo)

    return int(parsed.timestamp())


def _parse_verbosity(arguments):
    raw = arguments.get("-v")
    if isinstance(raw, bool):
        count = 1 if raw else 0
    elif isinstance(raw, int):
        count = raw
    elif isinstance(raw, (list, tuple)):
        count = len(raw)
    else:
        count = 0
    return 1 + count


def parse_poll_arguments(arguments):
    question = (arguments.get("<question>") or "").strip()
    if not question:
        raise SystemExit("Poll question cannot be empty.")

    options = [opt.strip() for opt in (arguments.get("--option") or []) if opt.strip()]
    if len(options) < 2:
        raise SystemExit("Polls require at least two non-empty options.")

    poll_type = (arguments.get("--poll-type") or "regular").strip().lower()
    if poll_type not in {"regular", "quiz"}:
        raise SystemExit("Poll type must be either 'regular' or 'quiz'.")

    allow_multiple = bool(arguments.get("--allow-multiple"))
    if poll_type == "quiz" and allow_multiple:
        raise SystemExit("Quiz polls cannot allow multiple answers.")

    correct_index_raw = arguments.get("--correct-index")
    correct_index = None
    if correct_index_raw is not None:
        try:
            correct_index = int(correct_index_raw)
        except ValueError:
            raise SystemExit("--correct-index must be an integer.")

    if poll_type == "quiz":
        if correct_index is None:
            raise SystemExit("Quiz polls require --correct-index.")
        if not 0 <= correct_index < len(options):
            raise SystemExit("--correct-index must reference an existing option.")
    elif correct_index is not None:
        raise SystemExit("--correct-index is only valid for quiz polls.")

    explanation = arguments.get("--explanation")
    if explanation and poll_type != "quiz":
        raise SystemExit("--explanation can only be used with quiz polls.")

    open_period_raw = arguments.get("--open-period")
    open_period = None
    if open_period_raw is not None:
        try:
            open_period = int(open_period_raw)
        except ValueError:
            raise SystemExit("--open-period must be an integer.")
        if not 5 <= open_period <= 600:
            raise SystemExit("--open-period must be between 5 and 600 seconds.")

    close_date_raw = arguments.get("--close-date")
    close_in_raw = arguments.get("--close-in")
    close_date_ts = None
    close_date_dt = None

    if open_period is not None and (close_date_raw is not None or close_in_raw is not None):
        raise SystemExit("Use only one of --open-period, --close-date, or --close-in.")
    if close_date_raw is not None and close_in_raw is not None:
        raise SystemExit("Use only one of --open-period, --close-date, or --close-in.")

    if close_date_raw is not None:
        close_date_ts = _parse_close_date_raw(close_date_raw)
        close_date_dt = _close_date_dt_from_ts(close_date_ts)

    if close_in_raw is not None:
        close_date_ts = _parse_close_in_raw(close_in_raw)
        close_date_dt = _close_date_dt_from_ts(close_date_ts)

    poll_data = dict(
        chat_id=p2int(arguments.get("<receiver>")),
        question=question,
        options=options,
        poll_type=poll_type,
        allow_multiple=allow_multiple,
        correct_index=correct_index,
        explanation=explanation,
        open_period=open_period,
        close_date_ts=close_date_ts,
        close_date_dt=close_date_dt,
        is_anonymous=bool(arguments.get("--anonymous")),
        disable_notification=bool(arguments.get("--disable-notification")),
    )

    return poll_data


async def handle(e, attempt, max_retries, verbosity):
    if verbosity >= 2:
        print(f"Error sending (attempt {attempt + 1}): {e}")

    if attempt == max_retries - 1:  # if it's the last attempt
        if verbosity >= 1:
            print(f"Failed after {max_retries} attempts.")
    else:
        await asyncio.sleep(10)


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
    max_retries=30,
    verbosity=1,
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
            for attempt in range(max_retries):
                try:
                    last_msg = await client.send_file(
                        receiver,
                        file,
                        reply_to=(last_msg),
                        allow_cache=False,
                        force_document=force_document,
                    )
                    break
                except Exception as e:
                    traceback.print_exc()
                    print(f"Error while sending file (attempt {attempt + 1}/{max_retries})", file=sys.stderr)
                    await handle(e, attempt, max_retries, verbosity)

        return last_msg
    else:
        length = len(message)
        if length <= 12000:
            s = 0
            e = 4000
            while length > s:
                for attempt in range(max_retries):
                    try:
                        last_msg = await client.send_message(
                            receiver,
                            message[s:e],
                            file=file,
                            force_document=force_document,
                            parse_mode=parse_mode,
                            link_preview=link_preview,
                            reply_to=(last_msg),
                        )
                        break

                    except Exception as err:
                        await handle(err, attempt, max_retries, verbosity)

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

    # If no files are provided, just send the message with retry logic
    if not files:
        for attempt in range(max_retries):
            try:
                await bot.send_message(
                    chat_id=chat_id, text=message, parse_mode=parse_mode
                )
                break
            except Exception as e:
                await handle(e, attempt, max_retries, verbosity)
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
                    await handle(e, attempt, max_retries, verbosity)
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
                        await handle(e, attempt, max_retries, verbosity)

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


def ptb_get_parse_mode(mode_str):
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


async def ptb_send_poll(bot, poll_arguments, max_retries=20, verbosity=2):
    for attempt in range(max_retries):
        try:
            await bot.send_poll(
                chat_id=poll_arguments["chat_id"],
                question=poll_arguments["question"],
                options=poll_arguments["options"],
                is_anonymous=poll_arguments["is_anonymous"],
                type=poll_arguments["poll_type"],
                allows_multiple_answers=poll_arguments["allow_multiple"],
                correct_option_id=poll_arguments["correct_index"],
                explanation=poll_arguments["explanation"],
                open_period=poll_arguments["open_period"],
                close_date=poll_arguments["close_date_ts"],
                disable_notification=poll_arguments["disable_notification"],
            )
            break
        except Exception as e:
            await handle(e, attempt, max_retries, verbosity)


async def telethon_send_poll(client, poll_arguments, max_retries=30, verbosity=1):
    from telethon.tl import types

    def _twe(text):
        return types.TextWithEntities(text=str(text), entities=[])

    answers = []
    for idx, option_text in enumerate(poll_arguments["options"]):
        option_bytes = idx.to_bytes(2, byteorder="big")
        answers.append(types.PollAnswer(text=_twe(option_text), option=option_bytes))

    poll = types.Poll(
        id=0,
        question=_twe(poll_arguments["question"]),
        answers=answers,
        public_voters=not poll_arguments["is_anonymous"],
        multiple_choice=poll_arguments["allow_multiple"],
        quiz=(poll_arguments["poll_type"] == "quiz"),
        close_period=poll_arguments["open_period"],
        close_date=poll_arguments["close_date_dt"],
    )

    correct_answers = None
    if poll_arguments["poll_type"] == "quiz":
        correct_answers = [
            answers[poll_arguments["correct_index"]].option,
        ]

    input_media = types.InputMediaPoll(
        poll=poll,
        correct_answers=correct_answers,
        solution=poll_arguments["explanation"],
    )

    for attempt in range(max_retries):
        try:
            await client.send_message(
                poll_arguments["chat_id"],
                message=None,
                file=input_media,
                silent=poll_arguments["disable_notification"],
            )
            break
        except Exception as e:
            await handle(e, attempt, max_retries, verbosity)


async def tsend(arguments):
    poll_mode = bool(arguments.get("poll"))
    poll_arguments = parse_poll_arguments(arguments) if poll_mode else None
    verbosity = _parse_verbosity(arguments)

    parse_mode_str = arguments.get("--parse-mode", "markdown")
    message = None
    if not poll_mode:
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
                    .proxy(proxy_url)
                    .get_updates_proxy(proxy_url)
                    .build()
                )
                #: PTBDeprecationWarning: Deprecated since version 20.7: `ApplicationBuilder.proxy_url` is deprecated. Use `ApplicationBuilder.proxy` instead.

                bot = app.bot
            else:
                bot = telegram.Bot(token)

            async with bot:
                if poll_mode:
                    await ptb_send_poll(bot, poll_arguments, verbosity=verbosity)
                else:
                    parse_mode = ptb_get_parse_mode(parse_mode_str)

                    # ic(parse_mode_str)
                    if parse_mode_str == "html":
                        #: Sanitize the message to contain only HTML tags supported by Telegram
                        res = sanitize_telegram_html(message)
                        message = res["html"]
                        for img_file in res["image_files"]:
                            arguments["--file"].append(
                                img_file
                            )  # Add saved images to the list of files to send

                        # ic(message)

                    if arguments["--file"]:
                        await ptb_send_files_v1(
                            bot,
                            arguments,
                            message=message,
                            parse_mode=parse_mode,
                            verbosity=verbosity,
                        )
                    else:
                        await ptb_send(
                            bot,
                            chat_id=p2int(arguments["<receiver>"]),
                            message=message,
                            parse_mode=parse_mode,
                            verbosity=verbosity,
                        )
        else:  #: Telethon backend
            from telethon import TelegramClient

            # print("Telethon used")
            proxy = pysocks_proxy_from_env()

            client_params = dict(
                api_hash=api_hash,
                api_id=api_id,
                proxy=proxy,
            )
            client_params["session"] = str(Path.home()) + "/alice_is_happy"
            client = TelegramClient(**client_params)

            if token:
                # ic(token)
                await client.start(bot_token=token)

            else:
                await client.start()

            try:
                if poll_mode:
                    await telethon_send_poll(client, poll_arguments, verbosity=verbosity)
                else:
                    # print(arguments)
                    if parse_mode_str == "html":
                        arguments["<message>"] = re.sub(
                            r"(<(br|p)\s*/?>)", r"\1" + "\n", arguments["<message>"]
                        )

                    elif parse_mode_str == "none":
                        parse_mode_str = None
                        #: Disabling default formatting
                        #: [[https://docs.telethon.dev/en/stable/modules/client.html#telethon.client.messageparse.MessageParseMethods.parse_mode][TelegramClient — Telethon 1.36.0 documentation]]

                    await discreet_send(
                        client,
                        p2int(arguments["<receiver>"]),
                        arguments["<message>"],
                        file=(arguments["--file"] or None),
                        force_document=arguments["--force-document"],
                        parse_mode=parse_mode_str,
                        link_preview=arguments["--link-preview"],
                        album_mode=(not arguments["--no-album"]),
                        verbosity=verbosity,
                    )

            finally:
                await client.disconnect()

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
