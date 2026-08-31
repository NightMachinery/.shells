#!/usr/bin/env python

"""telegram-send
Usage:
  tsend.py poll [--] <receiver> <question> [--option=<option>]... [--option-json=<json>]... [--options-parse-mode=<mode>] [--allow-multiple] [--allow-adding-options | --no-adding-options] [--poll-type=<type>] [--correct-index=<index>] [--explanation=<text>] [--open-period=<seconds>] [--close-date=<timestamp>] [--close-in=<when>] [--anonymous] [--disable-notification] [-v...] [--lock-timeout=<seconds>] [--lock-path=<lockpath>]
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
    --option <option>  Adds an option to the poll. Use multiple times for more options. Markdown links are parsed by default. (poll command)
    --option-json <json>  Adds a rich Bot API InputPollOption JSON object. Use multiple times for more options. (poll command)
    --options-parse-mode <mode>  How to parse --option values: "markdown" or "plain". [default: markdown]
    -m --allow-multiple  Allow voters to pick more than one option. (poll command)
    --allow-adding-options  Allow users to add answer options after poll creation; off by default. (poll command)
    --no-adding-options  Explicitly keep user-added answer options disabled. (poll command)
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
  tsend.py poll --option '5 PM' --option '6 PM' -- some_friend "When should we play?"
  tsend.py poll --allow-adding-options --option '5 PM' --option '6 PM' -- some_friend "When should we play?"
  tsend.py poll --option 'hello [world](https://example.com)' --option 'plain option' -- some_friend "Pick one"

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
import json
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

#: Telethon session file; only used by backend 1, as the Bot API backend is
#: stateless. See [agfi:tsend-main] for injecting a different account's session.
session_path = getenv("TELEGRAM_SESSION", None)

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


def normalize_destination(destination):
    destination = str(destination or "").strip()
    if not destination:
        raise SystemExit("Destination cannot be empty.")
    return destination


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


POLL_QUESTION_MAX_CHARS = 300
POLL_OPTION_MIN_COUNT = 2
# Telegram Bot API currently allows up to 12 poll options.  python-telegram-bot
# 20.8 still reports Poll.MAX_OPTION_NUMBER == 10, so keep the current Bot API
# limit here until the local dependency catches up.
POLL_OPTION_MAX_COUNT = 12
POLL_OPTION_MAX_CHARS = 100


_MARKDOWN_LINK_RE = re.compile(r"\[([^\]\n]+)\]\((https?://[^\s)]+)\)")


def _utf16_len(text):
    return len(str(text).encode("utf-16-le")) // 2


def _poll_option_text(option):
    return str(option.get("text") or "")


def _parse_markdown_poll_option(raw):
    raw = str(raw)
    media = None

    def replace(match):
        nonlocal media
        label = match.group(1)
        url = match.group(2)
        if media is None:
            media = dict(type="link", url=url, text_url_entity=True)
        return label

    text = _MARKDOWN_LINK_RE.sub(replace, raw).strip()
    option = dict(text=text)
    if media:
        option["media"] = media
    return option


def _normalize_poll_option_json(raw):
    try:
        option = json.loads(raw)
    except json.JSONDecodeError as e:
        raise SystemExit(f"--option-json is not valid JSON: {e}")

    if not isinstance(option, dict):
        raise SystemExit("--option-json must be a JSON object.")

    option = dict(option)
    option["text"] = str(option.get("text") or "").strip()
    media = option.get("media")
    if media is not None and not isinstance(media, dict):
        raise SystemExit("--option-json media must be a JSON object when supplied.")
    return option


def _parse_poll_options(arguments):
    parse_mode = (arguments.get("--options-parse-mode") or "markdown").strip().lower()
    if parse_mode not in {"markdown", "plain"}:
        raise SystemExit('--options-parse-mode must be either "markdown" or "plain".')

    options = []
    for raw in arguments.get("--option") or []:
        raw = str(raw).strip()
        if not raw:
            continue
        if parse_mode == "markdown":
            options.append(_parse_markdown_poll_option(raw))
        else:
            options.append(dict(text=raw))

    for raw in arguments.get("--option-json") or []:
        options.append(_normalize_poll_option_json(raw))

    return options, parse_mode


def _warn_addable_poll_full(options):
    if len(options) == POLL_OPTION_MAX_COUNT:
        print(
            "Warning: --allow-adding-options is enabled, but this poll already "
            f"has {POLL_OPTION_MAX_COUNT}/{POLL_OPTION_MAX_COUNT} options; "
            "users cannot add more options unless you start with fewer options.",
            file=sys.stderr,
        )


def _validate_poll_options(question, options):
    question_len = len(question)
    if question_len > POLL_QUESTION_MAX_CHARS:
        raise SystemExit(
            f"Poll question is too long: {question_len}/{POLL_QUESTION_MAX_CHARS} characters."
        )

    if len(options) < POLL_OPTION_MIN_COUNT:
        raise SystemExit("Polls require at least two non-empty options.")
    if len(options) > POLL_OPTION_MAX_COUNT:
        raise SystemExit(
            f"Polls support at most {POLL_OPTION_MAX_COUNT} options; got {len(options)}."
        )

    for idx, option in enumerate(options):
        text = _poll_option_text(option)
        text_len = len(text)
        if text_len == 0:
            raise SystemExit(f"Poll option {idx} cannot be empty after parsing.")
        if text_len > POLL_OPTION_MAX_CHARS:
            raise SystemExit(
                f"Poll option {idx} is too long after parsing: "
                f"{text_len}/{POLL_OPTION_MAX_CHARS} characters: {text!r}"
            )


def _bot_api_poll_options(options):
    if any("media" in option or set(option.keys()) != {"text"} for option in options):
        return options
    return [_poll_option_text(option) for option in options]


def _poll_options_have_media(options):
    return any(option.get("media") for option in options)


def _poll_options_need_rich_bot_api(options):
    return any(option.get("media") for option in options)


def _telethon_input_media_from_bot_media(media, types):
    media_type = str(media.get("type") or "").strip().lower()

    if media_type == "link":
        url = str(media.get("url") or "").strip()
        if not url:
            raise SystemExit("Poll option link media requires a non-empty url.")
        return types.InputMediaWebPage(url=url)

    if media_type == "location":
        try:
            lat = float(media["latitude"])
            lon = float(media["longitude"])
        except (KeyError, TypeError, ValueError):
            raise SystemExit("Poll option location media requires latitude and longitude.")
        return types.InputMediaGeoPoint(
            geo_point=types.InputGeoPoint(lat=lat, long=lon)
        )

    if media_type == "venue":
        try:
            lat = float(media["latitude"])
            lon = float(media["longitude"])
        except (KeyError, TypeError, ValueError):
            raise SystemExit("Poll option venue media requires latitude and longitude.")
        return types.InputMediaVenue(
            geo_point=types.InputGeoPoint(lat=lat, long=lon),
            title=str(media.get("title") or ""),
            address=str(media.get("address") or ""),
            provider="",
            venue_id=str(media.get("foursquare_id") or media.get("google_place_id") or ""),
            venue_type=str(media.get("foursquare_type") or media.get("google_place_type") or ""),
        )

    media_value = str(media.get("media") or "").strip()
    if media_type == "photo" and media_value.startswith(("http://", "https://")):
        return types.InputMediaPhotoExternal(url=media_value)

    if media_type in {"video", "animation"} and media_value.startswith(("http://", "https://")):
        attrs = []
        if media_type == "animation":
            attrs.append(types.DocumentAttributeAnimated())
        else:
            attrs.append(
                types.DocumentAttributeVideo(
                    duration=float(media.get("duration") or 0),
                    w=int(media.get("width") or 0),
                    h=int(media.get("height") or 0),
                    supports_streaming=True,
                )
            )
        return types.InputMediaDocumentExternal(
            url=media_value,
            video_cover=None,
            video_timestamp=media.get("start_timestamp"),
        )

    if media_type in {"photo", "video", "animation", "sticker", "live_photo"}:
        raise SystemExit(
            f"Telethon poll option media type {media_type!r} currently supports HTTP URLs "
            "for photo/video/animation only; use TSEND_BACKEND=2 for full Bot API rich poll options."
        )

    raise SystemExit(f"Unsupported poll option media type: {media_type!r}")


async def _telethon_message_media_from_bot_media(client, peer, media, types, functions):
    media_type = str(media.get("type") or "").strip().lower()
    if media_type == "link":
        url = str(media.get("url") or "").strip()
        if not url:
            raise SystemExit("Poll option link media requires a non-empty url.")
        return types.MessageMediaWebPage(
            webpage=types.WebPagePending(id=0, date=None, url=url)
        )

    input_media = _telethon_input_media_from_bot_media(media, types)
    uploaded = await client(functions.messages.UploadMediaRequest(peer=peer, media=input_media))
    return uploaded


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

    options, options_parse_mode = _parse_poll_options(arguments)
    _validate_poll_options(question, options)

    poll_type = (arguments.get("--poll-type") or "regular").strip().lower()
    if poll_type not in {"regular", "quiz"}:
        raise SystemExit("Poll type must be either 'regular' or 'quiz'.")

    allow_multiple = bool(arguments.get("--allow-multiple"))
    if poll_type == "quiz" and allow_multiple:
        raise SystemExit("Quiz polls cannot allow multiple answers.")

    allow_adding_options = bool(arguments.get("--allow-adding-options"))
    if allow_adding_options and poll_type == "quiz":
        raise SystemExit("--allow-adding-options is not supported for quiz polls.")
    if allow_adding_options:
        _warn_addable_poll_full(options)

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

    is_anonymous = bool(arguments.get("--anonymous"))
    if allow_adding_options and is_anonymous:
        raise SystemExit("--allow-adding-options is not supported for anonymous polls.")

    poll_data = dict(
        chat_id=p2int(normalize_destination(arguments.get("<receiver>"))),
        question=question,
        options=options,
        bot_api_options=_bot_api_poll_options(options),
        options_parse_mode=options_parse_mode,
        poll_type=poll_type,
        allow_multiple=allow_multiple,
        correct_index=correct_index,
        explanation=explanation,
        open_period=open_period,
        close_date_ts=close_date_ts,
        close_date_dt=close_date_dt,
        is_anonymous=is_anonymous,
        allow_adding_options=allow_adding_options,
        disable_notification=bool(arguments.get("--disable-notification")),
    )

    return poll_data


class SendFailed(Exception):
    """Raised when a send has definitively failed, so the caller can exit non-zero."""


def is_permanent_error(e):
    """True for errors that no amount of retrying will fix.

    Retrying these used to burn max_retries * sleep seconds in complete silence
    before giving up (and then still exiting 0)."""
    if isinstance(e, ValueError):
        #: Telethon raises a bare ValueError ("Could not find the input entity for
        #: ...") when a peer cannot be resolved at all. Retrying never helps; the
        #: caller tries a cache refresh once first, and gives up if that fails too.
        return True

    return type(e).__name__ in (
        "ChannelInvalidError",
        "ChannelPrivateError",
        "PeerIdInvalidError",
        "ChatWriteForbiddenError",
        "UserIsBlockedError",
        "InputUserDeactivatedError",
    )


async def refresh_entity(client, receiver, verbosity=1):
    """Re-resolve `receiver` against the server and refresh the session's entity cache.

    Telethon caches an access_hash per entity in the session database, and access
    hashes are per-account. A hash left over from a different account (or an old
    login) makes the server answer CHANNEL_INVALID forever, even though the peer is
    perfectly reachable. Asking for the entity re-resolves it and writes the correct
    hash back."""
    try:
        entity = await client.get_entity(receiver)
        if verbosity >= 1:
            print(
                f"Refreshed stale entity cache for {receiver}.", file=sys.stderr
            )
        return entity
    except Exception as e:
        if verbosity >= 1:
            print(f"Could not re-resolve {receiver}: {e}", file=sys.stderr)
        return None


async def handle(e, attempt, max_retries, verbosity):
    #: Errors are surfaced at the default verbosity. They used to be visible only at
    #: `-vv`, which turned a permanent failure into a silent multi-minute stall.
    print(f"Error sending (attempt {attempt + 1}/{max_retries}): {e}", file=sys.stderr)
    if verbosity >= 2:
        traceback.print_exc()

    if attempt == max_retries - 1:  # if it's the last attempt
        print(f"Failed after {max_retries} attempts.", file=sys.stderr)
    else:
        #: Exponential backoff, capped. A flat 10s * 30 attempts meant a five-minute
        #: hang for a caller that just wanted to post a notification.
        await asyncio.sleep(min(2 ** attempt, 30))


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
    max_retries=5,
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
            sent = False
            refreshed = False
            for attempt in range(max_retries):
                try:
                    last_msg = await client.send_file(
                        receiver,
                        file,
                        reply_to=(last_msg),
                        allow_cache=False,
                        force_document=force_document,
                    )
                    sent = True
                    break
                except Exception as e:
                    if is_permanent_error(e) and not refreshed:
                        #: Most likely a stale access_hash; re-resolve once and retry.
                        refreshed = True
                        entity = await refresh_entity(client, receiver, verbosity)
                        if entity is not None:
                            receiver = entity
                            continue

                    if is_permanent_error(e):
                        raise SendFailed(
                            f"Cannot send file to {receiver}: {type(e).__name__}: {e}"
                        ) from e

                    await handle(e, attempt, max_retries, verbosity)

            if not sent:
                raise SendFailed(
                    f"Failed to send file to {receiver} after {max_retries} attempts."
                )

        return last_msg
    else:
        length = len(message)
        if length <= 12000:
            s = 0
            e = 4000
            refreshed = False
            while length > s:
                sent = False
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
                        sent = True
                        break

                    except Exception as err:
                        if is_permanent_error(err) and not refreshed:
                            #: Most likely a stale access_hash in the session's entity
                            #: cache; re-resolve once and retry before giving up.
                            refreshed = True
                            entity = await refresh_entity(client, receiver, verbosity)
                            if entity is not None:
                                receiver = entity
                                continue

                        if is_permanent_error(err):
                            raise SendFailed(
                                f"Cannot send to {receiver}: {type(err).__name__}: {err}"
                            ) from err

                        await handle(err, attempt, max_retries, verbosity)

                if not sent:
                    #: Previously this fell through silently and the loop just advanced
                    #: to the next chunk, so tsend exited 0 having sent nothing.
                    raise SendFailed(
                        f"Failed to send to {receiver} after {max_retries} attempts."
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
    api_kwargs = {}
    if poll_arguments["allow_adding_options"]:
        api_kwargs["allow_adding_options"] = True

    for attempt in range(max_retries):
        try:
            await bot.send_poll(
                chat_id=poll_arguments["chat_id"],
                question=poll_arguments["question"],
                options=poll_arguments["bot_api_options"],
                is_anonymous=poll_arguments["is_anonymous"],
                type=poll_arguments["poll_type"],
                allows_multiple_answers=poll_arguments["allow_multiple"],
                correct_option_id=poll_arguments["correct_index"],
                explanation=poll_arguments["explanation"],
                open_period=poll_arguments["open_period"],
                close_date=poll_arguments["close_date_ts"],
                disable_notification=poll_arguments["disable_notification"],
                api_kwargs=api_kwargs or None,
            )
            break
        except Exception as e:
            await handle(e, attempt, max_retries, verbosity)


async def telethon_send_poll(client, poll_arguments, max_retries=30, verbosity=1):
    from telethon.tl import functions, types

    def _twe(text):
        return types.TextWithEntities(text=str(text), entities=[])

    answers = []
    for idx, option in enumerate(poll_arguments["options"]):
        option_bytes = idx.to_bytes(2, byteorder="big")
        media = option.get("media")
        text = _poll_option_text(option)
        text_entities = []
        answer_kwargs = {}
        if media:
            media_type = str(media.get("type") or "").strip().lower()
            if media_type == "link" and media.get("text_url_entity"):
                text_entities.append(
                    types.MessageEntityTextUrl(
                        offset=0,
                        length=_utf16_len(text),
                        url=str(media.get("url") or ""),
                    )
                )
            else:
                answer_kwargs["media"] = await _telethon_message_media_from_bot_media(
                    client,
                    poll_arguments["chat_id"],
                    media,
                    types,
                    functions,
                )
        answers.append(
            types.PollAnswer(
                text=types.TextWithEntities(text=str(text), entities=text_entities),
                option=option_bytes,
                **answer_kwargs,
            )
        )

    poll = types.Poll(
        id=0,
        question=_twe(poll_arguments["question"]),
        answers=answers,
        hash=0,
        public_voters=not poll_arguments["is_anonymous"],
        multiple_choice=poll_arguments["allow_multiple"],
        quiz=(poll_arguments["poll_type"] == "quiz"),
        open_answers=poll_arguments["allow_adding_options"] or None,
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


def _bot_api_chat_id_from_telethon_entity(entity):
    from telethon.tl import types

    if isinstance(entity, types.User):
        return entity.id
    if isinstance(entity, types.Chat):
        return -entity.id
    if isinstance(entity, types.Channel):
        return int(f"-100{entity.id}")
    entity_id = getattr(entity, "id", None)
    if entity_id is None:
        raise SystemExit(f"Could not convert Telegram entity to Bot API chat_id: {entity!r}")
    return entity_id


async def _ptb_bot_from_env():
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
        return app.bot
    return telegram.Bot(token)


async def ptb_send_poll_from_telethon_resolution(client, poll_arguments, receiver, verbosity):
    if not token:
        raise SystemExit(
            "Rich poll option media requires the Bot API backend, but TSEND_TOKEN/TELEGRAM_TOKEN is not set."
        )

    entity = await client.get_entity(receiver)
    bot_api_chat_id = _bot_api_chat_id_from_telethon_entity(entity)
    poll_arguments = dict(poll_arguments)
    poll_arguments["chat_id"] = bot_api_chat_id

    bot = await _ptb_bot_from_env()
    async with bot:
        await ptb_send_poll(bot, poll_arguments, verbosity=verbosity)


async def tsend(arguments):
    poll_mode = bool(arguments.get("poll"))
    poll_arguments = parse_poll_arguments(arguments) if poll_mode else None
    verbosity = _parse_verbosity(arguments)

    arguments["<receiver>"] = normalize_destination(arguments.get("<receiver>"))

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
            #: Telethon appends `.session` when it is missing, so both forms of
            #: the path work. `expanduser` matters because callers pass
            #: shell-style paths that zsh never expanded.
            client_params["session"] = os.path.expanduser(
                session_path or "~/alice_is_happy"
            )
            client = TelegramClient(**client_params)

            await client.connect()
            if await client.get_me() is None:
                #: Only log in when the session is not already authorized. This
                #: mirrors Telethon's own `start` (client/auth.py), minus its
                #: warning when an authorized user session is handed a bot
                #: token -- which we hit on every call, since TSEND_TOKEN is set
                #: globally while `tsend-main` uses a user session.
                if token:
                    # ic(token)
                    await client.start(bot_token=token)

                else:
                    await client.start()

            try:
                if poll_mode:
                    me = await client.get_me()
                    if _poll_options_need_rich_bot_api(poll_arguments["options"]):
                        if verbosity >= 2:
                            print(
                                "Rich poll option media detected; using Bot API sendPoll "
                                f"after Telethon peer resolution (telethon_user_is_bot={bool(getattr(me, 'bot', False))})."
                            )
                        await ptb_send_poll_from_telethon_resolution(
                            client,
                            poll_arguments,
                            arguments["<receiver>"],
                            verbosity,
                        )
                    else:
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
                #: Telethon returns None from disconnect() when it is already
                #: disconnected, and `await None` raises TypeError -- which used to
                #: bury the real error under a confusing traceback.
                disconnected = client.disconnect()
                if disconnected is not None:
                    await disconnected

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
    try:
        loop.run_until_complete(tsend(arguments))
    except SendFailed as e:
        #: Exit non-zero so callers can actually detect a failed send. This used to
        #: exit 0 no matter what, which is why a broken destination went unnoticed.
        print(f"tsend: {e}", file=sys.stderr)
        sys.exit(1)
