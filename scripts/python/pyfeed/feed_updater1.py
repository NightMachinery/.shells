#!/usr/bin/env python3
# `pip install -U aiohttp aioredis feedparser pypandoc`
##
import asyncio
import pynight.common_telegram as tlg
from pynight.common_feed import (
    feed_update_async,
    feedset_process_loop,
)

##
import pypandoc


def feedproc_twitter(
    item,
    tlg_p=True,
):
    twitter_base_url = 'https://nitter.cz'

    summary_d = item["summary_detail"]
    assert summary_d["type"] == "text/html"
    summary_html = summary_d["value"]
    author = item["author"]
    if author.startswith('@'):
        author_display = f"{twitter_base_url}/{author[1:]}"
    else:
        author_display = author

    # Convert HTML to markdown using pypandoc
    markdown_summary = pypandoc.convert_text(summary_html, to="markdown", format="html")

    print(f"{markdown_summary}\n--------\n")

    if tlg_p:
        tlg.send(
            chat_id="-1001988139493",
            msg=f"""{author_display}\n\n{summary_html}""",
            parse_mode="html",
        )


##

feedsets = [
    {
        "urls": [
            "https://nitter.cz/_akhaliq/rss",
        ],
        "interval": 3600,
        "processor": feedproc_twitter,
        "update_fn": feed_update_async,
    },
]


async def main():
    await asyncio.gather(*(feedset_process_loop(feedset) for feedset in feedsets))


asyncio.run(main())
