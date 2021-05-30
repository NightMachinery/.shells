#!/usr/bin/env python3
# Usage:
# `reddit_sub_posts_urls_pushshift.py rational 100000000 > rational.txt`
# `tmuxnewsh2 reddit reddit_sub_posts_urls_pushshift.py rational 100000000`
#
# Docs:
# - https://github.com/praw-dev/praw
# - https://github.com/dmarx/psaw
##
from IPython import embed

import sys, os

link_output = False
limit = 10
subreddit = "rational"

al = len(sys.argv)
if al >= 2:
    subreddit = sys.argv[1]
if al >= 3:
    limit = int(sys.argv[2])

from psaw import PushshiftAPI
import json

if link_output:
    api = PushshiftAPI()
else:
    import praw

    r = praw.Reddit(
        client_id=os.environ["REDDIT_CLIENT_ID"],
        client_secret=os.environ["REDDIT_CLIENT_SECRET"],
        user_agent='User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36',
        # password=os.environ["REDDIT_PASSWORD"],
        # username=os.environ["REDDIT_USERNAME"],
    )

    api = PushshiftAPI(r)

gen = api.search_submissions(subreddit=subreddit, limit=limit)
results = list(gen)

##
# embed()
# print(json.dumps(results))
##


if link_output:
    for result in results:
        print(result.permalink)
else:
    # @todo3 refactor this into a standalone reddit2org (we need a way to get the submission object from the URL)
    ##
    from brish import z

    def stars(lv):
        return "*" * lv + " "

    def html2org(html):
        return z("html2org =(cat)", cmd_stdin=html).outrs

    def meta_get(c):
        meta = ""
        meta+=f"{c.author.name}"
        if hasattr(c, "score"):
            meta+=f" ({c.score})"

        return meta

    def meta_get_props(c):
        meta = ":PROPERTIES:"
        meta+=f"\n:AUTHOR: {c.author.name}"
        if hasattr(c, "score"):
            meta+=f"\n:SCORE: {c.score}"

        if hasattr(c, "created_utc"):
            meta+=f"\n:DATE_UNIX: {c.created_utc}"

        meta+="\n:END:\n"
        return meta
        
    def process_comment(f, comments, lv):
        l = len(comments) - 1
        for i, c in enumerate(comments):
            lv_c = lv

            ##
            # meta = meta_get(c)
            # meta+=": "
            ##
            meta = meta_get_props(c)
            ##

            head = "EMPTY_COMMENT"
            if c.body_html:
                head = (html2org(c.body_html) or "EMPTY_COMMENT")

            f.write("\n" + stars(lv_c) + head + "\n" + meta)
            lv_c += 1
            process_comment(f, c.replies, lv_c)
            if l != i:
                f.write("\n")


    for result in results:
        # embed() ; exit()
        f_name = z("ecn {result.title} | str2filename").outrs + f".{result.id}" + ".org"
        with open(f_name, "w") as f:
            lv = 1
            f.write(f"#+TITLE: {result.title}\n\n")


            if getattr(result, "url_overridden_by_dest", None):
                f.write(f"{stars(lv)}[[{result.url_overridden_by_dest}][{result.title}]]\n")
                lv += 1
            else:
                f.write(f"{stars(lv)}{result.title}\n")
                lv += 1

            meta = meta_get_props(result)
            f.write(meta)

            if result.selftext_html:
                f.write(html2org(result.selftext_html) + "\n\n")

            process_comment(f, result.comments, lv)
        print(f"wrote {f_name}", file=sys.stderr, flush=True)
