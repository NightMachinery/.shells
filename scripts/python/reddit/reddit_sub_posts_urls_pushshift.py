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

import sys
import os
import traceback
import time

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
    from praw.exceptions import DuplicateReplaceException
    from praw.models.reddit.more import MoreComments

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
    from brish import z, zp, bsh

    def stars(lv):
        return "*" * lv + " "

    def html2org(html):
        # tmp = "tmp.html"
        tmp = z("mktemp").outrs

        res = z("cat > {tmp}", cmd_stdin=html)
        assert res

        res = z("html2org {tmp}")
        assert res

        z("command rm {tmp}")

        return res.outrs

    # def meta_get(c):
    #     meta = ""
    #     meta+=f"{c.author.name}"
    #     if hasattr(c, "score"):
    #         meta+=f" ({c.score})"

    #     return meta

    def meta_get_props(c):
        meta = ":PROPERTIES:"

        if c.author: # can be None when they are deleted
            meta+=f"\n:Author: {c.author.name}"

        if hasattr(c, "score"):
            meta+=f"\n:Score: {c.score}"

        if hasattr(c, "created_utc") and c.created_utc:
            meta+=f"\n:DateUnix: {c.created_utc}"
            res = z('gdate -d "@"{c.created_utc} +"%Y-%b-%d"')
            if res:
                meta+=f"\n:DateShort: {res.outrs}"


        if getattr(c, "link_flair_text", None):
            meta+=f"\n:FlairText: {c.link_flair_text}"

        meta+="\n:END:\n"
        return meta
        
    def process_comment(f, comments, lv, shortname):
        while True:
            try:
                comments.replace_more(limit=None)
                break
            except DuplicateReplaceException:
                print(traceback.format_exc())
                break
            except:
                print(traceback.format_exc())
                time.sleep(1)

        l = len(comments) - 1

        for i, c in enumerate(comments):
            lv_c = lv

            # if isinstance(c, MoreComments):
            #     pass
            ##
            # meta = meta_get(c)
            # meta+=": "
            ##
            meta = meta_get_props(c)
            # Properties are key--value pairs. When they are associated with a single entry or with a tree they need to be inserted into a special drawer (see [[https://orgmode.org/manual/Drawers.html#Drawers][Drawers]]) with the name ‘=PROPERTIES=', which has to be located right below a headline, and its planning line (see [[https://orgmode.org/manual/Deadlines-and-Scheduling.html#Deadlines-and-Scheduling][Deadlines and Scheduling]]) when applicable.
            #
            # Still, putting the props after the heading is no fun; We can rename our drawer to :METADATA:, but why bother?
            ##

            head = "EMPTY_COMMENT"
            c_id = c.id or z("uuidm").outrs
            shortname += f"/{c_id}"
            if c.body_html:
                head = (html2org(c.body_html) or "EMPTY_COMMENT")
                index_file = f'indices/{shortname}/{c_id}.org'
                z("ensure-dir {index_file}")
                with open(index_file, "w") as f2:
                    f2.write(f"{meta}\n{head}")

                if head.startswith("#+"):
                    # do not put blocks in headings (e.g., #+begin_quote)

                    author = "deleted"
                    if c.author:
                        author = c.author.name or author

                    head = f"u/{author}:\n{head}"

            f.write("\n" + stars(lv_c) + head + "\n" + meta)
            lv_c += 1
            process_comment(f, c.replies, lv_c, shortname)
            if l != i:
                f.write("\n")

    def utf8len(s):
        return len(s.encode('utf-8'))

    for result in results:
        try:
            # embed() ; exit()

            # if not "looking at this sub" in result.title:
            #     continue

            f_name = z("ecn {result.title} | str2filename").outrs
            f_name = f_name[0:230]
            if utf8len(f_name) > 240:
                f_name = f_name[0:100]

            if utf8len(f_name) > 240:
                f_name = f_name[0:60]

            ## max file name length (this is different from max path length, which is usually 4096 bytes)
            # @linux `getconf NAME_MAX /dev/sda1`, `getconf PATH_MAX /dev/sda1`
            #
            # BTRFS   255 bytes
            # exFAT   255 UTF-16 characters
            # ext2    255 bytes
            # ext3    255 bytes
            # ext3cow 255 bytes
            # ext4    255 bytes
            # FAT32   8.3 (255 UCS-2 code units with VFAT LFNs)
            # NTFS    255 characters
            # XFS     255 bytes
            ##

            shortname = f"{f_name}.{result.id}"
            f_name = f"posts/{shortname}.org"
            z("ensure-dir {f_name}")
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

                process_comment(f, result.comments, lv, shortname)
            print(f"wrote {f_name}\n", file=sys.stderr, flush=True)

        except:
            print(traceback.format_exc())
            embed()
