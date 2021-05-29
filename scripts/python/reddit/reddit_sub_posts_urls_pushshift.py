#!/usr/bin/env python3
# Usage:
# `reddit_sub_posts_urls_pushshift.py rational 100000000 > rational.txt`
##
# from IPython import embed

import sys

limit = 10
subreddit = "rational"

al = len(sys.argv)
if al >= 2:
    subreddit = sys.argv[1]
if al >= 3:
    limit = int(sys.argv[2])

from psaw import PushshiftAPI
import json

api = PushshiftAPI()

gen = api.search_submissions(subreddit=subreddit, limit=limit)
results = list(gen)

# embed()
# print(json.dumps(results))

for result in results:
    print(result.permalink)
