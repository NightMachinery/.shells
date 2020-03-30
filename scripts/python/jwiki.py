#!/usr/bin/env python
import traceback
import wikipedia
import sys
from IPython import embed

keywords = sys.argv[1]
results = 3
if len(sys.argv) >= 3:
    results = int(sys.argv[2])
res = wikipedia.search(keywords, results=results, suggestion=True)
# embed()
c = res[0]
if len(res) == 2:
    c.append(res[1])
for candidate in set(c):
    #embed()
    try:
        p = wikipedia.page(candidate, auto_suggest=False)
        print(p.title)
        print()
        print(p.summary)
        print()
        print(p.url)
        print()
    except wikipedia.exceptions.DisambiguationError:
        pass
    except:
        print(traceback.format_exc(), file=sys.stderr)
