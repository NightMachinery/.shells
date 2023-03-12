#!/usr/bin/env python3
#: @alt org_html_postprocess.pl
#: [[https://stackoverflow.com/questions/74263235/beautifulsoup-search-and-replace-in-the-text-parts-of-html][python - BeautifulSoup: Search and replace in the text parts of HTML - Stack Overflow]]
##
import sys
import os
from icecream import ic
import re
from bs4 import BeautifulSoup, NavigableString, XMLParsedAsHTMLWarning
from IPython import embed

import warnings
warnings.filterwarnings('ignore', category=XMLParsedAsHTMLWarning)

html = sys.stdin.read()
soup = BeautifulSoup(html, features='lxml')

at_re = re.compile(r'''(?:(?<=\s)|(?<=^))(@(?:\w|/|\d|[][(),.;'])+)(?=\s|$)''')

for n in soup.find_all(string=at_re): #: =string= used to be =text=
    parent = n.parent

    # ic(type(n), n, parent)
    # with open('/dev/tty') as user_tty:
    #     sys.stdin=user_tty
    #     embed()
    # break

    parent_classes = parent.get('class', []) #: returns a list
    # ic(parent_classes)
    if not any(c in parent_classes for c in ('todo', 'done', 'at_tag')):
        n_str = str(n)
        n2 = at_re.sub(r'<span class="todo at_tag">\1</span>', n_str)
        n2_soup = BeautifulSoup(n2,
                                # features='html.parser',
                                features='lxml',
                                )
        n.replace_with(n2_soup)

        # ic(type(n2_soup), n2_soup)

print(soup)
