#!/usr/bin/env python
# -*- encoding: utf-8
"""
Best-effort clean up of downloaded YouTube .vtt subtitle files.
Forked from: https://github.com/alexwlchan/junkdrawer/blob/development/services/youtube/vtt2txt.py
"""

import itertools
import re
import sys


data = open(sys.argv[1]).read()

# Throw away the header, which is of the form:
#
#     WEBVTT
#     Kind: captions
#     Language: en
#     Style:
#     ::cue(c.colorCCCCCC) { color: rgb(204,204,204);
#      }
#     ::cue(c.colorE5E5E5) { color: rgb(229,229,229);
#      }
#     ##
#
data = data.split("##\n", 1)[-1]

# Now throw away all the timestamps, which are typically of
# the form:
#
#     00:00:01.819 --> 00:00:01.829 align:start position:0%
#
data, _ = re.subn(
    r'\d{2}:\d{2}:\d{2}\.\d{3} \-\-> \d{2}:\d{2}:\d{2}\.\d{3} align:start position:0%\n',
    '',
    data
)

# And the color changes, e.g.
#
#     <c.colorE5E5E5>
#
data, _ = re.subn(r'<c\.color[0-9A-Z]{6}>', '', data)

# And any other timestamps, typically something like:
#
#    </c><00:00:00,539><c>
#
# with optional closing/opening tags.
data, _ = re.subn(r'(?:</c>)?(?:<\d{2}:\d{2}:\d{2}\.\d{3}>)?(?:<c>)?', '', data)

# 00:00:03,500 --> 00:00:03,510
data, _ = re.subn(r'\d{2}:\d{2}:\d{2}\.\d{3} \-\-> \d{2}:\d{2}:\d{2}\.\d{3}\n', '', data)

# Now get the distinct lines.
data = [line.strip() for line in data.splitlines() if line.strip()]

for line, _ in itertools.groupby(data):
    print(line)

# pprint(data)
# components = [data]
# while True:
#     i = len(components)
#
#     last_component = components.pop()
#     if f'\n{i}\n' in last_component:
#         components.extend(list(last_component.split(f'\n{i}\n')))
#         assert len(components) == i + 1
#     elif last_component.startswith('1\n'):
#         components.extend(list(last_component.split(f'1\n', 1)))
#     else:
#         break

# # Now chuck away the first bit, which is something like "Kind: captions"
# # -- I don't know what it is, but we don't need it.
# if components[0].startswith('Kind: captions\n'):
#     components.pop(0)
#
# # Next, remove all the trailing whitespace from each subtitle.
# components = [c.rstrip() for c in components]
#
# # This gets a lot of duplication -- try to remove it as best we can.
# dedupe_components = []
# for c in components:
#     if not c:
#         continue
#
#     for line in c.splitlines():
#         if dedupe_components and dedupe_components[-1] == line:
#             continue
#         else:
#             dedupe_components.append(line)
#
# with open(sys.argv[1] + '.txt', 'w') as outfile:
#     outfile.write('\n'.join(dedupe_components))
#
# print(sys.argv[1] + '.txt')
