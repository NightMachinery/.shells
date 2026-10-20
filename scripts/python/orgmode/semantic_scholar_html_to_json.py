#!/usr/bin/env python3
"""Deprecated CLI shim: extraction and normalization now live in Go."""
import os
import sys

args = sys.argv[1:]
modes = {"--markdown": "markdown", "--fetch-json": "fetch-json",
         "--page-data": "page-data", "--validate": "json"}
mode = modes.get(args[0], "html") if args else "html"
if args and args[0] in modes:
    args = args[1:]
if len(args) != 1:
    sys.exit("usage: semantic_scholar_html_to_json.py [--markdown|--fetch-json|--page-data|--validate] <paper-url>")
print("semantic_scholar_html_to_json.py is deprecated; use org-paper-metadata --parse.", file=sys.stderr)
os.execvp("zsh", ["zsh", "-c", 'h-org-paper-tools-dep || exit $?; command org-paper-metadata --format json --parse "$1" "$2"', "_", mode, args[0]])
