#!/usr/bin/env python3

import sys, os
from rich.console import Console
from rich.markdown import Markdown

console = Console(force_terminal=bool(os.environ.get("rich_force_terminal", True)))
with open(sys.argv[1]) as readme:
    markdown = Markdown(readme.read(), code_theme="solarized-light")
    # themes are pygment: https://pygments.org/demo/
console.print(markdown)
