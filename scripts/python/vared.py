#!/usr/bin/env python3

import sys, os
import readline


# https://stackoverflow.com/questions/66942530/python-make-input-read-and-write-to-custom-file-descriptors
##
# def call_readline(stdin, stdout, message):
#     '''Implementation of PyOS_Readline() in the "readline" module'''
#     rl_instream = stdin
#     rl_outstream = stdout  # Readline writes to stdout
#     return readline_until_enter_or_signal(message)


def rlinput(message, prefill=""):
    readline.set_startup_hook(lambda: readline.insert_text(prefill))
    try:
        # When EOF is read, EOFError is raised.
        # return input(message)
        ##
        return call_readline(sys.stdin, sys.stderr, message)
    except:
        return None
    finally:
        readline.set_startup_hook()


###
from prompt_toolkit import prompt


def input2(message, default):
    try:
        return prompt(message=message, default=default)
    except:
        return None


###
message = ""
if len(sys.argv) >= 2:
    message = sys.argv[1]

if message in ("-h", "--help"):
    print(
        "Usage:\n\tvared.py [prompt default_value]\n\nReads from the stdin.\nReturns non-zero on exceptions."
    )
    exit(1)

default_value = ""
if len(sys.argv) >= 3:
    default_value = sys.argv[2]

# ans = rlinput(message, default_value)
ans = input2(message, default_value)
if ans != None:
    print(ans)
else:
    exit(1)
