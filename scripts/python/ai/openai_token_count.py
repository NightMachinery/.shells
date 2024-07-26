#!/usr/bin/env python3
##
import os
import tiktoken
import sys
from pynight.common_files import mkdir


##
HOME = os.environ["HOME"]

tiktoken_cache_dir = f"{HOME}/tmp/tiktoken_cache"
mkdir(tiktoken_cache_dir)

os.environ["TIKTOKEN_CACHE_DIR"] = tiktoken_cache_dir
##

def num_tokens_from_message(message, model="gpt-4"):
    encoding = tiktoken.encoding_for_model(model)
    num_tokens = len(encoding.encode(message))
    return num_tokens


message = sys.stdin.read()

num_tokens = num_tokens_from_message(message, "gpt-4")

print(num_tokens)
