#!/usr/bin/env python3
##
import tiktoken
import sys


def num_tokens_from_message(message, model="gpt-4"):
    encoding = tiktoken.encoding_for_model(model)
    num_tokens = len(encoding.encode(message))
    return num_tokens


message = sys.stdin.read()

num_tokens = num_tokens_from_message(message, "gpt-4")

print(num_tokens)
