#!/usr/bin/env python3
##
import sys
import tokenize
from io import BytesIO
import argparse


def remove_comments(code):
    tokens = tokenize.tokenize(BytesIO(code.encode("utf-8")).readline)
    new_tokens = []

    newline_token_types = [
        tokenize.NL,
        tokenize.NEWLINE,
    ]
    #: tokenize.NL stands for Non-Logical Newline.
    #: [[https://stackoverflow.com/questions/27382508/what-is-the-difference-between-nl-and-newline-in-tokenizer-py][python - What is the difference between NL and NEWLINE in tokenizer.py? - Stack Overflow]]

    for tok in tokens:
        token_type = tok.type
        token_string = tok.string

        if token_type == tokenize.COMMENT:
            #: Remove any preceding whitespace tokens
            # while new_tokens and (
            #     new_tokens[-1].type in newline_token_types
            #     or new_tokens[-1].string.strip() == ""
            # ):
            #     deleted_token = new_tokens.pop()
            #     if deleted_token.type in newline_token_types:
            #     # if deleted_token in newline_token_types:
            #         #: We don't want to remove whitespace from previous lines.
            #         break

            continue
        else:
            new_tokens.append(tok)

    return tokenize.untokenize(new_tokens).decode()


def main():
    parser = argparse.ArgumentParser(
        description="Remove comments and any preceding whitespace from Python code provided via standard input."
    )
    args = parser.parse_args()

    code = sys.stdin.read()
    cleaned_code = remove_comments(code)
    print(cleaned_code)


if __name__ == "__main__":
    main()
