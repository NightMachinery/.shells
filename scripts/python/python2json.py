#!/usr/bin/env python3
##
#: Converts a Python literal/repr (e.g. the output of `print(obj.to_dict())`)
#: to JSON. Stdlib only.
##
import argparse
import ast
import io
import json
import re
import sys
import tokenize


ADDRESS_RE = re.compile(r"\s+at\s+0[xX][0-9a-fA-F]+(?=>?$)")
ANGLE_RE = re.compile(r"<[^<>]*>")


def angle_spans(src):
    """Source offsets of each top-level `<...>` span, tokenizer-aware.

    Tokenizing (rather than regexing the raw text) keeps a `<` that merely
    occurs inside a string value from being treated as the start of a repr.
    """
    line_starts = [0]
    for line in src.splitlines(keepends=True):
        line_starts.append(line_starts[-1] + len(line))

    def offset(pos):
        row, col = pos
        return line_starts[row - 1] + col

    spans = []
    depth = 0
    start = 0
    for token in tokenize.generate_tokens(io.StringIO(src).readline):
        if token.type != tokenize.OP:
            continue

        if token.string == "<":
            if depth == 0:
                start = offset(token.start)
            depth += 1
        elif token.string == ">" and depth > 0:
            depth -= 1
            if depth == 0:
                spans.append((start, offset(token.end)))

    return spans


def clean_repr(text, *, keep_address=False):
    if not keep_address:
        text = ADDRESS_RE.sub("", text)

    return text


def quote_reprs(src, *, keep_address=False):
    """Replaces every `<...>` repr with a JSON string literal, so that the
    result is parseable Python."""
    try:
        spans = angle_spans(src)
    except (tokenize.TokenError, IndentationError, SyntaxError):
        #: The tokenizer chokes on reprs containing unbalanced quotes; fall
        #: back to a plain (string-blind) regex.
        return ANGLE_RE.sub(
            lambda m: json.dumps(clean_repr(m.group(0), keep_address=keep_address)),
            src,
        )

    out = []
    prev = 0
    for start, end in spans:
        out.append(src[prev:start])
        out.append(
            json.dumps(clean_repr(src[start:end], keep_address=keep_address))
        )
        prev = end
    out.append(src[prev:])

    return "".join(out)


def jsonify(node):
    """Converts an AST node of a Python literal/repr to JSON-able data."""
    if isinstance(node, ast.Constant):
        value = node.value
        if isinstance(value, (bytes, bytearray)):
            return bytes(value).hex()
        elif isinstance(value, complex):
            return str(value)
        elif value is Ellipsis:
            return "..."
        else:
            return value

    elif isinstance(node, (ast.List, ast.Tuple, ast.Set)):
        return [jsonify(element) for element in node.elts]

    elif isinstance(node, ast.Dict):
        result = {}
        for key, value in zip(node.keys, node.values):
            if key is None:
                #: `{**other}`; nothing sensible to key it by.
                result["**"] = jsonify(value)
            else:
                key = jsonify(key)
                result[key if isinstance(key, str) else json.dumps(key)] = jsonify(
                    value
                )

        return result

    elif isinstance(node, ast.Call) and node.keywords and not node.args:
        #: `User(id=1, first_name='...')`, i.e. the `print(obj)` repr form.
        result = {"_type": ast.unparse(node.func)}
        for keyword in node.keywords:
            if keyword.arg is None:
                result["**"] = jsonify(keyword.value)
            else:
                result[keyword.arg] = jsonify(keyword.value)

        return result

    try:
        #: Catches negative numbers, `1e3`, `1+2j`, etc.
        return jsonify(ast.Constant(value=ast.literal_eval(node)))
    except (ValueError, SyntaxError, TypeError, MemoryError, RecursionError):
        #: `datetime.datetime(2020, 1, 2)`, bare names, comparisons, ...
        return ast.unparse(node)


def python2json(src, *, keep_address=False):
    src = quote_reprs(src.strip(), keep_address=keep_address)

    return jsonify(ast.parse(src, mode="eval").body)


def main():
    parser = argparse.ArgumentParser(
        description="Convert a Python literal/repr on stdin to JSON on stdout."
    )
    parser.add_argument(
        "--keep-address",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="keep the memory address in object reprs, e.g. `<m.C object at 0x1>` rather than `<m.C object>` (default: %(default)s)",
    )
    parser.add_argument(
        "--indent",
        type=int,
        default=None,
        help="indent the output by this many spaces (default: %(default)s, i.e. compact)",
    )
    args = parser.parse_args()

    src = sys.stdin.read()
    if not src.strip():
        print("python2json: empty input", file=sys.stderr)
        return 1

    try:
        data = python2json(src, keep_address=args.keep_address)
    except SyntaxError as e:
        print(f"python2json: could not parse the input as Python: {e}", file=sys.stderr)
        return 1

    print(json.dumps(data, indent=args.indent, ensure_ascii=False))

    return 0


if __name__ == "__main__":
    sys.exit(main())
