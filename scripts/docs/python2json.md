# python2json: Python reprs to JSON

## What it is

`python2json` turns a Python literal or `repr` into pretty JSON, so that
something you copied out of a REPL or a log becomes `jq`-able:

```zsh
ec "{'id': 7, 'ok': True, 'x': None, 'tup': (1, 2)}" | python2json
```

```json
{
  "id": 7,
  "ok": true,
  "x": null,
  "tup": [
    1,
    2
  ]
}
```

Input comes from arguments, stdin, or — when neither is given — the clipboard,
via [agfi:in-or-args]. The result is copied back to the clipboard when stdout
is a tty, like [agfi:json5-to-json].

The zsh function lives in `zshlang/auto-load/others/json.zsh` and is a thin
wrapper around `python/python2json.py` piped through `command jq .`; the script
alone emits compact JSON and takes `--indent N` if you want it pretty without
`jq`.

## Why not json-beautify2

[agfi:json-beautify2] (`prettier --parser=json5`) happens to survive a plain
Python dict, because JSON5 also allows single quotes and trailing commas. It
does not survive anything else in a real `repr`: `True`/`False`/`None`, tuples,
`b'...'`, or object reprs. It also needs prettier from npm, whereas
`python2json.py` is stdlib only.

## Both repr forms are accepted

`print(obj.to_dict())` gives a dict, handled as above.

`print(obj)` gives a constructor call, and a call whose arguments are *all*
keyword arguments is expanded into an object tagged with its type:

```zsh
ec "Contact(id=1, name='someone', tags=[Tag(t='a')])" | python2json
```

```json
{
  "_type": "Contact",
  "id": 1,
  "name": "someone",
  "tags": [
    {
      "_type": "Tag",
      "t": "a"
    }
  ]
}
```

A call with positional arguments is not expanded — it becomes its own source
text as a string, since `{"_type": "datetime.datetime", ...}` with numbered
fields would be less readable than `"datetime.datetime(2020, 1, 2)"`.

## Values with no JSON equivalent

Anything the parser cannot turn into JSON data becomes a string of its source
text: `<mod.pkg.Thing object>`, `"datetime.datetime(2020, 1, 2)"`,
`"Ellipsis"`. Nothing is silently dropped, so a present-but-opaque field is
still distinguishable from an absent one.

Memory addresses are **stripped**: `<mod.pkg.Thing object at 0x15f8550>`
becomes `<mod.pkg.Thing object>`, so two dumps of the same object compare and
diff equal. Set `python2json_address_p=y` (or pass `--keep-address` to the
script) to keep them.

Also: `bytes` become a lowercase hex string; sets and tuples become arrays;
dict keys that are not strings are stringified, so `{(1, 2): 'x'}` keys on
`"[1, 2]"`.

Large integers stay JSON numbers. Our `jq` (1.6-159-gcff5336) round-trips an
unmodified 19-digit literal exactly; only doing arithmetic on it in `jq` would
lose precision.

## How it parses

`ast.literal_eval` cannot be used on its own: `<mod.pkg.Thing object at 0x...>`
is not valid Python at all, and a `repr` may contain calls and bare names.
So `python2json.py` works in two stages.

1. It tokenizes the input with `tokenize.generate_tokens` and replaces each
   top-level `<` … matching `>` span with a JSON string literal. Tokenizing
   rather than regexing the raw text matters: a `<` inside a string value —
   `{'s': 'a <b> c'}` — is a string character, not the start of a repr, and
   only the tokenizer knows the difference. Nesting is depth-counted. If the
   tokenizer chokes (a repr containing an unbalanced quote), it falls back to a
   `<[^<>]*>` regex.

2. It parses the result with `ast.parse(..., mode="eval")` and walks the tree,
   mapping dicts, lists, tuples, sets and constants onto JSON, expanding
   keyword-only calls as above, and falling back to `ast.unparse` for
   everything else.

Note that stage 2 only ever *reads* the syntax tree — the input is never
`eval`'d, so pasting a repr from an untrusted source executes nothing.

Malformed input exits non-zero with a message on stderr and prints nothing to
stdout.
