#!/usr/bin/env python3
##
#: Prints the unique, sorted words of the stardict dictionaries in the given
#: directories (searched recursively) to stdout. Words are extracted from the
#: '.idx' files (headwords) and '.syn' files (synonyms/inflections) if present.
##
import sys
from pathlib import Path


def entry_words(data, skip_bytes):
    #: Entries are 'word\0' followed by =skip_bytes= of binary metadata.
    i = 0
    n = len(data)
    while i < n:
        end = data.index(b"\0", i)
        yield data[i:end]
        i = end + 1 + skip_bytes


def dic_words(ifo_path):
    ifo_text = ifo_path.read_text(errors="replace")
    #: offset is 32-bit unless 'idxoffsetbits=64'; size is always 32-bit
    offset_bytes = 8 if "idxoffsetbits=64" in ifo_text else 4

    idx_path = ifo_path.with_suffix(".idx")
    if idx_path.exists():
        yield from entry_words(idx_path.read_bytes(), offset_bytes + 4)

    syn_path = ifo_path.with_suffix(".syn")
    if syn_path.exists():
        #: syn entries: 'word\0' + 32-bit index into the idx
        yield from entry_words(syn_path.read_bytes(), 4)


def main(dic_dirs):
    words = set()
    for dic_dir in dic_dirs:
        for ifo_path in Path(dic_dir).rglob("*.ifo"):
            words.update(dic_words(ifo_path))

    out = sys.stdout.buffer
    try:
        for word in sorted(words):
            out.write(word)
            out.write(b"\n")
    except BrokenPipeError:
        sys.exit(0)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <dic_dir> ...", file=sys.stderr)
        sys.exit(1)

    main(sys.argv[1:])
