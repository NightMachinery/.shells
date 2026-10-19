# Video picker

[agfi:zv] opens videos from its configured library roots, and
[agfi:zv] with a directory argument searches that directory. [agfi:zvv]
searches the library roots with its arguments used as the initial query.
The picker selects one file and launches it through mpv.

[agfi:openv] accepts search roots through `openv_dirs`. With no configured roots
it searches the current directory. It skips nonexistent roots and normalizes
duplicate or symlinked roots. If a configured list contains no existing
directory, it fails without searching the current directory instead.

Discovery uses absolute paths and removes duplicates caused by overlapping
roots before applying tag priorities. The recent-selection wrapper compares
rows without ANSI color codes, so a cached plain path and its colored current
row appear only once. The first row keeps its display bytes and MRU position.

[agfi:play-tag] resolves the selected file itself before launching mpv. It does
not use the destination variable from the disabled tagging operation. An empty
selection, cancellation, discovery error, or missing selected file never
launches a player. No matches produce no candidate rows.

Run `python3 zshlang/tests/video-picker.py` for synthetic checks of discovery,
selection, cancellation, unusual filenames, and repeated MRU use. The tests
record launch arguments without playing media and put history in scratch
space. Run [agfi:brishz-restart] after changing the Zsh functions.
