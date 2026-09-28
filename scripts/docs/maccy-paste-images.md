# Exporting images from Maccy

`maccy-paste-images [n=1]` saves the most recently copied `n` image items from
Maccy's history into the current directory. Text entries do not count toward
`n`. If the history contains fewer than `n` images, it saves those available.

The function reads Maccy's SQLite database in read-only mode. It leaves Maccy
history and the system clipboard untouched. It exports stored PNG or TIFF
bytes, not image files referenced by file URLs. If one history item contains
both formats, it uses PNG.

Names follow `maccy-YYYYMMDD-HHMMSS-<history-id>.png` (or `.tiff`), using local
time and the item's last-copy time. Existing files are skipped without being
overwritten; newly saved paths are printed, one per line. The function needs
macOS, Maccy, and Python 3.

If SQLite cannot open the history database, the function retries twice. A
persistent failure reports the database path along with SQLite's error.
