# ntag-recoverpath

`ntag-recoverpath` resolves a path that no longer exists because ntags were added to or removed
from the filename.

## Filename Model

ntags are stored in the filename with `..` as the separator:

```text
head..tag..ext
head..tag1..tag2..ext
```

The untagged head is the part of the filename before the first `..`. The extension is the final
suffix after the last `.` as reported by the caller's path model.

## Recovery Behavior

Given an input path:

1. If the input path exists, return it unchanged.
2. If the input path does not exist, search only the input path's parent directory.
3. Check the untagged fallback filename in that directory: `head.ext`, or `head` for suffixless
   paths.
4. If the untagged fallback exists, return it. This handles a stored tagged path after its tags
   have been removed.
5. Build a regular expression from the escaped untagged head and escaped suffix.
6. Match filenames that have the same head, at least one `..tag..` segment, and the same suffix.
7. Sort matching filenames lexicographically and return the first candidate's full path.
8. If no candidate matches, return the original input path and report recovery failure to callers
   that need that distinction.

Example:

```text
input:  /tmp/image.png
match:  /tmp/image..red..png
```

If `/tmp/image.png` exists, it is preferred over `/tmp/image..red..png`.

Reverse recovery example:

```text
input:  /tmp/image..red..png
match:  /tmp/image.png
```

If the stored tagged path is missing and `/tmp/image.png` exists, the untagged path wins over any
other tagged candidate.

## Regular Expression Matching

Implementations should use regular expressions, not shell globs. The head and suffix must be escaped
before inserting them into the pattern.

For a path with extension `png`, the filename regexp is:

```text
^HEAD\.\..+\.\.png$
```

For a suffixless path, the filename regexp is:

```text
^HEAD\.\..+\.\.$
```

## IPC Behavior

When qView's IPC socket answers a current-file-path request, it should recover the stored current
path first. If the file was externally retagged after qView loaded it, IPC should return the current
tagged path. If those tags are later removed, IPC should return the recovered untagged path. If the
stored path still exists, IPC should return it unchanged.

## Non-goals

- Do not recurse into subdirectories.
- Do not recover directory paths.
- Do not support separators other than `..`.
- Do not call shell helpers from native qView code.
