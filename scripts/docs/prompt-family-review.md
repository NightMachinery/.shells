# Prompt-family review

@OpenAI, 2026-10-22. Reviewed `zshlang/auto-load/others/prompt/` for use as stdin
transformations in OmniPrompter. These are suggestions; existing functions were
not broadly rewritten.

The most useful improvement is a consistent stdin/stdout contract. Some listed
functions require argv (`prompt-summarize-url`, `prompt-coco2imagenet`), while
`prompt-imdb-recommend` uses `$*` rather than stdin. Internal helpers such as
`prompt-input-file` also match the public naming convention. A small optional
registry could label functions, declare required arguments and distinguish
public transformations from internal helpers. Keep automatic name discovery as
a fallback so generated prompt functions remain accessible.

`snippet-chain` initializes empty output instead of consuming stdin, and calls
`ec-copy` internally. Seed the chain with stdin and keep transformations stdout
only; let the caller copy once at the boundary. A compatibility wrapper can
preserve the existing interactive clipboard behavior.

In `prompt-instruction-input`, the pattern `(?i)^none|n$` matches more than the
whole values `none` and `n`, for example `python`. Use an exact case branch or
`(?i)^(?:none|n)$`. Its fixed triple-backtick input wrapper can also break when
input already contains code fences; choose a fence longer than any input run.

Other targeted cleanups:

- `prompt-define-long` presents a JSON example with a trailing comma and an
  ellipsis. Use valid example JSON if output is expected to parse.
- `audio.zsh` asks for word-for-word transcription while also asking to remove
  fillers and false starts. Separate verbatim and edited transcript modes.
- `def-prompts-from-file` defines `_define_one` as a function despite declaring
  a scalar of that name local. Use a scoped anonymous helper or explicit
  cleanup to avoid leaving implementation functions in the global table.

A registry adds maintenance work, so begin with the stdin contract and the
incorrect input-mode regex. They fix concrete compatibility and correctness
problems without redesigning the prompt library.
