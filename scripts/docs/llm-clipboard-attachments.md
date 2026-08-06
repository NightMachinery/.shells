# LLM Clipboard Attachments (`MAGIC_CLIPBOARD`)

Functions like `run-prompt-image-ocr-latex`, `run-prompt-image-ocr`, and the `xzz` alias attach the current clipboard image to an LLM request by putting the sentinel `MAGIC_CLIPBOARD` into the `llm_attachments` global (via the `with-llm-attach-clipboard` alias in `zshlang/auto-load/others/openai.zsh`).

## Flow

- `run-prompt-image-ocr-latex` → `llm-run` (`reval-to-llm`) → `prompt-image-ocr-latex | llm-send` → `llm-m` → `llm` CLI.
- The prompt functions (`prompt-image-*` in `zshlang/auto-load/others/prompt/image.zsh`) only produce the prompt text; the actual attachment is handled on the `llm-send`/`llm-m` side.
- `h-llm-attachments-resolve-clipboard` (in `openai.zsh`) replaces each `MAGIC_CLIPBOARD` entry in `llm_attachments` with a temp PNG saved from the clipboard via [agfi:pbpaste-image], previewing it with [agfi:icat-v]. It is idempotent, so it is safe to call more than once.

## Ordering constraint

`llm-send` copies its input text to the system clipboard when `llm_input_copy_p` is truthy (the default), which destroys any image on the clipboard. Therefore `MAGIC_CLIPBOARD` must be resolved to a temp file *before* that copy happens.

`llm-send` calls `h-llm-attachments-resolve-clipboard` at its very top for this reason. `llm-m` also calls it (for callers that invoke `llm-m` directly). If you add a new code path that writes to the clipboard before the request is sent, resolve the attachments first by calling `h-llm-attachments-resolve-clipboard`.

Historical bug: `llm-send` used to copy the input text first and only later resolve `MAGIC_CLIPBOARD` inside `llm-m`, so `pngpaste` found text instead of an image and the attachment failed.
