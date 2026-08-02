--: Converts hard line breaks to soft breaks.
--:
--: Pandoc's org writer emits hard breaks as `\\` (org forced newlines),
--: which `pandoc-convert` used to strip textually afterwards
--: (`org-trim-forced-newlines` = `sd '\\\\' ''`) — corrupting LaTeX row
--: separators like `0&1\\` inside math, which lives in Math/RawBlock
--: nodes this filter never touches. Converting at the AST level removes
--: the need for any textual stripping.

function LineBreak()
  return pandoc.SoftBreak()
end
