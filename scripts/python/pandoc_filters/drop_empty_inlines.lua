--: Drops empty inline formatting nodes.
--:
--: Outlook-composed emails (as rendered by Gmail) are littered with empty
--: `<u></u>` elements (Outlook's `<o:p></o:p>` paragraph markers rewritten
--: by Gmail). Pandoc's HTML reader parses each as `Underline []`, which
--: the org writer emits as a literal `__` (and other writers emit as
--: similar junk). A textual fix would corrupt legitimate `__` (e.g.,
--: Python dunders in code blocks), so we drop the empty nodes at the AST
--: level instead. Pandoc walks innermost-first, so nested cases like
--: `<b><u></u></b>` collapse fully.
--:
--: Spans are left alone, as they may carry anchors/attributes.

local function drop_if_empty(el)
  if #el.content == 0 then
    return {}
  end
end

return {
  {
    Underline = drop_if_empty,
    Emph = drop_if_empty,
    Strong = drop_if_empty,
    Strikeout = drop_if_empty,
    Superscript = drop_if_empty,
    Subscript = drop_if_empty,
    SmallCaps = drop_if_empty,
  },
}
