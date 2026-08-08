--: @upstreamBug Emits raw HTML blocks as `#+begin_export html` instead of
--: pandoc's `#+begin_html`.
--:
--: pandoc's org writer hardcodes the Org 7/8-era wrapper
--: (src/Text/Pandoc/Writers/Org.hs, still current as of pandoc 3.7.0.2):
--:
--:   blockToOrg (RawBlock "html" str) =
--:     return $ blankline $$ "#+begin_html" $$
--:              nest 2 (literal str) $$ "#+end_html" $$ blankline
--:
--: Org replaced that with `#+begin_export html` in 9.2 (2018). On Org 9.2+
--: `#+begin_html` parses as a plain *special block*, not an export block --
--: verified with `org-element-at-point` on Org 9.6.15 -- so the HTML is no
--: longer passed through on export; it is treated as org prose inside a
--: `<div class="html">`.
--:
--: pandoc never notices because its own org *reader* accepts both spellings
--: and yields an identical `RawBlock (Format "html")`, so every pandoc
--: round-trip is lossless; only real Emacs Org-mode sees the breakage.
--: That also makes the rewrite safe: pandoc can still read back what we emit.
--:
--: Done at the AST level rather than as a textual `#+begin_html` ->
--: `#+begin_export html` postprocess so that a literal `#+begin_html`
--: appearing inside a source/example block is not rewritten.
--:
--: `org` is one of the formats pandoc's org writer emits verbatim
--: (`isRawFormat` = latex | tex | org), so re-tagging the block is enough to
--: control the exact output.
--:
--: Org output only; do not add this filter for other writers.

--: `%s` is locale-dependent and can match 0xA0, a byte that also occurs inside
--: UTF-8 sequences; match ASCII whitespace explicitly. See org_details.lua.
local WS = "[ \t\r\n\v\f]"

function RawBlock(el)
  if el.format ~= "html" then
    return nil
  end

  local text = el.text:gsub(WS .. "+$", "")
  if text == "" then
    return {}
  end

  --: The Div wrapper restores the blank-line separation pandoc's own
  --: `#+begin_html` branch emits (`blankline $$ ... $$ blankline`); bare raw
  --: blocks are glued to their neighbors. Same trick as org_math_env.lua.
  return pandoc.Div(pandoc.RawBlock(
    "org",
    "#+begin_export html\n" .. text .. "\n#+end_export"
  ))
end
