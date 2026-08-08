--: Recovers `<sup>`/`<sub>` as real Superscript/Subscript nodes.
--:
--: pandoc's org writer only emits raw inlines verbatim for latex/tex/org
--: (`isRawFormat`); everything else is dropped, and it says so:
--:
--:   $ printf 'a <sup>b</sup> c\n' | pandoc --verbose -f markdown -t org
--:   [INFO] Not rendering RawInline (Format "html") "<sup>"
--:   [INFO] Not rendering RawInline (Format "html") "</sup>"
--:   a b c
--:
--: So the citation markers LLM transcripts append to sentences --
--: `<sup>[[Short Title](https://...)]</sup>` -- lost their superscript and
--: collapsed into the surrounding prose as `[[[url][title]]]`: three opening
--: brackets that read as broken org (they do parse as a link, but only by
--: accident of the outer pair being literal text).
--:
--: Converting to a Superscript node makes the org writer emit `^{...}`, which
--: round-trips: org parses `^{[[url][title]]}` as a superscript containing a
--: link, and ox-html exports it back to `<sup><a href=...>...</a></sup>`.
--:
--: Org output only -- other writers handle raw HTML inlines themselves.

--: `%s` is locale-dependent and can match 0xA0, a byte that also occurs inside
--: UTF-8 sequences; match ASCII whitespace explicitly. See org_details.lua.
local WS = "[ \t\r\n\v\f]"

local TAGS = {
  sup = pandoc.Superscript,
  sub = pandoc.Subscript,
}

--: Returns the tag name when `il` is a raw html `<sup>`/`<sub>` (open when
--: `closing` is false, else the matching close tag).
local function html_tag(il, closing)
  if not (il and il.t == "RawInline" and il.format == "html") then
    return nil
  end
  local pat = closing and ("^" .. WS .. "*</(%a+)>" .. WS .. "*$")
    or ("^" .. WS .. "*<(%a+)[ \t\r\n\v\f>]")
  local name = il.text:match(pat)
  if name and TAGS[name:lower()] then
    return name:lower()
  end
  return nil
end

--: `<sup>[<link>]</sup>` -- the literal brackets are the transcript's citation
--: style, but org links carry their own, so `^{[[[url][t]]]}` would show three.
--: Drop the redundant pair when that is the entire content.
local function unwrap_brackets(inlines)
  if #inlines == 3
    and inlines[1].t == "Str" and inlines[1].text == "["
    and inlines[2].t == "Link"
    and inlines[3].t == "Str" and inlines[3].text == "]"
  then
    return { inlines[2] }
  end
  return inlines
end

local function convert(inlines)
  local out, i, changed = pandoc.List(), 1, false
  while i <= #inlines do
    local name = html_tag(inlines[i], false)
    --: Locate the matching close tag; nesting of sup/sub is not meaningful,
    --: so the first one wins.
    local close = nil
    if name then
      for j = i + 1, #inlines do
        if html_tag(inlines[j], true) == name then
          close = j
          break
        end
      end
    end

    if close then
      local content = {}
      for j = i + 1, close - 1 do
        table.insert(content, inlines[j])
      end
      out:insert(TAGS[name](unwrap_brackets(content)))
      changed = true
      i = close + 1
    else
      out:insert(inlines[i])
      i = i + 1
    end
  end

  if changed then
    return out
  end
end

return {
  { Inlines = convert },
}
