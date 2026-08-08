--: Folds `<details>`/`<summary>` HTML into a foldable org special block.
--:
--: LLM chat transcripts and GitHub READMEs wrap collapsible sections in
--: `<details><summary>...</summary>` with a blank line before the body, so
--: that the body is still parsed as markdown. A blank line *terminates* an
--: HTML block in pandoc's markdown reader, and pandoc's AST has no node for
--: "raw wrapper containing parsed blocks", so one `<details>` arrives as
--: four sibling nodes:
--:
--:   RawBlock (Format "html") "<details>"
--:   RawBlock (Format "html") "<summary>"
--:   Plain [RawInline "<strong>", Str "Sources", RawInline "</strong>"]
--:   RawBlock (Format "html") "</summary>"
--:   <the body blocks, parsed normally>
--:   RawBlock (Format "html") "</details>"
--:
--: The org writer then wraps *each* raw block on its own, so an 11-`<details>`
--: transcript yields 44 `#+begin_html` blocks of noise, and the summary text
--: lands as an ordinary paragraph (the org writer silently drops raw *inline*
--: HTML, so `<strong>` is lost -- see [agfi:pandoc-convert] and
--: ~/scripts/docs/md2org-details.md).
--:
--: We rebuild the grouping as an org special block, which folds with TAB:
--:
--:   #+begin_details Sources
--:   <the body blocks, as org>
--:   #+end_details
--:
--: Nested `<details>` are handled. A `<details>` whose whole subtree lives in
--: a single RawBlock (no blank line anywhere inside) is left alone;
--: org_raw_html.lua turns that into a valid export block instead.

--: Lua patterns are byte-based and `%s` calls the locale's `isspace()`, which
--: matches 0xA0 (Latin-1 NBSP) under some locales -- that byte also occurs
--: *inside* UTF-8 sequences, e.g. 🛠 (U+1F6E0) = F0 9F 9B A0, so `%s` would
--: shred emoji in summary titles. Match ASCII whitespace explicitly instead.
local WS = "[ \t\r\n\v\f]"

local function is_raw_html(b)
  return b.t == "RawBlock" and b.format == "html"
end

local function tag_is(b, pat)
  return is_raw_html(b) and b.text:match(pat) ~= nil
end

local DETAILS_OPEN = "^" .. WS .. "*<details[ \t\r\n\v\f>]"
local DETAILS_CLOSE = "^" .. WS .. "*</details>"
local SUMMARY_OPEN = "^" .. WS .. "*<summary[ \t\r\n\v\f>]"
local SUMMARY_CLOSE = "^" .. WS .. "*</summary>"

--: The org writer drops RawInline html, so `<strong>`/`<em>` around the
--: summary text would vanish silently; strip the tags and keep the text.
local function summary_text(blk)
  local kept = {}
  for _, il in ipairs(blk.content) do
    if not (il.t == "RawInline" and il.format == "html") then
      table.insert(kept, il)
    end
  end
  local text = pandoc.utils.stringify(pandoc.Para(kept))
  --: A special block's parameters are single-line by definition.
  text = text:gsub(WS .. "+", " ")
  return (text:gsub("^ +", ""):gsub(" +$", ""))
end

--: Index of the RawBlock closing the `<details>` opened at `from`, honouring
--: nesting; nil when unbalanced.
local function find_close(blocks, from)
  local depth = 1
  for i = from + 1, #blocks do
    if tag_is(blocks[i], DETAILS_OPEN) then
      depth = depth + 1
    elseif tag_is(blocks[i], DETAILS_CLOSE) then
      depth = depth - 1
      if depth == 0 then
        return i
      end
    end
  end
  return nil
end

local convert

--: Splits a leading `<summary>...</summary>` triple off the body.
--:
--: Only a *leading* summary is consumed. `<summary>` is by spec the first
--: child of its `<details>`, and scanning the whole body would swallow a
--: nested `<details>`'s summary too -- retitling the outer block with the
--: inner one's text and leaving the inner one untitled.
local function split_summary(blocks)
  local title, start = nil, 1

  if tag_is(blocks[1], SUMMARY_OPEN) then
    if tag_is(blocks[2], SUMMARY_CLOSE) then
      --: empty `<summary></summary>`
      start = 3
    elseif blocks[2] and not is_raw_html(blocks[2])
      and tag_is(blocks[3], SUMMARY_CLOSE) then
      title = summary_text(blocks[2])
      start = 4
    end
  end

  local body = pandoc.List()
  for i = start, #blocks do
    body:insert(blocks[i])
  end
  return title, body
end

convert = function(blocks)
  local out, i = pandoc.List(), 1
  while i <= #blocks do
    local b = blocks[i]
    local close = tag_is(b, DETAILS_OPEN) and find_close(blocks, i) or nil
    if close then
      local title, body = split_summary({table.unpack(blocks, i + 1, close - 1)})
      local header = title and title ~= "" and ("#+begin_details " .. title)
        or "#+begin_details"
      local inner = pandoc.List()
      inner:insert(pandoc.RawBlock("org", header))
      inner:extend(convert(body))
      inner:insert(pandoc.RawBlock("org", "#+end_details"))
      --: The Div wrapper makes the org writer separate the block from its
      --: neighbors with blank lines; bare raw blocks are glued tightly.
      --: Same trick as org_math_env.lua. An attribute-less Div emits nothing
      --: of its own.
      out:insert(pandoc.Div(inner))
      i = close + 1
    else
      out:insert(b)
      i = i + 1
    end
  end
  return out
end

return {
  { Blocks = convert },
}
