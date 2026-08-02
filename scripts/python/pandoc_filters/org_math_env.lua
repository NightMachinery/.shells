--: Converts standalone display math to explicit `\begin{equation*}`
--: environments, preserving interior newlines. Org parses these line-wise
--: as latex-environment elements, so they are immune to the lone-operator
--: plain-list bug that breaks multi-line `\[...\]` fragments.
--:
--: Display math that cannot become a block is reflowed onto one line,
--: where fragments are safe: inside table cells (pass 1), mixed into a
--: text paragraph (pass 2), or in inline-only contexts such as headings
--: (pass 3). Inline math is always reflowed (pass 3).

local function reflow(s)
  return (s:gsub('%s*\n%s*', ' '))
end

local function trim(s)
  return (s:gsub('^%s*', ''):gsub('%s*$', ''))
end

local reflow_display = {
  Math = function(m)
    if m.mathtype == 'DisplayMath' then
      m.text = reflow(trim(m.text))
      return m
    end
  end,
}

local function blockify(para)
  --: If PARA is display math standing alone, return an explicit
  --: environment block; if it mixes math and text, reflow the math.
  local display = nil
  for _, item in ipairs(para.content) do
    if item.t == 'Math' and item.mathtype == 'DisplayMath' and not display then
      display = item
    elseif item.t ~= 'Space' and item.t ~= 'SoftBreak' then
      return para:walk(reflow_display)
    end
  end
  if display then
    --: the Div wrapper makes the org writer separate the raw block from
    --: its neighbors with blank lines (bare raw blocks are glued tightly)
    return pandoc.Div(pandoc.RawBlock('org',
      '\\begin{equation*}\n' .. trim(display.text) .. '\n\\end{equation*}'))
  end
end

return {
  { --: pass 1: tables cannot hold multi-line blocks in cells, so cell
    --: display math becomes a single-line raw fragment that pass 2 will
    --: not blockify
    Table = function(tbl)
      return tbl:walk({
        Math = function(m)
          if m.mathtype == 'DisplayMath' then
            return pandoc.RawInline('org', '\\[ ' .. reflow(trim(m.text)) .. ' \\]')
          end
        end,
      })
    end,
  },
  { --: pass 2: standalone display-math paragraphs become environments
    Para = blockify,
    Plain = blockify,
  },
  { --: pass 3: whatever display math is left sits in an inline-only
    --: context (headings, emphasis, ...); reflow it, and inline math too
    Math = function(m)
      m.text = reflow(trim(m.text))
      return m
    end,
  },
}
