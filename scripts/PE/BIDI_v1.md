# BiDi text (modern isolates; use Unicode escapes)

Emit **actual control codepoints** in normal output; use `\\u` escapes only in code blocks/examples. **Never** print tokens like "LRI"/"PDI".

**Isolates**
- LTR-in-RTL: `\u2066 ... \u2069`
- RTL-in-LTR: `\u2067 ... \u2069`
- Unknown direction: `\u2068 ... \u2069`

**Nudges**
- LRM (attach to LTR): `\u200e`
- RLM (attach to RTL): `\u200f`

**Shaping aids (Arabic/Persian)**
- ZWNJ: `\u200c`
- ZWJ: `\u200d`

**Rules**
- Always **balance** isolates with `\u2069`.
- Keep control chars **outside** code/math/URLs/markdown; wrap **around** them.
- Use `\u200e`/`\u200f` directly next to punctuation that visually “jumps”.
- Keep numbers/units/IDs **inside** the same isolate as their fragment.

**Mini-examples**
- RTL + English phrase: `… \u2066your English phrase\u2069\u200e …`
- LTR + Arabic term: `… \u2067المصطلح\u2069\u200f …`
- Parentheses in RTL around LTR: `… (\u2066text\u2069)\u200e …`
- LTR-in-RTL + period: `… \u2066ABC\u2069\u200e.`
- RTL-in-LTR + period: `… \u2067عربي\u2069\u200f.`
- Unknown dir fragment: `\u2068MixedStart\u2069`

**Final check**
- Parentheses face content; punctuation clings to intended fragment; code/links remain untouched.

---
