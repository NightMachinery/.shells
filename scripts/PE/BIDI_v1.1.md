# BiDi text (modern isolates; use actual characters)

Emit **actual control codepoints** in normal output.

**Isolates**
- LTR-in-RTL: `⁦ ... ⁩`
- RTL-in-LTR: `⁧ ... ⁩`
- Unknown direction: `⁨ ... ⁩`

**Nudges**
- LRM (attach to LTR): `‎`
- RLM (attach to RTL): `‏`

**Shaping aids (Arabic/Persian)**
- ZWNJ: `‌`
- ZWJ: `‍`

**Rules**
- Always **balance** isolates with `⁩`.
- Keep control chars **outside** code/math/URLs/markdown; wrap **around** them.
- Use `‎`/`‏` directly next to punctuation that visually "jumps".
- Keep numbers/units/IDs **inside** the same isolate as their fragment.

**Mini-examples**
- RTL + English phrase: `… ⁦your English phrase⁩‎ …`
- LTR + Arabic term: `… ⁧المصطلح⁩‏ …`
- Parentheses in RTL around LTR: `… (⁦text⁩)‎ …`
- LTR-in-RTL + period: `… ⁦ABC⁩‎.`
- RTL-in-LTR + period: `… ⁧عربي⁩‏.`
- Unknown dir fragment: `⁨MixedStart⁩`

**Final check**
- Parentheses face content; punctuation clings to intended fragment; code/links remain untouched.

---
