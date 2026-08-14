# Copying ChatGPT messages without destroying the LaTeX

`~/scripts/javascript/userscripts/chatgpt-copy-markdown.user.js` is a
Tampermonkey userscript that copies a ChatGPT message as Markdown by
serializing the *rendered* DOM, instead of using ChatGPT's own copy button.

It exists because ChatGPT's copy button silently destroys every equation.

## The bug

Copy a message containing math and the delimiters come back stripped:

```
\[ ... \]  ->  [ ... ]
\( ... \)  ->  ( ... )
```

Downstream this is fatal. `md2org` recognizes math through pandoc's
`tex_math_single_backslash`, and `org_math_env.lua` keys off pandoc's
`DisplayMath` node — with the backslashes gone there is no math node at all, so
equations land in org notes as prose. See `../md2org-latex/readme.md` for what
that looks like on the org side.

### What is actually happening

The losses are exactly the CommonMark backslash-escape set — a backslash before
ASCII **punctuation** — and nothing else. Destroyed: `\[` `\]` `\(` `\)`, and
also `\!`, `\,`, `\;`, `\{`, `\}`, `\%`, `\&`. Critically `\\`, the row
separator in `cases` and matrix environments, collapses to a single `\`.
Untouched: `\to`, `\frac`, `\partial`, `\operatorname`, `\text` — every
backslash followed by a *letter*.

The `\!` is the diagnostic one. An observed `\(h_l\!\to h_{l+1}\)` arrived as
`(h_l!\to h_{l+1})`. Had any stage treated that span as math, its body would be
opaque and `\!` would have survived alongside `\to`. It did not, so the region
was parsed as ordinary prose by a CommonMark parser that consumed the escapes,
then re-serialized without restoring them. It is a parse/serialize round trip,
not a raw-source copy — consistent with the same pass that rewrites ChatGPT's
internal citation tokens into `([Title][1])` reference links, which appear in
the same output.

This is a copy-path bug only. The message renders correctly on screen, so the
exact TeX is still present in the page and the recovery can be lossless.

## Why `data-math-source` and not the usual approach

Nearly every "fix ChatGPT LaTeX copying" script and extension reads the KaTeX
MathML annotation, `annotation[encoding="application/x-tex"]`. **That does not
work on ChatGPT.** Verified against the live page: `span.katex` is present 6
times in a message while `annotation[encoding="application/x-tex"]`,
`.katex-mathml` and `<math>` are present *zero* times. ChatGPT renders KaTeX in
HTML-only mode, which emits no MathML at all.

Reading the annotation therefore finds nothing, and the usual fallback — the
node's `textContent` — yields the rendered glyphs, e.g.

```
Jx(q)​=∂x∂f(x;Q(W))​
```

which is the "LaTeX turns into Unicode when copied" complaint those extensions
are known for.

What ChatGPT does emit is a wrapper element around the `.katex` node:

```html
<span role="math"
      data-math-source="J_x^{(q)}=\frac{\partial f(x;Q(W))}{\partial x}"
      aria-label="J_x^{(q)}=\frac{\partial f(x;Q(W))}{\partial x}"
      data-start="257" data-end="310">
  <span class="katex">…rendered glyphs…</span>
</span>
```

`data-math-source` is the verbatim TeX. The script prefers it, falls back to a
MathML annotation, then to `aria-label`, and if all three are missing it warns
rather than quietly emitting glyphs.

Two structural details that are easy to get wrong:

- Display math is detected by the wrapper *containing* a `.katex-display`, not
  by being inside one. `closest('.katex-display')` returns false here.
- The wrapper and the inner `.katex` both match any reasonable math selector,
  and Turndown visits children before parents. Without an outermost-node guard
  every equation is processed twice, and the inner pass — which has no source
  attribute of its own — trips the degraded-copy warning on every copy.

## Install

Tampermonkey, then open the raw file and accept the install prompt. It matches
`https://chatgpt.com/*` and `https://chat.openai.com/*`.

The script is self-contained: Turndown 7.2.0 and turndown-plugin-gfm 1.0.2 are
vendored verbatim at the bottom of the file, so there is no network dependency
and nothing to fetch at install time.

This started as two `@require` lines pinned to jsDelivr with `#sha256=`
integrity hashes, which failed in practice. Tampermonkey "refuses to load the
resource" when an integrity check does not line up, and it does so *silently* —
the script installs cleanly and then reports `TurndownService failed to load`
on the first click, with nothing indicating a rejected dependency. Vendoring
removes that failure mode along with CDN availability, and it is arguably the
better supply-chain position anyway: the exact bytes that run are in the repo
and diffable, rather than behind a hash pointing at someone else's server.

Both vendored blocks are the upstream browser builds unmodified. Each declares
a single `var`, so they land in the script's own IIFE scope rather than on any
global. To update, swap in the corresponding `dist/*.js` from the tagged
release and re-run the verification below.

## Use

Every assistant message gets two controls at the **far right** of its own action
bar, past ChatGPT's copy/rate/share/more buttons:

- The **Markdown mark** copies that message. It is deliberately not a second
  clipboard glyph — it sits next to ChatGPT's own copy button and has to be
  distinguishable at a glance.
- The **chevron** opens a menu: *Copy this message*, *Copy whole chat*,
  *Download whole chat (.md)*.

Also available:

- **Option+Shift+C** copies the message containing the caret or selection, or
  the last assistant message. Ignored while the composer has focus.
- Tampermonkey's menu carries all three actions, for when no message is in view.

Action bars are forced visible rather than appearing on hover. ChatGPT keeps
them `pointer-events-none` behind a sliding mask; both are overridden inline on
the bar element, not by matching their utility class names, which are
arbitrary-value Tailwind and change freely. Set `ALWAYS_SHOW_ACTIONS` to false
to leave the hover behaviour alone.

Button styling is cloned from the adjacent native copy button at injection time,
so ChatGPT's theme tokens and hover states apply and a restyle on their side
carries over for free. Injection is idempotent and driven by a `MutationObserver`,
because turns are re-rendered constantly by streaming, virtualization and model
switches.

A toast reports the character count and how many equations were recovered, or
warns when something degraded. It never fails silently — silent degradation is
the entire reason this script exists.

### Whole-chat export

Both whole-chat actions walk every `section[data-turn]` in order and emit the
conversation title and URL, then one section per turn. Role headings are **H1**
so that a message's own H2/H3 nest underneath rather than colliding with them;
after `md2org` that gives `* User` / `* ChatGPT` with content headings below at
`**`. User turns are included, read from `.whitespace-pre-wrap`.

## Output conventions

Math is emitted as `$…$` and `$$…$$`, not `\(…\)` / `\[…\]`. An unescaped `$`
is inert to a CommonMark round trip, so the output survives being re-copied or
passed through another Markdown tool — unlike the delimiters that failed in the
first place. Pandoc's `tex_math_dollars` is on by default in its `markdown`
reader, and `$$` produces a `DisplayMath` node, so `org_math_env.lua` still
converts display math to `\begin{equation*}` exactly as before. To change this,
edit `MATH_DELIMITERS` at the top of the script.

Code fences get their language from the `<code>` element's `language-*` class,
falling back to the header bar text. The fence is grown past the longest
backtick run in the body, so code containing a fence does not truncate the
block.

Citations become ordinary inline Markdown links. The visible pill label carries
a grouped-source badge such as `Artificial Analysis+1`, which is stripped, and
the `utm_source=chatgpt.com` tracking parameter is removed from the URL.

Everything else — headings, lists, emphasis, blockquotes, tables — is
Turndown's, with the GFM plugin enabled. Note that Turndown escapes Markdown
metacharacters in prose text, including backslashes, which is exactly the step
ChatGPT's own copy path omits.

## Verifying after a ChatGPT redesign

The script is coupled to ChatGPT's DOM and will eventually break. All the
fragile parts are in the `SELECTORS` object and the `mathSource` /
`isDisplayMath` functions at the top.

The conversion can be exercised against the live page without touching the
clipboard. The script exposes `window.__chatgptCopyMarkdown`, and `chrome-cli`
(which the zsh wrapper points at Arc by default) can drive it. Because the file
is self-contained, injecting it is just the file plus a small tail that calls
the exposed API:

```zsh
cp chatgpt-copy-markdown.user.js bundle.js
cat >> bundle.js <<'EOF'
;(function () {
  var api = window.__chatgptCopyMarkdown;
  var msgs = api.assistantMessages();
  var md = api.messageToMarkdown(msgs[msgs.length - 1]);
  return JSON.stringify({ len: md.length, unresolved: api.unresolvedMath(), md: md });
})();
EOF
chrome-cli execute "$(command cat bundle.js)"
```

`unresolved` must be 0. A non-zero count means math was found whose source
could not be read, which is the signal that `SELECTORS` needs updating.

Injecting this way adds a second set of controls to each turn; reload to clear
them.

Two constraints on driving Arc this way, both learned the hard way:

- `chrome-cli` only reaches Arc's **active** tab. Any `-t` query against a
  background tab returns empty, and Arc reassigns tab ids between invocations,
  so `chrome-cli activate -t` cannot be used to get to the tab you want. The
  tab has to already be the visible one.
- Neither `osascript -e 'tell application "Arc" to activate'` nor `open -a Arc`
  brings Arc forward here; both return success and change nothing.
  `hs -c 'hs.application.launchOrFocus("Arc")'` does work.

### Screenshotting Arc

`screencapture -l <windowid>` captures one window without activating it, which
is what you want when the terminal should keep focus. Get the id from Quartz —
Arc's main window is the largest one at layer 0:

```zsh
wid=$(python3 -c "
from Quartz import CGWindowListCopyWindowInfo, kCGWindowListOptionAll, kCGNullWindowID
best = None
for w in CGWindowListCopyWindowInfo(kCGWindowListOptionAll, kCGNullWindowID):
    if (w.get('kCGWindowOwnerName') or '') != 'Arc' or w.get('kCGWindowLayer') != 0:
        continue
    b = w.get('kCGWindowBounds', {})
    area = b.get('Width', 0) * b.get('Height', 0)
    if best is None or area > best[1]:
        best = (w.get('kCGWindowNumber'), area)
print(best[0] if best else '')
")
screencapture -x -o -l "$wid" shot.png
```

Use `kCGWindowListOptionAll`, not `...OnScreenOnly`: an occluded window is not
"on screen" and will not be listed at all.

The catch is that this captures the window's **backing store**, and a fully
occluded window does not redraw — so you get a frame from whenever it was last
visible, silently missing any DOM changes you just made. If the capture has to
reflect a fresh mutation, Arc must be frontmost at capture time:

```zsh
hs -c 'hs.application.launchOrFocus("Arc")'
osascript -e 'delay 2'
# ... inject / pose / open the menu via chrome-cli ...
screencapture -x -o -l "$wid" shot.png
hs -c 'hs.application.launchOrFocus("kitty")'   # hand focus back
```

Use `osascript -e 'delay N'` rather than `sleep` when the runner blocks
foreground sleeps.

Note that `window.__chatgptCopyMarkdown` is only reachable this way when the
code is injected into page context; under Tampermonkey's sandbox it lives on
the sandboxed window instead.

The end-to-end check that matters is the org round trip:

```zsh
md2org < live.md > live.org
```

then parse it and confirm org sees real math rather than prose:

```elisp
(with-temp-buffer
  (insert-file-contents "live.org")
  (let ((org-inhibit-startup t)) (org-mode))
  (let ((tree (org-element-parse-buffer)))
    (list (length (org-element-map tree 'latex-environment 'identity))
          (length (org-element-map tree 'latex-fragment 'identity)))))
```

On the message this was developed against, that yields 2 environments and 4
fragments — `\(Q(W)\)`, `\(W\)`, `\(J\)` and `\(h_l\!\to h_{l+1}\)` — where
ChatGPT's own copy produced none. The surviving `\!` is the specific proof that
the escape stripping is gone.

## Limitations

- ChatGPT web only. Text copied from anywhere else, or already captured in old
  notes, is not helped by this. Recovering *those* would need heuristics, and
  they are lossy: a bare `(W)` or `(J)` carries no signal distinguishing it
  from prose parentheses, and a mangled `\\` row separator is unrecoverable.
- Coupled to an undocumented DOM. `data-math-source` is not an API and can
  disappear without warning; the warning toast is what makes that visible
  rather than silent.
- Copying while a reply is still streaming is refused rather than half-done.
- ChatGPT's reference-style citation list is not reproduced; links are inlined
  instead. Pandoc handles both, and inline links survive excerpting.

## Related

- `../md2org-latex/readme.md` — the org-side failure taxonomy, including what
  broken math does to `org-element` parsing.
- `$DOOMDIR/docs/org/latex-preview/begin-env-bug.md` and
  `night/org-latex-fix-begin-env-bug` — the repair for `\[...\]` blocks that
  reached org intact but that org mis-parses for a different reason.
