// ==UserScript==
// @name         ChatGPT: copy message as clean Markdown
// @namespace    https://github.com/NightMachinery
// @version      1.0.0
// @description  Copy a ChatGPT message as Markdown with its LaTeX intact, by reading data-math-source instead of the mangled copy path.
// @author       NightMachinery
// @match        https://chatgpt.com/*
// @match        https://chat.openai.com/*
// @run-at       document-idle
// @grant        GM_setClipboard
// @grant        GM_registerMenuCommand
// @require      https://cdn.jsdelivr.net/npm/turndown@7.2.0/dist/turndown.js#sha256=ae3605eb07ab920a2d181008ace692ec560fa6cd67d2e291f77cbc5c4322cd38
// @require      https://cdn.jsdelivr.net/npm/turndown-plugin-gfm@1.0.2/dist/turndown-plugin-gfm.js#sha256=cf744cc1b7580f06d64ce236a4ff2630a53d389eccf2133a09d71ca443511912
// ==/UserScript==

//
// Why this exists
// ---------------
// ChatGPT's own copy button round-trips the message through a CommonMark
// parse/serialize that is not math-aware. The parse consumes backslash escapes
// and the serialize never restores them, so every backslash before ASCII
// punctuation is destroyed:
//
//     \[ ... \]  ->  [ ... ]        display math delimiters
//     \( ... \)  ->  ( ... )        inline math delimiters
//     \!         ->  !              and likewise \, \; \{ \} \% \&
//     \\         ->  \              matrix / cases row separators
//
// Backslashes followed by a *letter* survive, which is why `\frac` and `\to`
// look fine while the equation as a whole is unusable. Downstream, `md2org`
// then has no math to convert at all.
//
// The rendering path is unaffected, so the exact TeX is still in the page.
// This script serializes the rendered DOM instead of trusting the copy path,
// which makes the recovery lossless. It needs no API token and touches no
// private endpoint.
//
// Docs: ~/scripts/docs/chatgpt-copy-markdown/readme.md
//

/* global TurndownService, turndownPluginGfm, GM_setClipboard, GM_registerMenuCommand */

(function () {
  'use strict';

  // --------------------------------------------------------------------------
  // Configuration
  //
  // Everything ChatGPT can break in a redesign is in SELECTORS and the two
  // extractor functions below it. When a copy starts coming out wrong, that is
  // the block to repair.
  // --------------------------------------------------------------------------

  const MATH_DELIMITERS = {
    // Dollars, not \[ \] / \( \), on purpose. An unescaped `$` is inert to a
    // CommonMark round trip, so the output survives being re-copied or passed
    // through another Markdown tool -- unlike the delimiters that failed here.
    // Pandoc's `tex_math_dollars` is on by default in its `markdown` reader,
    // and `$$` yields a DisplayMath node, which is what `org_math_env.lua`
    // keys off to emit `\begin{equation*}`.
    inlineOpen: '$',
    inlineClose: '$',
    displayOpen: '$$',
    displayClose: '$$',
  };

  const SELECTORS = {
    message: 'div[data-message-id]',
    assistantMessage: 'div[data-message-author-role="assistant"]',
    roleAttribute: 'data-message-author-role',
    // `.markdown` is the rendered message body. Scoping to it excludes
    // reasoning panels, source drawers, and the action bar.
    body: ':scope .markdown, :scope .whitespace-pre-wrap',
    // Verified against the live DOM (2026-08): ChatGPT renders KaTeX in
    // HTML-only mode. There is no `.katex-mathml`, no <math>, and no
    // <annotation encoding="application/x-tex"> anywhere on the page -- the
    // usual extraction target simply does not exist. What it does emit is a
    // wrapper <span role="math" data-math-source="..." aria-label="..."> around
    // the .katex node. The other selectors are fallbacks for MathML-emitting
    // renderers, kept so this does not silently regress to glyph soup if
    // ChatGPT switches output modes.
    math: '[data-math-source], [role="math"], span.katex, mjx-container',
    mathSourceAttribute: 'data-math-source',
    mathAnnotation: 'annotation[encoding="application/x-tex"]',
    mathDisplayWrapper: '.katex-display',
    codeLanguageHeader: 'div > div:first-child',
    citation: '[data-testid="webpage-citation-pill"], [data-testid*="citation"], [data-mdx-inline-links]',
    citationLinksAttribute: 'data-mdx-inline-links',
    streaming: '[data-testid="stop-button"]',
  };

  // Option+Shift+C on macOS. Ignored while the composer has focus.
  const HOTKEY = (event) =>
    event.altKey && event.shiftKey && !event.metaKey && !event.ctrlKey &&
    (event.code === 'KeyC' || String(event.key).toLowerCase() === 'c');

  const TOAST_MS = 3200;

  // Tracking parameter ChatGPT appends to every citation it hands out.
  const STRIP_QUERY_PARAMS = ['utm_source'];

  // --------------------------------------------------------------------------
  // Math extraction
  // --------------------------------------------------------------------------

  // Ordered by trust: the explicit source attribute, then MathML annotations
  // for renderers that emit them, then the accessibility label, which ChatGPT
  // currently populates with the same TeX. Returns null when nothing usable is
  // present, so the caller can warn instead of emitting rendered glyphs.
  const mathSource = (node) => {
    const explicit = node.getAttribute(SELECTORS.mathSourceAttribute);
    if (explicit && explicit.trim()) return explicit.trim();

    const annotation = node.querySelector(SELECTORS.mathAnnotation);
    if (annotation && annotation.textContent.trim()) {
      return annotation.textContent.trim();
    }

    if (node.getAttribute('role') === 'math') {
      const label = node.getAttribute('aria-label');
      if (label && label.trim()) return label.trim();
    }

    return null;
  };

  // The math wrapper contains a .katex node that also matches SELECTORS.math,
  // so a naive querySelectorAll double-counts every equation.
  const outermostMath = (root) =>
    Array.from(root.querySelectorAll(SELECTORS.math)).filter(
      (node) => !(node.parentElement && node.parentElement.closest(SELECTORS.math))
    );

  // ChatGPT nests .katex-display *inside* the wrapper, so the usual
  // `closest('.katex-display')` test returns false here; check both directions.
  const isDisplayMath = (node) =>
    !!node.querySelector(SELECTORS.mathDisplayWrapper) ||
    !!node.closest(SELECTORS.mathDisplayWrapper) ||
    node.getAttribute('display') === 'block' ||
    !!node.querySelector('math[display="block"]');

  // --------------------------------------------------------------------------
  // Small helpers
  // --------------------------------------------------------------------------

  const resolveGlobal = (name) =>
    (typeof window !== 'undefined' && window[name]) ||
    (typeof globalThis !== 'undefined' && globalThis[name]) ||
    null;

  const trimTrailingNewline = (text) => text.replace(/\n+$/, '');

  // A fence has to be longer than the longest backtick run it contains, or code
  // that itself contains a fence silently truncates the block.
  const fenceFor = (code) => {
    let longest = 0;
    for (const run of code.match(/`+/g) || []) {
      longest = Math.max(longest, run.length);
    }
    return '`'.repeat(Math.max(3, longest + 1));
  };

  const cleanUrl = (raw) => {
    try {
      const url = new URL(raw, location.href);
      for (const param of STRIP_QUERY_PARAMS) url.searchParams.delete(param);
      return url.toString();
    } catch (_error) {
      return raw;
    }
  };

  const copyToClipboard = (text) => {
    if (typeof GM_setClipboard === 'function') {
      // Focus-independent, unlike navigator.clipboard, which throws
      // "Document is not focused" when called from an injected handler.
      GM_setClipboard(text, 'text');
      return Promise.resolve();
    }
    return navigator.clipboard.writeText(text);
  };

  // --------------------------------------------------------------------------
  // Toasts
  //
  // Silent degradation is the failure mode this script exists to fix, so it
  // must never fail quietly itself.
  // --------------------------------------------------------------------------

  let toastElement = null;
  let toastTimer = null;

  const toast = (message, kind) => {
    if (!toastElement) {
      toastElement = document.createElement('div');
      Object.assign(toastElement.style, {
        position: 'fixed',
        right: '16px',
        bottom: '76px',
        zIndex: '2147483647',
        maxWidth: '380px',
        padding: '10px 14px',
        borderRadius: '8px',
        font: '13px/1.45 ui-sans-serif, system-ui, sans-serif',
        color: '#fff',
        boxShadow: '0 4px 16px rgba(0,0,0,.28)',
        whiteSpace: 'pre-wrap',
        pointerEvents: 'none',
        transition: 'opacity .15s ease',
      });
      document.body.appendChild(toastElement);
    }
    toastElement.style.background =
      kind === 'error' ? '#b3261e' : kind === 'warn' ? '#8a6100' : '#1f6f43';
    toastElement.textContent = message;
    toastElement.style.opacity = '1';
    clearTimeout(toastTimer);
    toastTimer = setTimeout(() => {
      toastElement.style.opacity = '0';
    }, TOAST_MS);
  };

  // --------------------------------------------------------------------------
  // Citations
  // --------------------------------------------------------------------------

  const readCitationLinks = (node) => {
    const seen = new Set();
    const links = [];

    const push = (rawUrl, rawLabel) => {
      if (!rawUrl) return;
      const url = cleanUrl(rawUrl);
      if (seen.has(url)) return;
      seen.add(url);

      // Pill labels carry a grouped-source badge, e.g. "Artificial Analysis+1".
      let label = String(rawLabel || '').trim().replace(/\+\d+$/, '').trim();
      if (!label) {
        try {
          label = new URL(url).hostname.replace(/^www\./, '');
        } catch (_error) {
          label = url;
        }
      }
      links.push({ url, label: label.replace(/[[\]]/g, '') });
    };

    const raw = node.getAttribute(SELECTORS.citationLinksAttribute);
    if (raw) {
      let parsed = null;
      try {
        parsed = JSON.parse(raw);
      } catch (_error) {
        parsed = null;
      }
      for (const entry of Array.isArray(parsed) ? parsed : []) {
        if (typeof entry === 'string') {
          push(entry, '');
        } else if (entry && typeof entry === 'object') {
          push(entry.url || entry.href, entry.title || entry.text || entry.attribution);
        }
      }
    }

    for (const anchor of node.querySelectorAll('a[href]')) {
      push(anchor.getAttribute('href'), anchor.textContent);
    }

    return links;
  };

  // --------------------------------------------------------------------------
  // Turndown
  // --------------------------------------------------------------------------

  let turndown = null;
  let unresolvedMath = 0;

  const buildTurndown = () => {
    const Turndown = resolveGlobal('TurndownService');
    if (!Turndown) {
      throw new Error('TurndownService failed to load (check the @require lines)');
    }

    const service = new Turndown({
      headingStyle: 'atx',
      hr: '---',
      bulletListMarker: '-',
      codeBlockStyle: 'fenced',
      emDelimiter: '*',
      strongDelimiter: '**',
      linkStyle: 'inlined',
    });

    const gfm = resolveGlobal('turndownPluginGfm');
    if (gfm && gfm.gfm) service.use(gfm.gfm);

    // Math.
    //
    // This MUST be a rule rather than a DOM substitution. Turndown escapes text
    // nodes -- including `\` -> `\\` -- so injecting `$$\frac{a}{b}$$` as text
    // would emit `\\frac`, reintroducing the exact bug being fixed here. A
    // rule's return value is inserted verbatim.
    //
    // Replacing the whole wrapper also means the aria-hidden `.katex-html`
    // glyph soup is never visited, so `\frac{a}{b}` cannot come out as the
    // rendered characters instead.
    service.addRule('chatgptMath', {
      // Only the outermost math node. ChatGPT nests .katex inside the
      // [data-math-source] wrapper, and Turndown visits children before
      // parents, so without this guard the inner node matches too, finds no
      // source on itself, and trips the degraded-copy warning on every copy.
      filter: (node) =>
        node.nodeType === 1 && node.matches && node.matches(SELECTORS.math) &&
        !(node.parentElement && node.parentElement.closest(SELECTORS.math)),
      replacement: (content, node) => {
        const tex = mathSource(node);
        if (!tex) {
          // Rendered glyphs are worse than useless downstream, but dropping the
          // math silently is worse still. Keep the text and count it so the
          // toast can report that this copy is degraded.
          unresolvedMath += 1;
          return content || node.textContent;
        }
        if (isDisplayMath(node)) {
          return (
            '\n\n' + MATH_DELIMITERS.displayOpen + '\n' +
            tex + '\n' + MATH_DELIMITERS.displayClose + '\n\n'
          );
        }
        return MATH_DELIMITERS.inlineOpen + tex + MATH_DELIMITERS.inlineClose;
      },
    });

    // Code blocks.
    //
    // ChatGPT's <pre> carries a header bar with the language name and the
    // Copy/Edit buttons. Turndown's default pre>code handling would serialize
    // that chrome into the fence, so take the <code> text explicitly.
    service.addRule('chatgptCodeBlock', {
      filter: (node) => node.nodeName === 'PRE',
      replacement: (_content, node) => {
        const codeNode = node.querySelector('code');
        const code = trimTrailingNewline(
          (codeNode ? codeNode.textContent : node.textContent) || ''
        );

        let language = '';
        const className = (codeNode && codeNode.getAttribute('class')) || '';
        const classMatch = /language-([\w+#.-]+)/.exec(className);
        if (classMatch) {
          language = classMatch[1];
        } else {
          const header = node.querySelector(SELECTORS.codeLanguageHeader);
          const headerText = ((header && header.textContent) || '').trim();
          // The header is a bare language name; anything longer is UI chrome.
          if (/^[\w+#.-]{1,20}$/.test(headerText)) language = headerText.toLowerCase();
        }

        const fence = fenceFor(code);
        return '\n\n' + fence + language + '\n' + code + '\n' + fence + '\n\n';
      },
    });

    // Citation pills.
    //
    // The visible label is the site name plus a grouped-source badge; the real
    // URL is on the inner anchor. Emitting a Markdown link keeps the citation
    // usable after the text leaves the page.
    service.addRule('citationPill', {
      filter: (node) =>
        node.nodeType === 1 && node.matches && node.matches(SELECTORS.citation),
      replacement: (content, node) => {
        const links = readCitationLinks(node);
        if (!links.length) return content;
        // No leading space: the pill is spaced by a CSS margin, but Turndown
        // still emits the separating whitespace from the surrounding text, so
        // adding one here doubles it.
        return '(' + links
          .map((link) => '[' + link.label + '](' + link.url + ')')
          .join(', ') + ')';
      },
    });

    return service;
  };

  // --------------------------------------------------------------------------
  // Message -> Markdown
  // --------------------------------------------------------------------------

  const messageBody = (message) => message.querySelector(SELECTORS.body) || message;

  const messageToMarkdown = (message) => {
    if (!turndown) turndown = buildTurndown();
    unresolvedMath = 0;
    // Convert a detached clone so nothing mutates the live page.
    const clone = messageBody(message).cloneNode(true);
    const markdown = turndown.turndown(clone);
    return markdown.replace(/\n{3,}/g, '\n\n').trim();
  };

  const assistantMessages = () =>
    Array.from(document.querySelectorAll(SELECTORS.assistantMessage));

  // Prefer the message the caret or selection is in, so older messages are
  // reachable without injecting a button into ChatGPT's shifting markup.
  const targetMessage = () => {
    const selection = window.getSelection();
    if (selection && selection.anchorNode) {
      const anchor =
        selection.anchorNode.nodeType === 1
          ? selection.anchorNode
          : selection.anchorNode.parentElement;
      const owner = anchor && anchor.closest && anchor.closest(SELECTORS.message);
      if (owner && owner.getAttribute(SELECTORS.roleAttribute) === 'assistant') {
        return owner;
      }
    }
    const all = assistantMessages();
    return all.length ? all[all.length - 1] : null;
  };

  const copyMessage = async () => {
    const message = targetMessage();
    if (!message) {
      toast('No assistant message found on this page.', 'error');
      return;
    }

    if (document.querySelector(SELECTORS.streaming)) {
      toast('Still generating — copy again once the reply finishes.', 'warn');
      return;
    }

    let markdown;
    try {
      markdown = messageToMarkdown(message);
    } catch (error) {
      toast('Conversion failed: ' + error.message, 'error');
      return;
    }

    if (!markdown) {
      toast('That message converted to nothing — the selectors are probably stale.', 'error');
      return;
    }

    try {
      await copyToClipboard(markdown);
    } catch (error) {
      toast('Clipboard write failed: ' + error.message, 'error');
      return;
    }

    const mathTotal = outermostMath(messageBody(message)).length;

    if (unresolvedMath) {
      toast(
        'Copied ' + markdown.length + ' chars, but ' + unresolvedMath + ' of ' +
        mathTotal + ' math nodes had no readable source and fell back to rendered ' +
        'text. Check SELECTORS.mathSourceAttribute — ChatGPT may have changed it.',
        'warn'
      );
      return;
    }

    toast(
      'Copied ' + markdown.length + ' chars' +
      (mathTotal ? ' (' + mathTotal + ' math node' + (mathTotal === 1 ? '' : 's') + ')' : '') +
      '.'
    );
  };

  // --------------------------------------------------------------------------
  // Entry points
  // --------------------------------------------------------------------------

  const isEditingContext = (node) => {
    if (!node || node.nodeType !== 1) return false;
    if (node.isContentEditable) return true;
    const name = node.nodeName;
    return name === 'INPUT' || name === 'TEXTAREA';
  };

  document.addEventListener(
    'keydown',
    (event) => {
      if (!HOTKEY(event)) return;
      if (isEditingContext(event.target)) return;
      event.preventDefault();
      event.stopPropagation();
      copyMessage();
    },
    true
  );

  const addButton = () => {
    const button = document.createElement('button');
    button.type = 'button';
    button.textContent = 'MD';
    button.title = 'Copy this (or the last) assistant message as clean Markdown';
    Object.assign(button.style, {
      position: 'fixed',
      right: '16px',
      bottom: '16px',
      zIndex: '2147483646',
      width: '38px',
      height: '38px',
      borderRadius: '19px',
      border: '1px solid rgba(127,127,127,.45)',
      background: 'rgba(127,127,127,.16)',
      color: 'inherit',
      font: '600 12px/1 ui-sans-serif, system-ui, sans-serif',
      cursor: 'pointer',
      backdropFilter: 'blur(6px)',
    });
    button.addEventListener('click', (event) => {
      event.preventDefault();
      copyMessage();
    });
    document.body.appendChild(button);
  };

  addButton();

  if (typeof GM_registerMenuCommand === 'function') {
    GM_registerMenuCommand('Copy message as clean Markdown', copyMessage);
  }

  // Exposed so the conversion can be exercised without the clipboard, which is
  // how the selectors get re-verified against the live page after a ChatGPT
  // redesign. See the verification section of the docs.
  window.__chatgptCopyMarkdown = {
    messageToMarkdown,
    targetMessage,
    assistantMessages,
    copyMessage,
    unresolvedMath: () => unresolvedMath,
    SELECTORS,
  };
})();
