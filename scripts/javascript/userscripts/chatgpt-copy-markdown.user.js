// ==UserScript==
// @name         ChatGPT: copy message as clean Markdown
// @namespace    https://github.com/NightMachinery
// @version      1.1.0
// @description  Copy a ChatGPT message as Markdown with its LaTeX intact, by reading data-math-source instead of the mangled copy path.
// @author       NightMachinery
// @match        https://chatgpt.com/*
// @match        https://chat.openai.com/*
// @run-at       document-idle
// @grant        GM_setClipboard
// @grant        GM_registerMenuCommand
// ==/UserScript==

// Turndown 7.2.0 and turndown-plugin-gfm 1.0.2 are vendored verbatim at the
// bottom of this file rather than pulled in with @require. Two reasons: a
// @require carrying a `#sha256=` integrity hash is refused *silently* by
// Tampermonkey when anything about the hash or the response does not line up,
// which presents as a bare "TurndownService failed to load" at click time; and
// vendoring makes the script self-contained, offline, and auditable in-repo,
// which is a better supply-chain position than a hash pointing at a CDN.

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
    // Vendored at the bottom of this file, so this can only fail if the file
    // was truncated or edited.
    if (typeof TurndownService === 'undefined') {
      throw new Error('TurndownService missing — the vendored library block is gone');
    }

    const service = new TurndownService({
      headingStyle: 'atx',
      hr: '---',
      bulletListMarker: '-',
      codeBlockStyle: 'fenced',
      emDelimiter: '*',
      strongDelimiter: '**',
      linkStyle: 'inlined',
    });

    if (typeof turndownPluginGfm !== 'undefined' && turndownPluginGfm.gfm) {
      service.use(turndownPluginGfm.gfm);
    }

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

  // Built with createElementNS rather than innerHTML: chatgpt.com may enforce
  // Trusted Types, under which innerHTML is a guarded sink and throws.
  const copyIcon = () => {
    const ns = 'http://www.w3.org/2000/svg';
    const svg = document.createElementNS(ns, 'svg');
    svg.setAttribute('viewBox', '0 0 24 24');
    svg.setAttribute('width', '17');
    svg.setAttribute('height', '17');
    svg.setAttribute('fill', 'none');
    svg.setAttribute('stroke', 'currentColor');
    svg.setAttribute('stroke-width', '2');
    svg.setAttribute('stroke-linecap', 'round');
    svg.setAttribute('stroke-linejoin', 'round');

    const back = document.createElementNS(ns, 'rect');
    back.setAttribute('x', '9');
    back.setAttribute('y', '9');
    back.setAttribute('width', '13');
    back.setAttribute('height', '13');
    back.setAttribute('rx', '2');
    svg.appendChild(back);

    const front = document.createElementNS(ns, 'path');
    front.setAttribute('d', 'M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1');
    svg.appendChild(front);

    return svg;
  };

  const addButton = () => {
    const button = document.createElement('button');
    button.type = 'button';
    button.appendChild(copyIcon());
    button.setAttribute('aria-label', 'Copy message as clean Markdown');
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
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      padding: '0',
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

  // ==========================================================================
  // Vendored dependencies. Nothing below this line is ours -- both files are
  // the upstream browser builds, byte for byte, kept here so the script has no
  // network dependency and no silent @require failure mode.
  //
  //   turndown 7.2.0            MIT   https://github.com/mixmark-io/turndown
  //   turndown-plugin-gfm 1.0.2 MIT   https://github.com/mixmark-io/turndown-plugin-gfm
  //
  // Both declare a single `var`, so they land in this IIFE's scope rather than
  // on any global. To update, replace each block with the corresponding
  // dist/*.js from the tagged release and re-run the verification in
  // ~/scripts/docs/chatgpt-copy-markdown/readme.md.
  // ==========================================================================

var TurndownService = (function () {
  'use strict';

  function extend (destination) {
    for (var i = 1; i < arguments.length; i++) {
      var source = arguments[i];
      for (var key in source) {
        if (source.hasOwnProperty(key)) destination[key] = source[key];
      }
    }
    return destination
  }

  function repeat (character, count) {
    return Array(count + 1).join(character)
  }

  function trimLeadingNewlines (string) {
    return string.replace(/^\n*/, '')
  }

  function trimTrailingNewlines (string) {
    // avoid match-at-end regexp bottleneck, see #370
    var indexEnd = string.length;
    while (indexEnd > 0 && string[indexEnd - 1] === '\n') indexEnd--;
    return string.substring(0, indexEnd)
  }

  var blockElements = [
    'ADDRESS', 'ARTICLE', 'ASIDE', 'AUDIO', 'BLOCKQUOTE', 'BODY', 'CANVAS',
    'CENTER', 'DD', 'DIR', 'DIV', 'DL', 'DT', 'FIELDSET', 'FIGCAPTION', 'FIGURE',
    'FOOTER', 'FORM', 'FRAMESET', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'HEADER',
    'HGROUP', 'HR', 'HTML', 'ISINDEX', 'LI', 'MAIN', 'MENU', 'NAV', 'NOFRAMES',
    'NOSCRIPT', 'OL', 'OUTPUT', 'P', 'PRE', 'SECTION', 'TABLE', 'TBODY', 'TD',
    'TFOOT', 'TH', 'THEAD', 'TR', 'UL'
  ];

  function isBlock (node) {
    return is(node, blockElements)
  }

  var voidElements = [
    'AREA', 'BASE', 'BR', 'COL', 'COMMAND', 'EMBED', 'HR', 'IMG', 'INPUT',
    'KEYGEN', 'LINK', 'META', 'PARAM', 'SOURCE', 'TRACK', 'WBR'
  ];

  function isVoid (node) {
    return is(node, voidElements)
  }

  function hasVoid (node) {
    return has(node, voidElements)
  }

  var meaningfulWhenBlankElements = [
    'A', 'TABLE', 'THEAD', 'TBODY', 'TFOOT', 'TH', 'TD', 'IFRAME', 'SCRIPT',
    'AUDIO', 'VIDEO'
  ];

  function isMeaningfulWhenBlank (node) {
    return is(node, meaningfulWhenBlankElements)
  }

  function hasMeaningfulWhenBlank (node) {
    return has(node, meaningfulWhenBlankElements)
  }

  function is (node, tagNames) {
    return tagNames.indexOf(node.nodeName) >= 0
  }

  function has (node, tagNames) {
    return (
      node.getElementsByTagName &&
      tagNames.some(function (tagName) {
        return node.getElementsByTagName(tagName).length
      })
    )
  }

  var rules = {};

  rules.paragraph = {
    filter: 'p',

    replacement: function (content) {
      return '\n\n' + content + '\n\n'
    }
  };

  rules.lineBreak = {
    filter: 'br',

    replacement: function (content, node, options) {
      return options.br + '\n'
    }
  };

  rules.heading = {
    filter: ['h1', 'h2', 'h3', 'h4', 'h5', 'h6'],

    replacement: function (content, node, options) {
      var hLevel = Number(node.nodeName.charAt(1));

      if (options.headingStyle === 'setext' && hLevel < 3) {
        var underline = repeat((hLevel === 1 ? '=' : '-'), content.length);
        return (
          '\n\n' + content + '\n' + underline + '\n\n'
        )
      } else {
        return '\n\n' + repeat('#', hLevel) + ' ' + content + '\n\n'
      }
    }
  };

  rules.blockquote = {
    filter: 'blockquote',

    replacement: function (content) {
      content = content.replace(/^\n+|\n+$/g, '');
      content = content.replace(/^/gm, '> ');
      return '\n\n' + content + '\n\n'
    }
  };

  rules.list = {
    filter: ['ul', 'ol'],

    replacement: function (content, node) {
      var parent = node.parentNode;
      if (parent.nodeName === 'LI' && parent.lastElementChild === node) {
        return '\n' + content
      } else {
        return '\n\n' + content + '\n\n'
      }
    }
  };

  rules.listItem = {
    filter: 'li',

    replacement: function (content, node, options) {
      content = content
        .replace(/^\n+/, '') // remove leading newlines
        .replace(/\n+$/, '\n') // replace trailing newlines with just a single one
        .replace(/\n/gm, '\n    '); // indent
      var prefix = options.bulletListMarker + '   ';
      var parent = node.parentNode;
      if (parent.nodeName === 'OL') {
        var start = parent.getAttribute('start');
        var index = Array.prototype.indexOf.call(parent.children, node);
        prefix = (start ? Number(start) + index : index + 1) + '.  ';
      }
      return (
        prefix + content + (node.nextSibling && !/\n$/.test(content) ? '\n' : '')
      )
    }
  };

  rules.indentedCodeBlock = {
    filter: function (node, options) {
      return (
        options.codeBlockStyle === 'indented' &&
        node.nodeName === 'PRE' &&
        node.firstChild &&
        node.firstChild.nodeName === 'CODE'
      )
    },

    replacement: function (content, node, options) {
      return (
        '\n\n    ' +
        node.firstChild.textContent.replace(/\n/g, '\n    ') +
        '\n\n'
      )
    }
  };

  rules.fencedCodeBlock = {
    filter: function (node, options) {
      return (
        options.codeBlockStyle === 'fenced' &&
        node.nodeName === 'PRE' &&
        node.firstChild &&
        node.firstChild.nodeName === 'CODE'
      )
    },

    replacement: function (content, node, options) {
      var className = node.firstChild.getAttribute('class') || '';
      var language = (className.match(/language-(\S+)/) || [null, ''])[1];
      var code = node.firstChild.textContent;

      var fenceChar = options.fence.charAt(0);
      var fenceSize = 3;
      var fenceInCodeRegex = new RegExp('^' + fenceChar + '{3,}', 'gm');

      var match;
      while ((match = fenceInCodeRegex.exec(code))) {
        if (match[0].length >= fenceSize) {
          fenceSize = match[0].length + 1;
        }
      }

      var fence = repeat(fenceChar, fenceSize);

      return (
        '\n\n' + fence + language + '\n' +
        code.replace(/\n$/, '') +
        '\n' + fence + '\n\n'
      )
    }
  };

  rules.horizontalRule = {
    filter: 'hr',

    replacement: function (content, node, options) {
      return '\n\n' + options.hr + '\n\n'
    }
  };

  rules.inlineLink = {
    filter: function (node, options) {
      return (
        options.linkStyle === 'inlined' &&
        node.nodeName === 'A' &&
        node.getAttribute('href')
      )
    },

    replacement: function (content, node) {
      var href = node.getAttribute('href');
      if (href) href = href.replace(/([()])/g, '\\$1');
      var title = cleanAttribute(node.getAttribute('title'));
      if (title) title = ' "' + title.replace(/"/g, '\\"') + '"';
      return '[' + content + '](' + href + title + ')'
    }
  };

  rules.referenceLink = {
    filter: function (node, options) {
      return (
        options.linkStyle === 'referenced' &&
        node.nodeName === 'A' &&
        node.getAttribute('href')
      )
    },

    replacement: function (content, node, options) {
      var href = node.getAttribute('href');
      var title = cleanAttribute(node.getAttribute('title'));
      if (title) title = ' "' + title + '"';
      var replacement;
      var reference;

      switch (options.linkReferenceStyle) {
        case 'collapsed':
          replacement = '[' + content + '][]';
          reference = '[' + content + ']: ' + href + title;
          break
        case 'shortcut':
          replacement = '[' + content + ']';
          reference = '[' + content + ']: ' + href + title;
          break
        default:
          var id = this.references.length + 1;
          replacement = '[' + content + '][' + id + ']';
          reference = '[' + id + ']: ' + href + title;
      }

      this.references.push(reference);
      return replacement
    },

    references: [],

    append: function (options) {
      var references = '';
      if (this.references.length) {
        references = '\n\n' + this.references.join('\n') + '\n\n';
        this.references = []; // Reset references
      }
      return references
    }
  };

  rules.emphasis = {
    filter: ['em', 'i'],

    replacement: function (content, node, options) {
      if (!content.trim()) return ''
      return options.emDelimiter + content + options.emDelimiter
    }
  };

  rules.strong = {
    filter: ['strong', 'b'],

    replacement: function (content, node, options) {
      if (!content.trim()) return ''
      return options.strongDelimiter + content + options.strongDelimiter
    }
  };

  rules.code = {
    filter: function (node) {
      var hasSiblings = node.previousSibling || node.nextSibling;
      var isCodeBlock = node.parentNode.nodeName === 'PRE' && !hasSiblings;

      return node.nodeName === 'CODE' && !isCodeBlock
    },

    replacement: function (content) {
      if (!content) return ''
      content = content.replace(/\r?\n|\r/g, ' ');

      var extraSpace = /^`|^ .*?[^ ].* $|`$/.test(content) ? ' ' : '';
      var delimiter = '`';
      var matches = content.match(/`+/gm) || [];
      while (matches.indexOf(delimiter) !== -1) delimiter = delimiter + '`';

      return delimiter + extraSpace + content + extraSpace + delimiter
    }
  };

  rules.image = {
    filter: 'img',

    replacement: function (content, node) {
      var alt = cleanAttribute(node.getAttribute('alt'));
      var src = node.getAttribute('src') || '';
      var title = cleanAttribute(node.getAttribute('title'));
      var titlePart = title ? ' "' + title + '"' : '';
      return src ? '![' + alt + ']' + '(' + src + titlePart + ')' : ''
    }
  };

  function cleanAttribute (attribute) {
    return attribute ? attribute.replace(/(\n+\s*)+/g, '\n') : ''
  }

  /**
   * Manages a collection of rules used to convert HTML to Markdown
   */

  function Rules (options) {
    this.options = options;
    this._keep = [];
    this._remove = [];

    this.blankRule = {
      replacement: options.blankReplacement
    };

    this.keepReplacement = options.keepReplacement;

    this.defaultRule = {
      replacement: options.defaultReplacement
    };

    this.array = [];
    for (var key in options.rules) this.array.push(options.rules[key]);
  }

  Rules.prototype = {
    add: function (key, rule) {
      this.array.unshift(rule);
    },

    keep: function (filter) {
      this._keep.unshift({
        filter: filter,
        replacement: this.keepReplacement
      });
    },

    remove: function (filter) {
      this._remove.unshift({
        filter: filter,
        replacement: function () {
          return ''
        }
      });
    },

    forNode: function (node) {
      if (node.isBlank) return this.blankRule
      var rule;

      if ((rule = findRule(this.array, node, this.options))) return rule
      if ((rule = findRule(this._keep, node, this.options))) return rule
      if ((rule = findRule(this._remove, node, this.options))) return rule

      return this.defaultRule
    },

    forEach: function (fn) {
      for (var i = 0; i < this.array.length; i++) fn(this.array[i], i);
    }
  };

  function findRule (rules, node, options) {
    for (var i = 0; i < rules.length; i++) {
      var rule = rules[i];
      if (filterValue(rule, node, options)) return rule
    }
    return void 0
  }

  function filterValue (rule, node, options) {
    var filter = rule.filter;
    if (typeof filter === 'string') {
      if (filter === node.nodeName.toLowerCase()) return true
    } else if (Array.isArray(filter)) {
      if (filter.indexOf(node.nodeName.toLowerCase()) > -1) return true
    } else if (typeof filter === 'function') {
      if (filter.call(rule, node, options)) return true
    } else {
      throw new TypeError('`filter` needs to be a string, array, or function')
    }
  }

  /**
   * The collapseWhitespace function is adapted from collapse-whitespace
   * by Luc Thevenard.
   *
   * The MIT License (MIT)
   *
   * Copyright (c) 2014 Luc Thevenard <lucthevenard@gmail.com>
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy
   * of this software and associated documentation files (the "Software"), to deal
   * in the Software without restriction, including without limitation the rights
   * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   * copies of the Software, and to permit persons to whom the Software is
   * furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in
   * all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   * THE SOFTWARE.
   */

  /**
   * collapseWhitespace(options) removes extraneous whitespace from an the given element.
   *
   * @param {Object} options
   */
  function collapseWhitespace (options) {
    var element = options.element;
    var isBlock = options.isBlock;
    var isVoid = options.isVoid;
    var isPre = options.isPre || function (node) {
      return node.nodeName === 'PRE'
    };

    if (!element.firstChild || isPre(element)) return

    var prevText = null;
    var keepLeadingWs = false;

    var prev = null;
    var node = next(prev, element, isPre);

    while (node !== element) {
      if (node.nodeType === 3 || node.nodeType === 4) { // Node.TEXT_NODE or Node.CDATA_SECTION_NODE
        var text = node.data.replace(/[ \r\n\t]+/g, ' ');

        if ((!prevText || / $/.test(prevText.data)) &&
            !keepLeadingWs && text[0] === ' ') {
          text = text.substr(1);
        }

        // `text` might be empty at this point.
        if (!text) {
          node = remove(node);
          continue
        }

        node.data = text;

        prevText = node;
      } else if (node.nodeType === 1) { // Node.ELEMENT_NODE
        if (isBlock(node) || node.nodeName === 'BR') {
          if (prevText) {
            prevText.data = prevText.data.replace(/ $/, '');
          }

          prevText = null;
          keepLeadingWs = false;
        } else if (isVoid(node) || isPre(node)) {
          // Avoid trimming space around non-block, non-BR void elements and inline PRE.
          prevText = null;
          keepLeadingWs = true;
        } else if (prevText) {
          // Drop protection if set previously.
          keepLeadingWs = false;
        }
      } else {
        node = remove(node);
        continue
      }

      var nextNode = next(prev, node, isPre);
      prev = node;
      node = nextNode;
    }

    if (prevText) {
      prevText.data = prevText.data.replace(/ $/, '');
      if (!prevText.data) {
        remove(prevText);
      }
    }
  }

  /**
   * remove(node) removes the given node from the DOM and returns the
   * next node in the sequence.
   *
   * @param {Node} node
   * @return {Node} node
   */
  function remove (node) {
    var next = node.nextSibling || node.parentNode;

    node.parentNode.removeChild(node);

    return next
  }

  /**
   * next(prev, current, isPre) returns the next node in the sequence, given the
   * current and previous nodes.
   *
   * @param {Node} prev
   * @param {Node} current
   * @param {Function} isPre
   * @return {Node}
   */
  function next (prev, current, isPre) {
    if ((prev && prev.parentNode === current) || isPre(current)) {
      return current.nextSibling || current.parentNode
    }

    return current.firstChild || current.nextSibling || current.parentNode
  }

  /*
   * Set up window for Node.js
   */

  var root = (typeof window !== 'undefined' ? window : {});

  /*
   * Parsing HTML strings
   */

  function canParseHTMLNatively () {
    var Parser = root.DOMParser;
    var canParse = false;

    // Adapted from https://gist.github.com/1129031
    // Firefox/Opera/IE throw errors on unsupported types
    try {
      // WebKit returns null on unsupported types
      if (new Parser().parseFromString('', 'text/html')) {
        canParse = true;
      }
    } catch (e) {}

    return canParse
  }

  function createHTMLParser () {
    var Parser = function () {};

    {
      if (shouldUseActiveX()) {
        Parser.prototype.parseFromString = function (string) {
          var doc = new window.ActiveXObject('htmlfile');
          doc.designMode = 'on'; // disable on-page scripts
          doc.open();
          doc.write(string);
          doc.close();
          return doc
        };
      } else {
        Parser.prototype.parseFromString = function (string) {
          var doc = document.implementation.createHTMLDocument('');
          doc.open();
          doc.write(string);
          doc.close();
          return doc
        };
      }
    }
    return Parser
  }

  function shouldUseActiveX () {
    var useActiveX = false;
    try {
      document.implementation.createHTMLDocument('').open();
    } catch (e) {
      if (root.ActiveXObject) useActiveX = true;
    }
    return useActiveX
  }

  var HTMLParser = canParseHTMLNatively() ? root.DOMParser : createHTMLParser();

  function RootNode (input, options) {
    var root;
    if (typeof input === 'string') {
      var doc = htmlParser().parseFromString(
        // DOM parsers arrange elements in the <head> and <body>.
        // Wrapping in a custom element ensures elements are reliably arranged in
        // a single element.
        '<x-turndown id="turndown-root">' + input + '</x-turndown>',
        'text/html'
      );
      root = doc.getElementById('turndown-root');
    } else {
      root = input.cloneNode(true);
    }
    collapseWhitespace({
      element: root,
      isBlock: isBlock,
      isVoid: isVoid,
      isPre: options.preformattedCode ? isPreOrCode : null
    });

    return root
  }

  var _htmlParser;
  function htmlParser () {
    _htmlParser = _htmlParser || new HTMLParser();
    return _htmlParser
  }

  function isPreOrCode (node) {
    return node.nodeName === 'PRE' || node.nodeName === 'CODE'
  }

  function Node (node, options) {
    node.isBlock = isBlock(node);
    node.isCode = node.nodeName === 'CODE' || node.parentNode.isCode;
    node.isBlank = isBlank(node);
    node.flankingWhitespace = flankingWhitespace(node, options);
    return node
  }

  function isBlank (node) {
    return (
      !isVoid(node) &&
      !isMeaningfulWhenBlank(node) &&
      /^\s*$/i.test(node.textContent) &&
      !hasVoid(node) &&
      !hasMeaningfulWhenBlank(node)
    )
  }

  function flankingWhitespace (node, options) {
    if (node.isBlock || (options.preformattedCode && node.isCode)) {
      return { leading: '', trailing: '' }
    }

    var edges = edgeWhitespace(node.textContent);

    // abandon leading ASCII WS if left-flanked by ASCII WS
    if (edges.leadingAscii && isFlankedByWhitespace('left', node, options)) {
      edges.leading = edges.leadingNonAscii;
    }

    // abandon trailing ASCII WS if right-flanked by ASCII WS
    if (edges.trailingAscii && isFlankedByWhitespace('right', node, options)) {
      edges.trailing = edges.trailingNonAscii;
    }

    return { leading: edges.leading, trailing: edges.trailing }
  }

  function edgeWhitespace (string) {
    var m = string.match(/^(([ \t\r\n]*)(\s*))(?:(?=\S)[\s\S]*\S)?((\s*?)([ \t\r\n]*))$/);
    return {
      leading: m[1], // whole string for whitespace-only strings
      leadingAscii: m[2],
      leadingNonAscii: m[3],
      trailing: m[4], // empty for whitespace-only strings
      trailingNonAscii: m[5],
      trailingAscii: m[6]
    }
  }

  function isFlankedByWhitespace (side, node, options) {
    var sibling;
    var regExp;
    var isFlanked;

    if (side === 'left') {
      sibling = node.previousSibling;
      regExp = / $/;
    } else {
      sibling = node.nextSibling;
      regExp = /^ /;
    }

    if (sibling) {
      if (sibling.nodeType === 3) {
        isFlanked = regExp.test(sibling.nodeValue);
      } else if (options.preformattedCode && sibling.nodeName === 'CODE') {
        isFlanked = false;
      } else if (sibling.nodeType === 1 && !isBlock(sibling)) {
        isFlanked = regExp.test(sibling.textContent);
      }
    }
    return isFlanked
  }

  var reduce = Array.prototype.reduce;
  var escapes = [
    [/\\/g, '\\\\'],
    [/\*/g, '\\*'],
    [/^-/g, '\\-'],
    [/^\+ /g, '\\+ '],
    [/^(=+)/g, '\\$1'],
    [/^(#{1,6}) /g, '\\$1 '],
    [/`/g, '\\`'],
    [/^~~~/g, '\\~~~'],
    [/\[/g, '\\['],
    [/\]/g, '\\]'],
    [/^>/g, '\\>'],
    [/_/g, '\\_'],
    [/^(\d+)\. /g, '$1\\. ']
  ];

  function TurndownService (options) {
    if (!(this instanceof TurndownService)) return new TurndownService(options)

    var defaults = {
      rules: rules,
      headingStyle: 'setext',
      hr: '* * *',
      bulletListMarker: '*',
      codeBlockStyle: 'indented',
      fence: '```',
      emDelimiter: '_',
      strongDelimiter: '**',
      linkStyle: 'inlined',
      linkReferenceStyle: 'full',
      br: '  ',
      preformattedCode: false,
      blankReplacement: function (content, node) {
        return node.isBlock ? '\n\n' : ''
      },
      keepReplacement: function (content, node) {
        return node.isBlock ? '\n\n' + node.outerHTML + '\n\n' : node.outerHTML
      },
      defaultReplacement: function (content, node) {
        return node.isBlock ? '\n\n' + content + '\n\n' : content
      }
    };
    this.options = extend({}, defaults, options);
    this.rules = new Rules(this.options);
  }

  TurndownService.prototype = {
    /**
     * The entry point for converting a string or DOM node to Markdown
     * @public
     * @param {String|HTMLElement} input The string or DOM node to convert
     * @returns A Markdown representation of the input
     * @type String
     */

    turndown: function (input) {
      if (!canConvert(input)) {
        throw new TypeError(
          input + ' is not a string, or an element/document/fragment node.'
        )
      }

      if (input === '') return ''

      var output = process.call(this, new RootNode(input, this.options));
      return postProcess.call(this, output)
    },

    /**
     * Add one or more plugins
     * @public
     * @param {Function|Array} plugin The plugin or array of plugins to add
     * @returns The Turndown instance for chaining
     * @type Object
     */

    use: function (plugin) {
      if (Array.isArray(plugin)) {
        for (var i = 0; i < plugin.length; i++) this.use(plugin[i]);
      } else if (typeof plugin === 'function') {
        plugin(this);
      } else {
        throw new TypeError('plugin must be a Function or an Array of Functions')
      }
      return this
    },

    /**
     * Adds a rule
     * @public
     * @param {String} key The unique key of the rule
     * @param {Object} rule The rule
     * @returns The Turndown instance for chaining
     * @type Object
     */

    addRule: function (key, rule) {
      this.rules.add(key, rule);
      return this
    },

    /**
     * Keep a node (as HTML) that matches the filter
     * @public
     * @param {String|Array|Function} filter The unique key of the rule
     * @returns The Turndown instance for chaining
     * @type Object
     */

    keep: function (filter) {
      this.rules.keep(filter);
      return this
    },

    /**
     * Remove a node that matches the filter
     * @public
     * @param {String|Array|Function} filter The unique key of the rule
     * @returns The Turndown instance for chaining
     * @type Object
     */

    remove: function (filter) {
      this.rules.remove(filter);
      return this
    },

    /**
     * Escapes Markdown syntax
     * @public
     * @param {String} string The string to escape
     * @returns A string with Markdown syntax escaped
     * @type String
     */

    escape: function (string) {
      return escapes.reduce(function (accumulator, escape) {
        return accumulator.replace(escape[0], escape[1])
      }, string)
    }
  };

  /**
   * Reduces a DOM node down to its Markdown string equivalent
   * @private
   * @param {HTMLElement} parentNode The node to convert
   * @returns A Markdown representation of the node
   * @type String
   */

  function process (parentNode) {
    var self = this;
    return reduce.call(parentNode.childNodes, function (output, node) {
      node = new Node(node, self.options);

      var replacement = '';
      if (node.nodeType === 3) {
        replacement = node.isCode ? node.nodeValue : self.escape(node.nodeValue);
      } else if (node.nodeType === 1) {
        replacement = replacementForNode.call(self, node);
      }

      return join(output, replacement)
    }, '')
  }

  /**
   * Appends strings as each rule requires and trims the output
   * @private
   * @param {String} output The conversion output
   * @returns A trimmed version of the ouput
   * @type String
   */

  function postProcess (output) {
    var self = this;
    this.rules.forEach(function (rule) {
      if (typeof rule.append === 'function') {
        output = join(output, rule.append(self.options));
      }
    });

    return output.replace(/^[\t\r\n]+/, '').replace(/[\t\r\n\s]+$/, '')
  }

  /**
   * Converts an element node to its Markdown equivalent
   * @private
   * @param {HTMLElement} node The node to convert
   * @returns A Markdown representation of the node
   * @type String
   */

  function replacementForNode (node) {
    var rule = this.rules.forNode(node);
    var content = process.call(this, node);
    var whitespace = node.flankingWhitespace;
    if (whitespace.leading || whitespace.trailing) content = content.trim();
    return (
      whitespace.leading +
      rule.replacement(content, node, this.options) +
      whitespace.trailing
    )
  }

  /**
   * Joins replacement to the current output with appropriate number of new lines
   * @private
   * @param {String} output The current conversion output
   * @param {String} replacement The string to append to the output
   * @returns Joined output
   * @type String
   */

  function join (output, replacement) {
    var s1 = trimTrailingNewlines(output);
    var s2 = trimLeadingNewlines(replacement);
    var nls = Math.max(output.length - s1.length, replacement.length - s2.length);
    var separator = '\n\n'.substring(0, nls);

    return s1 + separator + s2
  }

  /**
   * Determines whether an input can be converted
   * @private
   * @param {String|HTMLElement} input Describe this parameter
   * @returns Describe what it returns
   * @type String|Object|Array|Boolean|Number
   */

  function canConvert (input) {
    return (
      input != null && (
        typeof input === 'string' ||
        (input.nodeType && (
          input.nodeType === 1 || input.nodeType === 9 || input.nodeType === 11
        ))
      )
    )
  }

  return TurndownService;

}());

var turndownPluginGfm = (function (exports) {
'use strict';

var highlightRegExp = /highlight-(?:text|source)-([a-z0-9]+)/;

function highlightedCodeBlock (turndownService) {
  turndownService.addRule('highlightedCodeBlock', {
    filter: function (node) {
      var firstChild = node.firstChild;
      return (
        node.nodeName === 'DIV' &&
        highlightRegExp.test(node.className) &&
        firstChild &&
        firstChild.nodeName === 'PRE'
      )
    },
    replacement: function (content, node, options) {
      var className = node.className || '';
      var language = (className.match(highlightRegExp) || [null, ''])[1];

      return (
        '\n\n' + options.fence + language + '\n' +
        node.firstChild.textContent +
        '\n' + options.fence + '\n\n'
      )
    }
  });
}

function strikethrough (turndownService) {
  turndownService.addRule('strikethrough', {
    filter: ['del', 's', 'strike'],
    replacement: function (content) {
      return '~' + content + '~'
    }
  });
}

var indexOf = Array.prototype.indexOf;
var every = Array.prototype.every;
var rules = {};

rules.tableCell = {
  filter: ['th', 'td'],
  replacement: function (content, node) {
    return cell(content, node)
  }
};

rules.tableRow = {
  filter: 'tr',
  replacement: function (content, node) {
    var borderCells = '';
    var alignMap = { left: ':--', right: '--:', center: ':-:' };

    if (isHeadingRow(node)) {
      for (var i = 0; i < node.childNodes.length; i++) {
        var border = '---';
        var align = (
          node.childNodes[i].getAttribute('align') || ''
        ).toLowerCase();

        if (align) border = alignMap[align] || border;

        borderCells += cell(border, node.childNodes[i]);
      }
    }
    return '\n' + content + (borderCells ? '\n' + borderCells : '')
  }
};

rules.table = {
  // Only convert tables with a heading row.
  // Tables with no heading row are kept using `keep` (see below).
  filter: function (node) {
    return node.nodeName === 'TABLE' && isHeadingRow(node.rows[0])
  },

  replacement: function (content) {
    // Ensure there are no blank lines
    content = content.replace('\n\n', '\n');
    return '\n\n' + content + '\n\n'
  }
};

rules.tableSection = {
  filter: ['thead', 'tbody', 'tfoot'],
  replacement: function (content) {
    return content
  }
};

// A tr is a heading row if:
// - the parent is a THEAD
// - or if its the first child of the TABLE or the first TBODY (possibly
//   following a blank THEAD)
// - and every cell is a TH
function isHeadingRow (tr) {
  var parentNode = tr.parentNode;
  return (
    parentNode.nodeName === 'THEAD' ||
    (
      parentNode.firstChild === tr &&
      (parentNode.nodeName === 'TABLE' || isFirstTbody(parentNode)) &&
      every.call(tr.childNodes, function (n) { return n.nodeName === 'TH' })
    )
  )
}

function isFirstTbody (element) {
  var previousSibling = element.previousSibling;
  return (
    element.nodeName === 'TBODY' && (
      !previousSibling ||
      (
        previousSibling.nodeName === 'THEAD' &&
        /^\s*$/i.test(previousSibling.textContent)
      )
    )
  )
}

function cell (content, node) {
  var index = indexOf.call(node.parentNode.childNodes, node);
  var prefix = ' ';
  if (index === 0) prefix = '| ';
  return prefix + content + ' |'
}

function tables (turndownService) {
  turndownService.keep(function (node) {
    return node.nodeName === 'TABLE' && !isHeadingRow(node.rows[0])
  });
  for (var key in rules) turndownService.addRule(key, rules[key]);
}

function taskListItems (turndownService) {
  turndownService.addRule('taskListItems', {
    filter: function (node) {
      return node.type === 'checkbox' && node.parentNode.nodeName === 'LI'
    },
    replacement: function (content, node) {
      return (node.checked ? '[x]' : '[ ]') + ' '
    }
  });
}

function gfm (turndownService) {
  turndownService.use([
    highlightedCodeBlock,
    strikethrough,
    tables,
    taskListItems
  ]);
}

exports.gfm = gfm;
exports.highlightedCodeBlock = highlightedCodeBlock;
exports.strikethrough = strikethrough;
exports.tables = tables;
exports.taskListItems = taskListItems;

return exports;

}({}));

})();
