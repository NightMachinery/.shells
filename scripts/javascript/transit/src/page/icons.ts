// Every icon on this page, as SVG.
//
// They used to be characters: a walking figure, a bell, a gear, a heavy cross.
// That works until it does not, and when it does not it fails in the worst
// possible way, which is silently and only on somebody else's device. The
// walking figure is in a Unicode block Android's system font does not cover, so
// the one symbol that says "and then you walk" rendered as an empty box on the
// phone this page exists for, while looking perfect on the machine it was
// written on. Nothing in a test that runs here could have caught that, because
// the font is not here.
//
// A glyph is a request that some font, somewhere, happens to have a picture.
// An inline SVG is the picture. So the icons are drawn here, once, referenced
// by `<use>`, sized in em so they scale with whatever text they sit beside, and
// painted with `currentColor` so they inherit the colour of that text rather
// than needing one of their own.
//
// Text punctuation is deliberately not here. An arrow between two stop names, a
// middle dot between two facts, an ellipsis on a shortened name: those are
// words, not icons, and they live in blocks every system font covers.

const SVG_NS = 'http://www.w3.org/2000/svg';

/** The id prefix, so nothing here can collide with an id the page makes. */
const PREFIX = 'transit-icon-';

export type IconName = 'walk' | 'bell' | 'gear' | 'close' | 'dot-filled' | 'dot-hollow';

/**
 * Each icon as the inside of a 24x24 symbol.
 *
 * Strokes rather than fills wherever the shape allows, because a stroked icon
 * stays legible at the nine or ten pixels these are drawn at, where a filled
 * one turns into a blob.
 */
const PATHS: Record<IconName, string> = {
  // A walking figure: head, body, two legs, one arm. Drawn rather than borrowed
  // so it reads at ten pixels, where a faithful pictogram does not.
  walk: '<circle cx="13" cy="4.2" r="2.2" fill="currentColor"/><path d="M12.4 8.2 10 13.2l-2.6 1.4M12.4 8.2l2.8 1.6.9 3.4M12.4 8.2 11 12l2.4 2.3.6 5.5M13.4 14.3 10 19.7" fill="none" stroke="currentColor" stroke-width="1.9" stroke-linecap="round" stroke-linejoin="round"/>',
  // A bell, with its clapper, because a bell without one reads as a dome.
  bell: '<path d="M12 3.2a5.4 5.4 0 0 0-5.4 5.4c0 4.3-1.5 5.6-2 6.3-.3.4 0 1 .5 1h13.8c.5 0 .8-.6.5-1-.5-.7-2-2-2-6.3A5.4 5.4 0 0 0 12 3.2Z" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linejoin="round"/><path d="M9.8 18.4a2.3 2.3 0 0 0 4.4 0" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/>',
  // A gear: a ring and six teeth, which is as much as survives at this size.
  gear: '<circle cx="12" cy="12" r="3.1" fill="none" stroke="currentColor" stroke-width="1.8"/><path d="M12 2.6v3M12 18.4v3M21.4 12h-3M5.6 12h-3M18.6 5.4l-2.1 2.1M7.5 16.5l-2.1 2.1M18.6 18.6l-2.1-2.1M7.5 7.5 5.4 5.4" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/>',
  close: '<path d="M6 6l12 12M18 6 6 18" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round"/>',
  // The two states a time can be in. A disc that is filled or not carries the
  // difference at any size and in any font, which a word does not.
  'dot-filled': '<circle cx="12" cy="12" r="6" fill="currentColor"/>',
  'dot-hollow': '<circle cx="12" cy="12" r="5.2" fill="none" stroke="currentColor" stroke-width="2.2"/>',
};

/**
 * Put the sprite in the document, once.
 *
 * Both pages call this, and the second call on the same document does nothing.
 * The sprite is hidden rather than sized to nothing, because a zero-sized SVG is
 * not reliably rendered into a `<use>` by every engine.
 */
export function installIcons(): void {
  if (document.getElementById(`${PREFIX}sprite`) !== null) return;
  const sprite = document.createElementNS(SVG_NS, 'svg');
  sprite.id = `${PREFIX}sprite`;
  sprite.setAttribute('aria-hidden', 'true');
  sprite.setAttribute('focusable', 'false');
  sprite.style.position = 'absolute';
  sprite.style.width = '0';
  sprite.style.height = '0';
  sprite.style.overflow = 'hidden';

  let markup = '';
  for (const [name, body] of Object.entries(PATHS)) {
    markup += `<symbol id="${PREFIX}${name}" viewBox="0 0 24 24">${body}</symbol>`;
  }
  // The one place this page builds nodes from a string. The content is a
  // constant in this file with nothing interpolated into it, so there is
  // nothing here for anybody to inject; everywhere else takes user or upstream
  // text and must keep using textContent.
  const holder = document.createElementNS(SVG_NS, 'defs');
  holder.innerHTML = markup;
  sprite.append(holder);
  document.body.prepend(sprite);
}

/**
 * One icon, as an element to put where a glyph used to go.
 *
 * `title` rather than an aria-label by default: these sit next to or inside
 * something that already names itself, and a second name would be read twice.
 */
export function icon(name: IconName, className?: string): SVGSVGElement {
  const node = document.createElementNS(SVG_NS, 'svg');
  node.setAttribute('class', className === undefined ? 'icon' : `icon ${className}`);
  node.setAttribute('viewBox', '0 0 24 24');
  node.setAttribute('aria-hidden', 'true');
  node.setAttribute('focusable', 'false');
  const use = document.createElementNS(SVG_NS, 'use');
  // Both spellings: `href` is the current one and `xlink:href` is what older
  // engines still read. Setting one and not the other loses somebody.
  use.setAttribute('href', `#${PREFIX}${name}`);
  use.setAttributeNS('http://www.w3.org/1999/xlink', 'xlink:href', `#${PREFIX}${name}`);
  node.append(use);
  return node;
}
