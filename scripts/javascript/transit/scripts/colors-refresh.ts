/**
 * Regenerate `data/line-colors.json` from the transit association's published
 * GTFS feed. Run it with `bun run colors:refresh`.
 *
 * Source archive (the network-wide feed, covering regional rail, rapid transit,
 * underground, tram and every bus category):
 *   https://www.mvv-muenchen.de/fileadmin/mediapool/developer/opendata/gesamt_gtfs.zip
 * listed on https://www.mvv-muenchen.de/service-hilfe/mvv-content-fuer-entwickler
 *
 * Licence: Creative Commons Attribution (CC BY). The publisher asks that work
 * derived from the feed name "Münchner Verkehrs- und Tarifverbund GmbH (MVV)"
 * along with the feed version. Both travel with the data in the generated file.
 * The retrieval *date* the publisher also asks for is deliberately absent: that
 * file is committed, and a date would turn every regeneration into a diff even
 * when the feed has not moved. `feed_version` identifies the release instead
 * and changes only when the feed does. Same reasoning as the config exporter.
 *
 * Only `routes.txt` and `feed_info.txt` are read. The rest of the archive is
 * timetable data this package has no use for.
 */

const FEED_URL = 'https://www.mvv-muenchen.de/fileadmin/mediapool/developer/opendata/gesamt_gtfs.zip';

const ATTRIBUTION = 'Münchner Verkehrs- und Tarifverbund GmbH (MVV), CC BY';

/**
 * Refuse an archive larger than this. The network-wide feed is tens of
 * megabytes; a jump into the hundreds means the URL now points at something
 * else, most likely a feed carrying shapes or a country-wide aggregate, and
 * downloading it by accident is worse than failing with a message.
 */
const MAX_ARCHIVE_BYTES = 64 * 1024 * 1024;

const OUTPUT_PATH = new URL('../data/line-colors.json', import.meta.url).pathname;

interface LineColor {
  bg: string;
  fg?: string;
}

/**
 * Colours the feed uses as *filler* rather than as artwork: a value it gives to
 * every route of an operator because the operator supplied none.
 *
 * This feed assigns one colour per agency, and the city operator's share of it
 * is black. Black is not what that operator prints on anything, and writing it
 * into the table would turn every tram and city bus badge into the same slab of
 * black, which is a worse answer than the per-mode default those lines fall
 * back to. The regional colours in the feed are kept, because those genuinely
 * are the categories' printed colours.
 *
 * Black reaching the table through OVERRIDES is a different matter and stays:
 * one suburban line really is black in the official artwork.
 */
const FILLER_COLORS: ReadonlySet<string> = new Set(['#000000']);

/**
 * Official colours that the feed does not carry, kept so that a regeneration
 * cannot silently undo them.
 *
 * The feed encodes colour per *agency*, not per line, so every rapid-transit
 * line in it is the one suburban green and every underground line is filler
 * black. The values below are the operators' own per-line artwork, which is
 * what a badge is supposed to show. Applied after the feed, so they win.
 *
 * Two of the underground lines are drawn as a pair of stripes officially; a
 * single colour is not expressible, so each takes one of its pair. One suburban
 * number is not currently in service and takes a neutral tone. No `fg`, so the
 * text colour comes from the contrast computation in `src/colors.ts`.
 */
const OVERRIDES: Readonly<Record<string, LineColor>> = {
  S1: { bg: '#16BAE7' },
  S2: { bg: '#76B82A' },
  S3: { bg: '#951B81' },
  S4: { bg: '#E30613' },
  S5: { bg: '#8A8A8A' },
  S6: { bg: '#00975F' },
  S7: { bg: '#963833' },
  S8: { bg: '#000000' },
  S20: { bg: '#F05A73' },
  U1: { bg: '#52822F' },
  U2: { bg: '#C20831' },
  U3: { bg: '#EC6725' },
  U4: { bg: '#00A984' },
  U5: { bg: '#BC7A00' },
  U6: { bg: '#0065AE' },
  U7: { bg: '#C20831' },
  U8: { bg: '#EC6725' },
};

/**
 * The same normalisation `lineKey` in `src/colors.ts` applies. Duplicated
 * rather than imported so that this generator does not depend on the file it
 * generates: `src/colors.ts` imports `data/line-colors.json`, and a generator
 * that cannot run until its own output exists is a generator you cannot use to
 * recover from a bad one.
 */
function lineKey(label: string): string {
  return label.replace(/\s+/g, '').toUpperCase();
}

/** `#RRGGBB` from a GTFS colour, which arrives as six hex digits and no hash. */
function toHex(value: string): string | null {
  const text = value.trim();
  return /^[0-9a-fA-F]{6}$/.test(text) ? `#${text.toUpperCase()}` : null;
}

// --- Reading one member out of a zip archive ------------------------------
//
// Done here rather than by shelling out to `unzip` or `bsdtar` so the script
// needs nothing but bun, and by seeking the central directory rather than
// streaming so that only the member we want is ever decompressed.

const EOCD_SIGNATURE = 0x06054b50;
const CENTRAL_SIGNATURE = 0x02014b50;
/** A zip comment can be 64 KiB, and the record itself is 22 bytes. */
const EOCD_SEARCH_WINDOW = 22 + 0xffff;

function findEndOfCentralDirectory(view: DataView): number {
  const first = Math.max(0, view.byteLength - EOCD_SEARCH_WINDOW);
  for (let at = view.byteLength - 22; at >= first; at -= 1) {
    if (view.getUint32(at, true) === EOCD_SIGNATURE) return at;
  }
  throw new Error('not a zip archive: no end-of-central-directory record');
}

async function inflateRaw(bytes: Uint8Array): Promise<string> {
  const source = new Response(bytes).body;
  if (source === null) throw new Error('empty deflate stream');
  return await new Response(source.pipeThrough(new DecompressionStream('deflate-raw'))).text();
}

async function readMember(archive: Uint8Array, name: string): Promise<string> {
  const view = new DataView(archive.buffer, archive.byteOffset, archive.byteLength);
  const eocd = findEndOfCentralDirectory(view);
  const entries = view.getUint16(eocd + 10, true);
  let at = view.getUint32(eocd + 16, true);
  if (at === 0xffffffff) throw new Error('zip64 archives are not supported');

  const names = new TextDecoder();
  for (let index = 0; index < entries; index += 1) {
    if (view.getUint32(at, true) !== CENTRAL_SIGNATURE) throw new Error('corrupt central directory');
    const method = view.getUint16(at + 10, true);
    const compressed = view.getUint32(at + 20, true);
    const nameLength = view.getUint16(at + 28, true);
    const extraLength = view.getUint16(at + 30, true);
    const commentLength = view.getUint16(at + 32, true);
    const localHeader = view.getUint32(at + 42, true);
    const entryName = names.decode(archive.subarray(at + 46, at + 46 + nameLength));
    at += 46 + nameLength + extraLength + commentLength;
    if (entryName !== name) continue;

    // The central directory's lengths are authoritative, but the payload sits
    // after the *local* header, whose name and extra fields have their own
    // lengths and are routinely a different size from the central copy.
    const localNameLength = view.getUint16(localHeader + 26, true);
    const localExtraLength = view.getUint16(localHeader + 28, true);
    const start = localHeader + 30 + localNameLength + localExtraLength;
    const payload = archive.subarray(start, start + compressed);
    if (method === 0) return new TextDecoder().decode(payload);
    if (method === 8) return await inflateRaw(payload);
    throw new Error(`${name}: unsupported compression method ${method}`);
  }
  throw new Error(`${name}: not in the archive`);
}

// --- CSV --------------------------------------------------------------------

/**
 * RFC 4180 with the one wrinkle GTFS actually uses: quoted fields containing
 * commas, and doubled quotes inside them. Line names and long names both carry
 * commas, so splitting on commas mangles the row.
 */
function parseCsv(text: string): Record<string, string>[] {
  const body = text.charCodeAt(0) === 0xfeff ? text.slice(1) : text;
  const rows: string[][] = [];
  let row: string[] = [];
  let field = '';
  let quoted = false;
  for (let at = 0; at < body.length; at += 1) {
    const char = body[at];
    if (quoted) {
      if (char !== '"') field += char;
      else if (body[at + 1] === '"') { field += '"'; at += 1; }
      else quoted = false;
      continue;
    }
    if (char === '"') quoted = true;
    else if (char === ',') { row.push(field); field = ''; }
    else if (char === '\n') { row.push(field); rows.push(row); row = []; field = ''; }
    else if (char !== '\r') field += char;
  }
  if (field.length > 0 || row.length > 0) { row.push(field); rows.push(row); }

  const header = rows.shift();
  if (header === undefined) return [];
  return rows
    .filter((entry) => entry.length >= header.length)
    .map((entry) => Object.fromEntries(header.map((key, index) => [key, entry[index] ?? ''])));
}

// --- Building the table -----------------------------------------------------

interface Tally {
  /** Count per `bg\tfg` pair, so the most frequent spelling of a line wins. */
  counts: Map<string, number>;
}

function mostFrequent(tally: Tally): { pair: string; collisions: number } {
  let best = '';
  let bestCount = -1;
  let total = 0;
  // Ties break on the lexicographically smaller pair so the output does not
  // depend on the order routes happen to appear in the feed.
  for (const [pair, count] of [...tally.counts].sort((a, b) => (a[0] < b[0] ? -1 : 1))) {
    total += count;
    if (count > bestCount) { best = pair; bestCount = count; }
  }
  return { pair: best, collisions: tally.counts.size > 1 ? total - bestCount : 0 };
}

function render(doc: {
  source: string;
  feedVersion: string;
  attribution: string;
  lines: Record<string, LineColor>;
}): string {
  // Written by hand rather than through `JSON.stringify(…, 2)` so that one line
  // of the file is one line of the network. Sorted by key, so a regeneration
  // that changes nothing produces the same bytes.
  const entries = Object.keys(doc.lines)
    .sort()
    .map((key) => {
      const value = doc.lines[key] as LineColor;
      const fields =
        value.fg === undefined ? `"bg": "${value.bg}"` : `"bg": "${value.bg}", "fg": "${value.fg}"`;
      return `    ${JSON.stringify(key)}: { ${fields} }`;
    });
  return [
    '{',
    `  "source": ${JSON.stringify(doc.source)},`,
    '  "generated_from": "routes.txt",',
    `  "feed_version": ${JSON.stringify(doc.feedVersion)},`,
    `  "attribution": ${JSON.stringify(doc.attribution)},`,
    '  "lines": {',
    entries.join(',\n'),
    '  }',
    '}',
    '',
  ].join('\n');
}

async function download(url: string): Promise<Uint8Array> {
  const head = await fetch(url, { method: 'HEAD' });
  if (!head.ok) throw new Error(`${url}: HTTP ${head.status}`);
  const declared = Number(head.headers.get('content-length') ?? Number.NaN);
  if (Number.isFinite(declared) && declared > MAX_ARCHIVE_BYTES) {
    throw new Error(
      `${url}: ${Math.round(declared / 1e6)} MB exceeds the ${Math.round(MAX_ARCHIVE_BYTES / 1e6)} MB ceiling; ` +
        'check that the URL still points at the network-wide feed',
    );
  }
  const response = await fetch(url);
  if (!response.ok) throw new Error(`${url}: HTTP ${response.status}`);
  return new Uint8Array(await response.arrayBuffer());
}

async function main(): Promise<void> {
  const archive = await download(FEED_URL);
  const routes = parseCsv(await readMember(archive, 'routes.txt'));
  const info = parseCsv(await readMember(archive, 'feed_info.txt'));
  const feedVersion = info[0]?.['feed_version'] ?? '';

  const tallies = new Map<string, Tally>();
  let filler = 0;
  let uncoloured = 0;
  for (const route of routes) {
    const key = lineKey(route['route_short_name'] ?? '');
    const bg = toHex(route['route_color'] ?? '');
    if (key.length === 0) continue;
    if (bg === null) { uncoloured += 1; continue; }
    if (FILLER_COLORS.has(bg)) { filler += 1; continue; }
    const fg = toHex(route['route_text_color'] ?? '');
    const pair = `${bg}\t${fg ?? ''}`;
    const tally = tallies.get(key) ?? { counts: new Map<string, number>() };
    tally.counts.set(pair, (tally.counts.get(pair) ?? 0) + 1);
    tallies.set(key, tally);
  }

  const lines: Record<string, LineColor> = {};
  let collisions = 0;
  for (const [key, tally] of tallies) {
    const winner = mostFrequent(tally);
    collisions += winner.collisions;
    const [bg, fg] = winner.pair.split('\t');
    lines[key] = fg === undefined || fg.length === 0 ? { bg: bg as string } : { bg: bg as string, fg };
  }

  const disagreements: string[] = [];
  for (const [key, official] of Object.entries(OVERRIDES)) {
    const generated = lines[key];
    if (generated === undefined || generated.bg !== official.bg) {
      disagreements.push(`${key}: feed ${generated?.bg ?? 'absent'} -> official ${official.bg}`);
    }
    lines[key] = official;
  }

  await Bun.write(
    OUTPUT_PATH,
    render({ source: FEED_URL, feedVersion, attribution: ATTRIBUTION, lines }),
  );

  const report = [
    `line-colors: ${Object.keys(lines).length} lines from feed version ${feedVersion || 'unknown'}`,
    `line-colors: ${routes.length} routes read, ${filler} dropped as filler colours, ${uncoloured} with no colour`,
    `line-colors: ${collisions} routes lost a short-name collision to a more frequent colour`,
    `line-colors: ${disagreements.length} hand-checked lines disagreed with the feed`,
    ...disagreements.map((entry) => `line-colors:   ${entry}`),
  ];
  process.stderr.write(`${report.join('\n')}\n`);
}

await main();
