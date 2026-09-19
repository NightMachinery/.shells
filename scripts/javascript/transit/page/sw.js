// The service worker, which exists for one reason: so that opening the installed
// app underground shows the app rather than the browser's offline page.
//
// It caches the shell and nothing else. The departures are not here: they live
// in IndexedDB, written by the page after each successful fetch, and the page
// renders them with their age on load. That split is deliberate. A service
// worker that cached API responses would serve a stale board silently, with no
// age and no way for the page to know it was stale, which is the one failure
// this whole page is built to avoid.
//
// The cache name carries a hash of every shell file, written in when the page is
// published. This is not a nicety. A browser decides whether to install a new
// worker by comparing the worker's own bytes, so a deploy that changes the page
// but not this file is a deploy the installed app never notices: it keeps the
// old cache, keeps answering from it, and the reader sees a version that is
// gone. The only way to make that impossible is to make a changed shell change
// this file, which is what the hash does. An unpublished copy says the
// placeholder and is still a valid, if permanent, cache name.
//
// A shell is all of a piece, and this file's second job is to make sure a reader
// is never served half of one. The rule is: the only thing that ever writes to
// the cache is an install, which writes every shell file together or none of
// them. Nothing is revalidated at fetch time. See "Why nothing is written at
// fetch time" below, which is a bug report as much as a comment.

const SHELL_VERSION = '__SHELL_HASH__';
const CACHE = `transit-shell-${SHELL_VERSION}`;

/**
 * The shell, relative to this worker's scope so the same bytes work wherever
 * the page is mounted. `data/config.json` is in here because the page cannot
 * draw a single tab without it, and it is a small file that changes only when
 * the configuration does.
 *
 * The three files the two pages link carry the shell version in their URL, and
 * the pages ask for them by exactly these URLs. That is what stops any cache
 * anywhere, this one or the browser's own, from pairing a script with a
 * stylesheet from a different build: the two builds do not share a URL, so one
 * cannot stand in for the other.
 */
const SHELL = [
  './',
  './index.html',
  `./app.js?v=${SHELL_VERSION}`,
  `./theme.css?v=${SHELL_VERSION}`,
  // The expanded view of one journey. It is in the shell because it is opened
  // from a board, often underground, and it needs nothing but itself and its
  // own URL to render: caching it is what makes that true offline as well.
  './route.html',
  `./route.js?v=${SHELL_VERSION}`,
  './icon.png',
  './icon-180.png',
  './manifest.webmanifest',
  './data/config.json',
];

/** The shell as absolute URLs, which is what a request can be compared against. */
const SHELL_URLS = new Set(SHELL.map((path) => new URL(path, self.location.href).href));

const INDEX_URL = new URL('./index.html', self.location.href).href;

self.addEventListener('install', (event) => {
  event.waitUntil(
    caches
      .open(CACHE)
      .then((cache) => cache.addAll(SHELL))
      // A shell file that will not cache must not block installation: the page
      // still works online, and the next install will try again.
      .catch(() => undefined)
      .then(() => self.skipWaiting()),
  );
});

self.addEventListener('activate', (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) => Promise.all(keys.filter((key) => key !== CACHE).map((key) => caches.delete(key))))
      .then(() => self.clients.claim()),
  );
});

// Why nothing is written at fetch time.
//
// This used to answer from the cache and refresh the entry behind the reader,
// one file at a time. Each file separately: that is the whole bug. A client that
// opened the page while a deploy was half-fetched kept the old worker, and the
// old worker quietly replaced whichever files that visit happened to touch. It
// ended up holding a script from one build and the page and stylesheet from
// another, in one cache, with a cache name that still claimed to be a single
// shell. On a phone that is not a subtle failure: the script drew a row the way
// the new build draws it, the stylesheet laid it out the way the old build laid
// it out, and the board ran off the side of the screen with the state word
// spilling out of a badge that had nowhere to sit. Nothing on the server was
// wrong, and no check against the server could have seen it, because the mixture
// only ever existed inside one browser's cache.
//
// So the cache is written by exactly one thing, the install step, which writes
// every shell file at once or none of them, and a new version arrives only as a
// new worker. Being one open behind is the price of never being half a build.
self.addEventListener('fetch', (event) => {
  const request = event.request;
  if (request.method !== 'GET') return;

  const url = new URL(request.url);
  // Only this origin, and only inside the worker's own scope. The departures
  // come from two third-party APIs and must never be touched here: they are
  // live data whose whole value is that it was fetched just now.
  if (url.origin !== self.location.origin) return;
  if (!url.pathname.startsWith(new URL('./', self.location.href).pathname)) return;

  // A navigation is the one request that cannot be matched by URL alone: the
  // reader can arrive at the directory, at the page, or at either with a query
  // on it, and all three mean the same document.
  if (request.mode === 'navigate') {
    event.respondWith(
      caches
        .open(CACHE)
        .then((cache) => cache.match(request, { ignoreSearch: true }).then((hit) => hit ?? cache.match(INDEX_URL)))
        .then((hit) => hit ?? fetch(request)),
    );
    return;
  }

  // Everything else that is not the shell is the network's business. There is
  // nothing to gain by caching it and a great deal to lose by caching it stale.
  if (!SHELL_URLS.has(url.href)) return;

  event.respondWith(
    caches
      .open(CACHE)
      .then((cache) => cache.match(url.href))
      // A miss here means an install that did not finish. The network is the
      // right answer, and it is deliberately not written back: a cache entry
      // this worker did not install is exactly the mixture this file exists to
      // prevent.
      .then((hit) => hit ?? fetch(request)),
  );
});
