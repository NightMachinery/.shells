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

const CACHE = 'transit-shell-__SHELL_HASH__';

/**
 * The shell, relative to this worker's scope so the same bytes work wherever
 * the page is mounted. `data/config.json` is in here because the page cannot
 * draw a single tab without it, and it is a small file that changes only when
 * the configuration does.
 */
const SHELL = [
  './',
  './index.html',
  './app.js',
  './theme.css',
  // The expanded view of one journey. It is in the shell because it is opened
  // from a board, often underground, and it needs nothing but itself and its
  // own URL to render: caching it is what makes that true offline as well.
  './route.html',
  './route.js',
  './icon.png',
  './icon-180.png',
  './manifest.webmanifest',
  './data/config.json',
];

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

self.addEventListener('fetch', (event) => {
  const request = event.request;
  if (request.method !== 'GET') return;

  const url = new URL(request.url);
  // Only this origin, and only inside the worker's own scope. The departures
  // come from two third-party APIs and must never be touched here: they are
  // live data whose whole value is that it was fetched just now.
  if (url.origin !== self.location.origin) return;
  if (!url.pathname.startsWith(new URL('./', self.location.href).pathname)) return;

  // Stale while revalidate: answer from the cache at once so the app opens
  // instantly, and refresh the entry in the background so the next open has the
  // new deploy. One open behind is the price of opening at all when there is no
  // network, and a deploy here is never urgent.
  event.respondWith(
    caches.match(request).then((cached) => {
      const network = fetch(request)
        .then((response) => {
          // `ok` is not enough on a site behind an access gate. An expired
          // session answers a request for the shell with a redirect to a login
          // page, and following that redirect produces a perfectly successful
          // response that is not this page. Cached, it would replace the app
          // with a login screen that outlives the session it belonged to and
          // would be served offline for ever. `basic` means the bytes came from
          // this origin without a cross-origin hop, which is the only case
          // worth keeping, and the final URL is checked too because a
          // same-origin redirect can still land somewhere else.
          if (response.ok && response.type === 'basic' && new URL(response.url || request.url).origin === self.location.origin) {
            const copy = response.clone();
            void caches.open(CACHE).then((cache) => cache.put(request, copy));
          }
          return response;
        })
        .catch(() => cached ?? Response.error());
      return cached ?? network;
    }),
  );
});
