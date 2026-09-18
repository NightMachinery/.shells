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
// Bumping CACHE invalidates everything. Do that when the shell changes shape in
// a way the stale-while-revalidate below cannot heal on its own.

const CACHE = 'transit-shell-v1';

/**
 * The shell, relative to this worker's scope so the same bytes work wherever
 * the page is mounted. `data/config.json` is in here because the page cannot
 * draw a single tab without it, and it is a small file that changes only when
 * the configuration does.
 */
const SHELL = ['./', './index.html', './app.js', './icon.png', './icon-180.png', './manifest.webmanifest', './data/config.json'];

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
          if (response.ok) {
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
