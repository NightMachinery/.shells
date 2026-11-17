// Build revision: bf8e6d2b26c8
'use strict';
self.addEventListener('install', event => event.waitUntil(self.skipWaiting()));
self.addEventListener('activate', event => event.waitUntil(self.clients.claim()));

// Intentionally no Cache Storage: private pages, login responses, the catalog,
// audio, and write requests must never be replayed after a login expires.
self.addEventListener('fetch', event => {
  const request = event.request;
  const url = new URL(request.url);
  const scope = new URL(self.registration.scope);
  if (request.method !== 'GET' || request.mode !== 'navigate' ||
      url.origin !== scope.origin || !url.pathname.startsWith(scope.pathname)) return;
  event.respondWith(fetch(request, {cache: 'no-store'}).catch(() => new Response(`<!doctype html>
<html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<meta name="theme-color" content="#f7f8f3"><title>Podcasts · Offline</title>
<style>body{font:17px/1.7 system-ui,sans-serif;margin:0;background:#f7f8f3;color:#20342e}main{max-width:32rem;margin:15vh auto;padding:28px}h1{font:38px Georgia,serif}a{display:inline-block;margin-top:12px;padding:10px 18px;border-radius:8px;background:#216246;color:white;text-decoration:none}</style>
<main><h1>You’re offline</h1><p>Connect to the internet to open your podcast library and check your downloads.</p><a href="./">Try again</a></main></html>`, {
    status: 503,
    headers: {'Content-Type': 'text/html; charset=utf-8', 'Cache-Control': 'no-store'}
  })));
});
