// A bare-bones service worker that hijacks its own fetches.

/* Install ------------------------------------------------------------- */
self.addEventListener("install", (event) => {
  // Activate immediately so changes show up without a second reload
  self.skipWaiting();
});

/* Activate ------------------------------------------------------------ */
self.addEventListener("activate", (event) => {
  // Become the controlling SW for *all* open tabs under our scope
  event.waitUntil(self.clients.claim());
});

/* Fetch --------------------------------------------------------------- */
self.addEventListener("fetch", (event) => {
  const { request } = event;
  const url = new URL(request.url);

  if (url.pathname === "/sha256" && request.method === "POST") {
    event.respondWith(
      (async () => {
        const buf = await request.bytes();
        const hash = await crypto.subtle.digest("SHA-256", buf);
        return new Response(hash, {
          headers: { "Content-Type": "application/octet-stream" },
        });
      })()
    );
  }
});
