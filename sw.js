self.addEventListener('install', function(e) {
  e.waitUntil(
    caches.open('halogen-web-audio-player-demo').then(function(cache) {
      return cache.addAll([
        './index.html',
        './index.html?homescreen=1',
        './dist/app.js',
        './dist/app.css'
      ]);
    })
  );
});

self.addEventListener('fetch', function(event) {
  console.log(event.request.url);
  event.respondWith(
    caches.match(event.request).then(function(response) {
      return response || fetch(event.request);
    })
  );
});
