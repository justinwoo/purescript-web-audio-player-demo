self.addEventListener('install', function(e) {
  e.waitUntil(
    caches.open('airhorner').then(function(cache) {
      return cache.addAll([
        './purescript-web-audio-player-demo',
        './purescript-web-audio-player-demo/index.html',
        './purescript-web-audio-player-demo/dist/app.js'
      ]);
    })
  );
});
