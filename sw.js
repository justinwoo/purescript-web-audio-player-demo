self.addEventListener('install', function(e) {
  e.waitUntil(
    caches.open('airhorner').then(function(cache) {
      return cache.addAll([
        './',
        './index.html',
        './dist/app.js',
        './dist/app.css'
      ]);
    })
  );
});
