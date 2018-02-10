self.addEventListener("install", function(e) {
  e.waitUntil(
    caches.open("halogen-web-audio-player-demo").then(function(cache) {
      return cache.addAll([
        "./index.html",
        "./index.html?homescreen=1",
        "./dist/app.js",
        "./dist/app.css"
      ]);
    })
  );
});

self.addEventListener("fetch", function(event) {
  console.log(event.request.url);
  fetch(event.request).catch(function() {
    return caches.match(event.request).then(function(response) {
      if (response) {
        return response;
      } else {
        alert("File Missing and cannot fetch while offline");
        throw new Error("File Missing and cannot fetch while offline");
      }
    });
  });
});
