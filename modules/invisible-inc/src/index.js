require('tachyons')
require('./styles.css')

const app = require('./Main.elm')['Elm']['Main']['init']({
  node: document.getElementById('elm'),
  flags: {
    now: Date.now(),
    window: { width: window.innerWidth, height: window.innerHeight },
    cache: localStorage.getItem('cache') || '',
  },
})

app.ports.getBeacons.subscribe(function() {
  const xs = Array.from(document.querySelectorAll('[data-beacon]'))
  const beacons = xs.map(toBeacon)
  console.debug(beacons)
  app.ports.gotBeacons.send(beacons)

  function toBeacon(x) {
    return {
      pos: JSON.parse(x.dataset.beacon),
      rect: x.getBoundingClientRect(),
    }
  }
})

if (app.ports) {
  if (app.ports.cache) {
    app.ports.cache.subscribe(function(string) {
      localStorage.setItem('cache', string)
    })
  }
}
