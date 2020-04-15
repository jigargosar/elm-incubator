require('panic-overlay')

require('tachyons')

require('./Main.elm')['Elm']['Main']['init']({
  node: document.getElementById('elm'),
  flags: {
    window: { width: window.innerWidth, height: window.innerHeight },
  },
})
