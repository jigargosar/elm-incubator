const Module = require('./Main.elm')

require('./styles.css')
require('tachyons')

Module['Elm']['Main'].init({
  node: document.getElementById('root'),
  flags: { now: Date.now(), bs: [window.innerWidth, window.innerHeight] },
})

