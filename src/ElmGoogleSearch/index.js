const Module = require('./Main.elm')

require('./styles.css')
require('tachyons')

Module['Elm']['ElmGoogleSearch']['Main'].init({
  node: document.getElementById('root'),
})
