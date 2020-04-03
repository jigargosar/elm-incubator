import { pathOr } from 'ramda'
const Module = require('./Main.elm')

require('./styles.css')
require('tachyons')

{
  Module['Elm']['ElmGoogleSearch']['Main'].init({
    node: document.getElementById('root'),
  })
}

function initElmModuleWithPortHelpers(initParams, main) {
  const app = main.init(initParams)
  function subscribe(portName, callback) {
    pathOr(() => console.error(`${portName}.subscribe Port Not Found`), [
      'ports',
      portName,
      'subscribe',
    ])(app)(callback)
  }

  return [app, subscribe]
}
