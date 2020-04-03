import { pathOr } from 'ramda'

require('./styles.css')
require('tachyons')

{
  initElmModuleWithPortHelpers(
    {
      node: document.getElementById('root'),
      flags: { now: Date.now() },
    },
    require('./Main.elm')['Elm']['ElmGoogleSearch']['Main'],
  )
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
