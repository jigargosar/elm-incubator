import { h, app } from 'hyperapp'
import 'tachyons'

function Model({ ct = 0 }) {
  function inc() {
    return Model({ ct: ct + 1 })
  }
  function dec() {
    return Model({ ct: ct - 1 })
  }

  function ctAsString() {
    return `${ct}`
  }

  return Object.freeze({ inc, dec, ctAsString })
}

app({
  init: Model({ ct: 0 }),
  view: model => {
    return h('div', {}, [
      h('h1', {}, model.ctAsString()),
      h('button', { onclick: model.dec }, 'subtract'),
      h('button', { onclick: model.inc }, 'add'),
    ])
  },
  node: document.getElementById('app'),
})
