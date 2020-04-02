import { h, app } from 'hyperapp'
import 'tachyons'

function AddClicked(state) {
  return { ...state, ct: state.ct + 1 }
}

function SubClicked(state) {
  return { ...state, ct: state.ct - 1 }
}

function initState() {
  return { ct: 0 }
}

{
  app({
    init: initState(),
    view: state => {
      return h('div', {}, [
        h('h1', {}, state.ct),
        h('button', { onclick: SubClicked }, 'subtract'),
        h('button', { onclick: AddClicked }, 'add'),
      ])
    },
    node: document.getElementById('app'),
  })
}
