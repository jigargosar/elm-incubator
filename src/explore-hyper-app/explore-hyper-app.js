import { h, app } from 'hyperapp'
import 'tachyons'

const AddClicked = state => [{ ...state, ct: state.ct + 1 }]

const SubClicked = state => [{ ...state, ct: state.ct - 1 }]

function initState() {
  return { ct: 0 }
}

function text(string) {
  return `${string}`
}

{
  app({
    init: initState(),
    view: state => {
      return h('div', {}, [
        h('h1', {}, text(state.ct)),
        h('button', { onclick: SubClicked }, 'subtract'),
        h('button', { onclick: AddClicked }, 'add'),
      ])
    },
    node: document.getElementById('app'),
  })
}
