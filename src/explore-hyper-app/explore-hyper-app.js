import { h, app } from 'hyperapp'
import 'tachyons'

const AddClicked = function(state) {
  return {
    ...state,
    ct: state.ct + 1,
  }
}

function SubClicked(state) {
  return {
    ...state,
    ct: state.ct - 1,
  }
}

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
        h('button', { onclick: SubClicked }, text('subtract')),
        h('button', { onclick: AddClicked }, text('add')),
      ])
    },
    node: document.getElementById('app'),
  })
}
