import { h, app } from 'hyperapp'
import 'tachyons'

// MODEL
function initState() {
  return { ct: 0 }
}

// UPDATE

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

// VIEW

function view(state) {
  return h('div', {}, [
    h('h1', {}, text(state.ct)),
    h('button', { onclick: SubClicked }, text('subtract')),
    h('button', { onclick: AddClicked }, text('add')),
  ])
}

function text(string) {
  return `${string}`
}

// MAIN

{
  app({
    init: initState(),
    view: view,
    node: document.getElementById('app'),
  })
}
