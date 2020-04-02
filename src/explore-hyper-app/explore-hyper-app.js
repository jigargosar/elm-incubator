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
  return div({ class: 'measure center' }, [
    div({ class: 'pv2 f2' }, [text(state.ct)]),
    button(
      {
        class: '',
        onclick: SubClicked,
      },
      ['subtract'],
    ),
    button(
      {
        onclick: AddClicked,
      },
      ['add'],
    ),
  ])
}

function text(string) {
  return `${string}`
}

function div(...args) {
  return h('div', ...args)
}

function button(...args) {
  return h('button', ...args)
}

// MAIN

{
  app({
    init: initState(),
    view: view,
    node: document.getElementById('app'),
  })
}
