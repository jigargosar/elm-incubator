import { h, app } from 'hyperapp'
import 'tachyons'

// MODEL

function initState() {
  return { ct: 0 }
}

// UPDATE

// VIEW

function view(state) {
  return div({ class: 'pv3 measure center f4 lh-title' }, [
    div({ tabindex: 0 }, ['before input']),
    div({ class: 'flex ba br4 b--moon-gray br--top shadow-1' }, [
      input({
        class: 'ph3 pv2 flex-auto bn bg-transparent outline-0',
        value: 'foo bar',
        autofocus: true,
      }),
    ]),
    div({ tabindex: 0 }, ['after input']),
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
function input(options) {
  return h('input', options)
}

// MAIN

{
  app({
    init: initState(),
    view: view,
    node: document.getElementById('app'),
  })
}
