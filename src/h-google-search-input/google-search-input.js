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
    div({}, [
      viewInput('foo bar', true),
      viewSuggestions()
    ]),
    div({ tabindex: 0 }, ['after input']),
  ])
}

function viewSuggestions() {
  return false
}

function viewInput(value, isShowingSuggestions) {
  const cls = isShowingSuggestions ? 'br4 br--top shadow-1' : 'br4'
  return div({ class: ['flex ba b--moon-gray', cls] }, [
    input({
      class: 'ph3 pv2 flex-auto bn bg-transparent outline-0',
      value: value,
      autofocus: true,
    }),
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
