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
    div({ class: 'relative' }, [
      //
      viewInput('foo bar', true),
      viewSuggestions(),
    ]),
    div({ tabindex: 0 }, ['after input']),
  ])
}

const brTopStyles = { borderBottomRightRadius: 0, borderBottomLeftRadius: 0 }

function viewInput(value, isShowingSuggestions) {
  const cls = isShowingSuggestions ? 'b--transparent shadow-1' : ''
  const style = isShowingSuggestions
    ? { ...brTopStyles, boxShadow: '0 1px 6px 0 rgba(32, 33, 36, 0.28)' }
    : {}
  return div(
    {
      class: ['flex-auto flex ba b--moon-gray', cls],
      style: { borderRadius: '1.25rem', ...style },
    },
    [
      input({
        class: 'ph3 pv2 flex-auto bn bg-transparent outline-0',
        value: value,
        autofocus: true,
      }),
    ],
  )
}

function viewSuggestions() {
  const sl = [
    'suggestion 1',
    'suggestion 2',
    'suggestion 2',
    'suggestion 2',
    'suggestion 2',
  ]

  return div(
    {
      class: 'absolute w-100 bg-white br4 br--bottom overflow-hidden',
      style: {
        top: '100%',
        boxShadow: '0 4px 6px 0 rgba(32, 33, 36, 0.28)',
      },
    },
    [
      div({ class: 'mh3 bt' }),
      div(
        { class: 'pv2' },
        sl.map(s =>
          div({ class: ['ph3 pv1', { 'bg-light-gray': false }] }, text(s)),
        ),
      ),
    ],
  )
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
