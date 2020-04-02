import { h, app } from 'hyperapp'
import 'tachyons'
import { initNEL } from './model'

// SEARCH WIDGET

function swInit([originalQS, suggestionsNEL]) {
  // assertString(originalQS)
  // assertNEL(assertString, suggestionsNEL)
  return [originalQS, ['TYPED', originalQS], ['HIDDEN', suggestionsNEL]]
}

// MODEL

function initState() {
  const suggestionsNEL = initNEL('suggestion 0', [
    'suggestion 1',
    'suggestion 2',
    'suggestion 2',
    'suggestion 2',
    'suggestion 2',
  ])
  return {sw: swInit(['foo bar', suggestionsNEL])}
}

// UPDATE

// VIEW

function view(state) {
  return div({ class: 'pv3 measure center f4 lh-title' }, [
    div({ tabindex: 0 }, ['before input']),
    viewSearchWidget(),
    div({ tabindex: 0 }, ['after input']),
  ])
}

function viewSearchWidget() {
  return div({ class: 'relative' }, [
    //
    viewInput('foo bar', true),
    viewSuggestions(),
  ])
}

function viewInput(value, isShowingSuggestions) {
  const cls = isShowingSuggestions ? 'b--transparent' : ''

  const stateStyles = isShowingSuggestions
    ? { ...brTopStyles, boxShadow: '0 1px 6px 0 rgba(32, 33, 36, 0.28)' }
    : {}
  return div(
    {
      class: ['flex-auto flex ba b--moon-gray', cls],
      style: { borderRadius: '1.25rem', ...stateStyles },
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

const brTopStyles = {
  borderBottomRightRadius: 0,
  borderBottomLeftRadius: 0,
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
      div({ class: 'mh3 bt b--light-gray' }),
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
