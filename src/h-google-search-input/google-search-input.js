import { app, h } from 'hyperapp'
import 'tachyons'
import { initSW } from './model'

// MAYBE

function Just(value) {
  return { tag: 'JUST', value }
}

function Nothing() {
  return { tag: 'NOTHING' }
}

// NON EMPTY LIST

function initNEList(h, t) {
  return [h, t]
}

function mapNEList(func, [h, t]) {
  return [func(h), t.map(func)]
}

function nelToList([h, t]) {
  return [h, ...t]
}

// LCR

function lcrMapCS(fc, fs, [l, c, r]) {
  return [l.map(fs), fc(c), r.map(fs)]
}

function lcrToList([l, c, r]) {
  return [...l, c, ...r]
}

// NON EMPTY SELECTION

function initSelection(neList) {
  return { tag: 'NONE_SELECTED', neList }
}

function selectionMapCS(funcCenter, funcOther, selection) {
  switch (selection.tag) {
    case 'NONE_SELECTED': {
      return {
        ...selection,
        neList: mapNEList(funcOther, selection.neList),
      }
    }
    case 'SELECTED': {
      return { ...selection, lcr: lcrMapCS(funcCenter, funcOther) }
    }
  }
}

function selectionToList(selection) {
  switch (selection.tag) {
    case 'NONE_SELECTED': {
      return nelToList(selection.neList)
    }
    case 'SELECTED': {
      return lcrToList(selection.lcr)
    }
  }
}

// SEARCH WIDGET

function getInputString(sw) {
  const { inputValue: iv } = sw
  switch (iv.tag) {
    case 'TYPED': {
      return iv.typed
    }
    case 'OVERRIDDEN': {
      return iv.overridden
    }
  }
}

function getVisibleSuggestionSelection(sw) {
  const ss = sw.suggestions
  if (ss.tag === 'VISIBLE') {
    return Just(ss.selection)
  } else {
    return Nothing()
  }
}

// MODEL

function initState() {
  const suggestionsNEL = initNEList('suggestion 0', [
    'suggestion 1',
    'suggestion 2',
    'suggestion 2',
    'suggestion 2',
    'suggestion 2',
  ])
  return { sw: initSW('foo bar', suggestionsNEL) }
}

// UPDATE

// VIEW

function view({ sw }) {
  return div({ class: 'pv3 measure center f4 lh-title' }, [
    div({ tabindex: 0 }, ['before input']),
    viewSearchWidget(sw),
    div({ tabindex: 0 }, ['after input']),
  ])
}

function viewSearchWidget(sw) {
  const mbSS = getVisibleSuggestionSelection(sw)
  console.log(mbSS)
  const showingSuggestions = mbSS.tag === 'JUST'
  return div({ class: 'relative' }, [
    //
    viewInput(getInputString(sw), showingSuggestions),
    showingSuggestions ? viewSuggestions(sw, mbSS.value) : '',
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

function viewSuggestions(sw, suggestionSelection) {
  const viewHighlightedSuggestion = s => viewSuggestion(true, s)
  const viewOtherSuggestion = s => viewSuggestion(false, s)
  const selectionViewItems = selectionToList(
    selectionMapCS(
      viewHighlightedSuggestion,
      viewOtherSuggestion,
      suggestionSelection,
    ),
  )

  function viewSuggestion(isSelected, s) {
    return div(
      { class: ['ph3 pv1', { 'bg-light-gray': isSelected }] },
      text(s),
    )
  }

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
      div({ class: 'pv2' }, selectionViewItems),
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
