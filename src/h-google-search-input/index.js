import { app, h } from 'hyperapp'
import 'tachyons'
import S from 'sanctuary'

const Maybe = (function() {
  function Just(value) {
    return { tag: Just, value }
  }

  function Nothing() {
    throw 'Nothing is not a constructor function'
  }

  return Object.freeze({
    Just,
    Nothing,
    map: S.curry2(function map(f, mb) {
      return mb === Nothing ? mb : Just(f(mb.value))
    }),
    withDefault: S.curry2(function withDefault(defVal, mb) {
      return mb === Nothing ? defVal : mb.value
    }),
  })
})()

console.log(Maybe.map)

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
  return { tag: ':SELECTION:NONE_SELECTED', neList }
}

function selectionMapCS(funcCenter, funcOther, selection) {
  switch (selection.tag) {
    case ':SELECTION:NONE_SELECTED': {
      return {
        ...selection,
        neList: mapNEList(funcOther, selection.neList),
      }
    }
    case ':SELECTION:SELECTED': {
      return { ...selection, lcr: lcrMapCS(funcCenter, funcOther) }
    }
  }
}

function selectionToList(selection) {
  switch (selection.tag) {
    case ':SELECTION:NONE_SELECTED': {
      return nelToList(selection.neList)
    }
    case ':SELECTION:SELECTED': {
      return lcrToList(selection.lcr)
    }
  }
}

// SEARCH WIDGET

function initSearchWidget(queryString, neSuggestions) {
  return {
    tag: 'SearchWidget',
    originalQuery: queryString,
    inputValue: { tag: 'TYPED', typed: queryString },
    suggestions: initSuggestions(neSuggestions),
  }
}

function getInputString(sw) {
  const iv = sw.inputValue
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
    return Maybe.Just(ss.selection)
  } else {
    return Maybe.Nothing
  }
}

// SUGGESTIONS

function initSuggestions(neList) {
  return { tag: 'VISIBLE', selection: initSelection(neList) }
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
  return { sw: initSearchWidget('foo bar', suggestionsNEL) }
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
  const emptyView = ''
  const suggestionsView = S.pipe([
    getVisibleSuggestionSelection,
    Maybe.map(viewSuggestions),
    Maybe.withDefault(emptyView),
  ])(sw)

  const areSuggestionsVisible = suggestionsView !== emptyView

  return div({ class: 'relative' }, [
    //
    viewInput(getInputString(sw), areSuggestionsVisible),
    suggestionsView,
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

function viewSuggestions(suggestionSelection) {
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
