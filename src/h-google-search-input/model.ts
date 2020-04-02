export type SearchWidget = {
  tag: 'SW'
  originalQuery: string
  inputValue: InputValue
  suggestions: Suggestions
}

type InputValue = ['TYPED', string] | ['OVERRIDDEN', string, string]

type Suggestions =
  | { tag: 'HIDDEN'; nonEmptyList: NEList<string> }
  | { tag: 'VISIBLE'; selection: NeSelection<string> }

export function initSW(
  q: string,
  suggestionsNEL: NEList<string>,
): SearchWidget {
  return {
    tag: 'SW',
    originalQuery: q,
    inputValue: ['TYPED', q],
    suggestions: { tag: 'VISIBLE', selection: selectionFromNEL(suggestionsNEL) },
  }
}

export function getInputValue({ inputValue }: SearchWidget): string {
  switch (inputValue[0]) {
    case 'TYPED':
      return inputValue[1]

    case 'OVERRIDDEN':
      return inputValue[2]
  }
}

export function areSuggestionsVisible({ suggestions }: SearchWidget) {
  switch (suggestions.tag) {
    case 'HIDDEN':
      return false
    case 'VISIBLE':
      return true
  }
}

export function mapVisibleSuggestionsToList<a, b>(
  funcC: F1<string, b>,
  funcS: F1<string, b>,
  { suggestions }: SearchWidget,
): b[] | undefined {
  switch (suggestions.tag) {
    case 'HIDDEN':
      break
    case 'VISIBLE':
      const newSel = selectionMapCS(funcC, funcS, suggestions.selection)
      return selectionToList(newSel)
  }
}

type F1<a, b> = (a: a) => b

type NeSelection<a> =
  | { tag: 'NONE_SELECTED'; neList: NEList<a> }
  | { tag: 'SELECTED'; lcr: LCR<a> }

function selectionFromNEL<a>(neList: NEList<a>): NeSelection<a> {
  return { tag: 'NONE_SELECTED', neList: neList }
}

function selectionMapCS<a, b>(
  funcC: F1<a, b>,
  funcS: F1<a, b>,
  sel: NeSelection<a>,
): NeSelection<b> {
  switch (sel.tag) {
    case 'NONE_SELECTED': {
      return { ...sel, neList: mapNEL(funcS, sel.neList) }
    }
    case 'SELECTED': {
      return { ...sel, lcr: mapCS(funcC, funcS, sel.lcr) }
    }
  }
}

function selectionToList<a, b>(selection: NeSelection<a>): a[] {
  switch (selection.tag) {
    case 'NONE_SELECTED': {
      return nelToList(selection.neList)
    }
    case 'SELECTED': {
      return lcrToList(selection.lcr)
    }
  }
}

type LCR<a> = [a[], a, a[]]

function lcrToList<a>([l, c, r]: LCR<a>): a[] {
  return [...l.reverse(), c, ...r]
}

function mapCS<a, b>(
  funcC: F1<a, b>,
  funcS: F1<a, b>,
  [l, c, r]: LCR<a>,
): LCR<b> {
  return [l.map(funcS), funcC(c), r.map(funcS)]
}

type NEList<a> = [a, a[]]

function mapNEL<a, b>(f1: F1<a, b>, [h, t]: NEList<a>): NEList<b> {
  return [f1(h), t.map(f1)]
}

export function initNEL<a>(h: a, t: a[]): NEList<a> {
  return [h, t]
}

function nelToList<a>([h, t]: NEList<a>): a[] {
  return [h, ...t]
}
