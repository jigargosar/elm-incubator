export type SearchWidget = ['SW', string, InputValue, Suggestions]

type InputValue = ['TYPED', string] | ['OVERRIDDEN', string, string]

type Suggestions =
  | ['HIDDEN', NEL<string>]
  | ['VISIBLE', NeSelection<string>]

export function initSW(
  q: string,
  suggestionsNEL: NEL<string>,
): SearchWidget {
  return [
    'SW',
    q,
    ['TYPED', q],
    ['VISIBLE', selectionFromNEL(suggestionsNEL)],
  ]
}

export function getInputValue([, , iv]: SearchWidget): string {
  switch (iv[0]) {
    case 'TYPED':
      return iv[1]

    case 'OVERRIDDEN':
      return iv[2]
  }
}

export function areSuggestionsVisible([, , , ss]: SearchWidget) {
  switch (ss[0]) {
    case 'HIDDEN':
      return false
    case 'VISIBLE':
      return true
  }
}

export function mapVisibleSuggestionsToList<a, b>(
  funcC: F1<string, b>,
  funcS: F1<string, b>,
  [, , , ss]: SearchWidget,
): b[] | undefined {
  switch (ss[0]) {
    case 'HIDDEN':
      break
    case 'VISIBLE':
      const newSel = selectionMapCS(funcC, funcS, ss[1])
      return selectionToList(newSel)
  }
}

type F1<a, b> = (a: a) => b

type NeSelection<a> = ['NONE_SELECTED', NEL<a>] | ['SELECTED', LCR<a>]

function selectionFromNEL<a>(suggestionsNEL: NEL<a>): NeSelection<a> {
  return ['NONE_SELECTED', suggestionsNEL]
}

function selectionMapCS<a, b>(
  funcC: F1<a, b>,
  funcS: F1<a, b>,
  nes: NeSelection<a>,
): NeSelection<b> {
  switch (nes[0]) {
    case 'NONE_SELECTED': {
      return ['NONE_SELECTED', mapNEL(funcS, nes[1])]
    }
    case 'SELECTED': {
      return ['SELECTED', mapCS(funcC, funcS, nes[1])]
    }
  }
}

function selectionToList<a, b>(nes: NeSelection<a>): a[] {
  switch (nes[0]) {
    case 'NONE_SELECTED': {
      return nelToList(nes[1])
    }
    case 'SELECTED': {
      return lcrToList(nes[1])
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

type NEL<a> = [a, a[]]

function mapNEL<a, b>(f1: F1<a, b>, [h, t]: NEL<a>): NEL<b> {
  return [f1(h), t.map(f1)]
}

export function initNEL<a>(h: a, t: a[]): NEL<a> {
  return [h, t]
}

function nelToList<a>([h, t]: NEL<a>): a[] {
  return [h, ...t]
}
