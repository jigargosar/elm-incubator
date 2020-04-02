export type SearchWidget = ['SW', string, InputValue, Suggestions]

type InputValue = ['TYPED', string] | ['OVERRIDDEN', string, string]

type Suggestions =
  | ['HIDDEN', NEL<string>]
  | ['VISIBLE', NeSelection<string>]

export function initSW(
  q: string,
  suggestionsNEL: NEL<string>,
): SearchWidget {
  return ['SW', q, ['TYPED', q], ['HIDDEN', suggestionsNEL]]
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

export function mapVisibleSuggestion(f1, f2, [, , , ss]: SearchWidget) {
  switch (ss[0]) {
    case 'HIDDEN':
      break
    case 'VISIBLE':
      return selectionMapCS(f1, f2, ss[1])
  }
}

type F1<a, b> = (a: a) => b

function selectionMapCS<a, b>(
  f1: F1<a, b>,
  f2: F1<a, b>,
  nes: NeSelection<a>,
): NeSelection<b> {
  switch (nes[0]) {
    case 'NONE_SELECTED': {
      return ['NONE_SELECTED', mapNEL(f2, nes[1])]
    }
    case 'SELECTED': {
      return ['SELECTED', mapCS(f1, f2, nes[1])]
    }
  }
}

type NeSelection<a> = ['NONE_SELECTED', NEL<a>] | ['SELECTED', LCR<a>]

type LCR<a> = ['LCR', a[], a, a[]]

function mapCS<a, b>(
  funcC: F1<a, b>,
  funcS: F1<a, b>,
  [, l, c, r]: LCR<a>,
): LCR<b> {
  return ['LCR', l.map(funcS), funcC(c), r.map(funcS)]
}

type NEL<a> = ['NEL', a, a[]]

function mapNEL<a, b>(f1: F1<a, b>, [, h, t]: NEL<a>): NEL<b> {
  return ['NEL', f1(h), t.map(f1)]
}

export function initNEL<a>(h: a, t: a[]): NEL<a> {
  return ['NEL', h, t]
}
