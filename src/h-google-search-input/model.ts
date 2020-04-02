export type SearchWidget = ['SW', string, InputValue, Suggestions]

export function initSW(
  q: string,
  suggestionsNEL: NEL<string>,
): SearchWidget {
  return ['SW', q, ['TYPED', q], ['HIDDEN', suggestionsNEL]]
}

export type InputValue = ['TYPED', string] | ['OVERRIDDEN', string, string]

export function ivToString(iv: InputValue): string {
  switch (iv[0]) {
    case 'TYPED':
      return iv[1]

    case 'OVERRIDDEN':
      return iv[2]
  }
}

type Suggestions =
  | ['HIDDEN', NEL<string>]
  | ['VISIBLE', NeSelection<string>]

export function areSuggestionsVisible([, , , ss]: SearchWidget) {
  switch (ss[0]) {
    case 'HIDDEN':
      return false
    case 'VISIBLE':
      return true
  }
}

type NeSelection<a> = ['NONE_SELECTED', NEL<a>] | ['SELECTED', LCR<a>]

type LCR<a> = ['LCR', a[], a, a[]]

type NEL<a> = ['NEL', a, a[]]

export function initNEL<a>(h: a, t: a[]): NEL<a> {
  return ['NEL', h, t]
}
