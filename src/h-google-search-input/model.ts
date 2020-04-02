type SearchWidget = ['SW', string, InputValue, Suggestions]

export function initSW(
  q: string,
  suggestionsNEL: NEL<string>,
): SearchWidget {
  return ['SW', q, ['TYPED', q], ['HIDDEN', suggestionsNEL]]
}

type InputValue = ['TYPED', string] | ['OVERRIDDEN', string, string]

type Suggestions =
  | ['HIDDEN', NEL<string>]
  | ['VISIBLE', NeSelection<string>]

type NeSelection<a> = ['NONE_SELECTED', NEL<a>] | ['SELECTED', LCR<a>]

type LCR<a> = ['LCR', a[], a, a[]]

type NEL<a> = ['NEL', a, a[]]

export function initNEL<a>(h: a, t: a[]): NEL<a> {
  return ['NEL', h, t]
}
