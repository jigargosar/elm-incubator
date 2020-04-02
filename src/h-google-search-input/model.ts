type SearchWidget = ['SW', string, InputValue, Suggestions]

type InputValue = ['TYPED', string] | ['OVERRIDDEN', string, string]

type Suggestions =
  | ['HIDDEN', NEL<string>]
  | ['VISIBLE', NeSelection<string>]

type NeSelection<a> = ['NONE_SELECTED', NEL<a>] | ['SELECTED', LCR<a>]

type LCR<a> = ['LCR', a[], a, a[]]

type NEL<a> = ['NEL', a, a[]]

export function initNEL<A>(h: A, t: A[]): NEL<A> {
  return ['NEL', h, t]
}
