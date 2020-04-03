type SearchWidget = {
  tag: 'SW'
  originalQuery: string
  inputValue: InputValue
  suggestions: Suggestions
}

type InputValue =
  | { tag: 'TYPED'; typed: string }
  | { tag: 'OVERRIDDEN'; typed: string; overridden: string }

type Suggestions =
  | { tag: 'HIDDEN'; nonEmptyList: NEList<string> }
  | { tag: 'VISIBLE'; selection: NESelection<string> }

type NESelection<a> =
  | { tag: 'NONE_SELECTED'; neList: NEList<a> }
  | { tag: 'SELECTED'; lcr: LCR<a> }

type LCR<a> = [a[], a, a[]]

type NEList<a> = [a, a[]]
