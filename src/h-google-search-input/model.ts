type NEL<a> = ['NEL', a, a[]]

export function initNEL<A>(h: A, t: A[]): NEL<A> {
  return ['NEL', h, t]
}
