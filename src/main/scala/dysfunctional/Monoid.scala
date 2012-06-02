package dysfunctional

trait Monoid[M] {
  def empty: M
  def append(a: M, b: M): M
}

