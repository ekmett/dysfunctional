package dysfunctional

object ++ {
  def unapply[A,B](p: (A,B)) = Some(p)
}
