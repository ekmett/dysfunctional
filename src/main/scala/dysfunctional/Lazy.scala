package dysfunctional

class Lazy[A] {
  lazy val force: A
}

object Lazy {
  implicit def delay(a: => A): Lazy[A] = new Lazy[A] {
    lazy val force = a
  }
}

