package dysfunctional

object Maybe {
  def run[A](f: Error[Unit] => A): Option[A] = {
    val err = new Error[E](())
    err.handle(Some(f(err)))(_ => None)
  }
}
