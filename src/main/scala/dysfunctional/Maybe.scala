package dysfunctional

object Maybe {
  val module = new Error.Module[Unit] {
    def maybe[A](f: Error[Unit] => A): Option[A] = {
      val err = new Error[E](())
      err.handle(Some(f(err)))(_ => None)
    }
  }
}
