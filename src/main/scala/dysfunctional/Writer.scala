package dysfunctional

trait Writer { my =>
  type M
  type Log = Writer { type M = my#M }

  val M : Monoid[M]
  import M._

  var log: M
  def tell(m: M) { log = append(log,m) }
  def listen[A](go: => A): (A,M) = {
    val old = log
    log = empty
    val a = go
    val new = log
    log = append(log,new)
    (a, new)
  }
  def pass[A](go: => (A, M => M)): A = {
    val old = log
    log = empty
    val (a, f) = go
    val new = log
    log = append(log, f(new))
    a
  }
}

object Writer {
  def run[W,A](f: Writer { type M = W } => A)(implicit W: Monoid[W]) = {
    f(new Writer {
      val M = W
      var log = M.empty
    })
  }
}
