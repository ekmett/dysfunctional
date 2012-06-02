package dysfunctional

class Writer[M](var log: M)(implicit M: Monoid[M]) {
  import M._
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
  class Module[M] {
    type Log = Writer[M]
    def tell(m: M)(implicit w: Log) = w.tell(m)
    def listen(go: => A)(implicit w: Log): (A,M)
    def pass(go: => (A, M => M))(implicit w: Log): (A,M)
  }
  def module[M] = new Module[M]
}

