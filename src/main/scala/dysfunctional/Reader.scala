package dysfunctional

class Reader {
  type E
  type Env = Reader[E]
  var env: E
  def ask: E = env
  def asks[A](f: E => A) = f(env)
  def local(e: E)(go: => A): A = {
    val old = env
    env = e
    val result = go
    env = old
    result
  }
}
object Reader {
  def run[X,A](e: X)(f: Reader { type E = X } => A) = f(new Reader { type E = X; var env = e })
}
