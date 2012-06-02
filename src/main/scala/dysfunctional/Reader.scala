package dysfunctional

object Reader {
  class Module[E] {
    type Env = E
    def ask(implicit e: E) = e
    def local(e: E)(f: E => A) = f(e)
  }
  def module[S] = new Module[S]
}
