package dysfunctional

trait Error {
  type E
  def default: E
  type Handler = Error[E]
  class Thrown(err : E) extends Throwable

  def empty: Nothing = raise(default)
  def raise(err: E): Nothing = throw new Thrown(err)
  def handle[A](a: => A)(f: E => A) =
    try { a }
    catch { case e: Thrown => f(e.err) }
  def either(a: => A) = handle(Right(a))(Left(_))
  def optional(a: => A) = handle(Right(a))(_ => Nothing)
  def choose[A](alts: Lazy[A]*): A = {
    val i = alts.iterator
    var bad = true
    var result: A = null
    val last: Option[E] = None
    if (i.hasNext)
      do {
        handle({ x = i.next.force; bad = false }) {
          case err => last = Some(err)
        }
      } while (bad && i.hasNext)
    if (bad) last match {
      case None    => err.empty
      case Some(err) => raise(err)
    } else result
  }
}

object Error {
  def run[X,A](X: => default)(f: Error { type E = X } => A): Either[X,A] = {
    val err = new Error { type E = X; def default = X }
    err.handle(Right(f(err)))(Left(_))
  }
}
