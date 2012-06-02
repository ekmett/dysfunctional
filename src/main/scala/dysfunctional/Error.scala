package dysfunctional

class Error[E](val default: E) {
  class Thrown(err : E) extends Throwable
  def empty: Nothing = raise(default)
  def raise(e: E): Nothing = throw new Thrown(e)
  def handle[A](a: => A)(f: E => A) =
    try { a }
    catch { case e: Thrown => f(e.err) }
}

object Error {
  class Module[E] {
    type Handler = Error[E]
    def empty(implicit err: Handler) = err.empty
    def either(a: => A)(implicit err: Handler) = err.handle(Right(a))(Left(_))
    def optional(a: => A)(implicit err: Handler) = err.handle(Right(a))(_ => Nothing)
    def choose[A](alts: Lazy[A]*)(implicit err: Handler): A = {
      val i = alts.iterator
      var bad = true
      var result: A = null
      val last: Option[E] = None
      if (i.hasNext)
        do {
          err.handle({ x = i.next.force; bad = false }) {
            case e => last = Some(e)
          }
        } while (bad && i.hasNext)
      if (bad) last match {
        case None    => err.empty
        case Some(e) => raise(e)
      } else result
    }
    def withDefault[A](E: default)(f: Handler => A): Either[E,A] = {
      val err = new Error[E](default)
      err.handle(Right(f(err)))(Left(_))
    }
  }
  def module[E] = new Module[E]
}

