package dysfunctional

trait State {
  type S
  var state: S
  type Store = State { S = S }
  def get = state
  def put(s: S) { state = s }
  def modify(f: S => S) { state = f(state) }
}

object State {
  def run[T,A](s: T)(f: State { type S = T } => A) = f(new State { type S = T; var state = s })
}

