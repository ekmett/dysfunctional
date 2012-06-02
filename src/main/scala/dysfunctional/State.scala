package dysfunctional

class State[S](var state: S) {
  def get = state
  def put(s: S) { state = s }
  def modify(f: S => S) { state = f(state) }
}

object State {
  class Module[S] {
    type Store = State[S]
    def get(implicit e: State[S]) = e.state
    def put(s: S)(implicit e: State[S]) { e.state = s }
    def modify(f: S => S)(implicit e: State[S]) { e.state = f(e.state) }
    def withState(s: S)(f: State[S] => A) = f(new StateEnv[S](s))
  }
  def module[S] = new Module[S]
}

