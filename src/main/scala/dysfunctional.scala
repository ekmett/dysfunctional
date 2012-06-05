package object dysfunctional {
  def get(implicit store: State): store#S = s.get
  def put(implicit store: State): store#S => Unit = store.put(_)
  def modify(implicit store: State): (store#S => store#S) => Unit = store.modify(_)

  def ask(implicit env: Reader): env#E = s.ask
  def local(implicit env: Reader) = new {
    def apply[A](f: env#E => env#E, a: => A): A {
      env.local(f)(a)
    }
  }
  def asks(implicit env: Reader) = new {
    def apply[A](f: env#E => A): A = env.asks(f)
  }
}
