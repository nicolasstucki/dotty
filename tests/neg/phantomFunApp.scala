import dotty.phantom.PhantomAny
import dotty.phantom.PhantomNothing

class Blinky extends PhantomAny
class Pinky extends Blinky

class phantomFunApp {
  def foo(a: Any) = ???
  def boo(b: PhantomAny) = ???

  foo(1)
  foo(new Blinky) // error
  foo(new Pinky) // error

  boo(new Blinky)
  boo(new Pinky)
  boo(1) // error
  boo("abc") // error
  boo(???) // error
}
