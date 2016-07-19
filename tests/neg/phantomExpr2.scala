import dotty.phantom.PhantomAny

object Blinky extends PhantomAny
object Inky extends PhantomAny

class Foo {
  def fooTry1 = try { } finally { Blinky } // error
}
