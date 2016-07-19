import dotty.phantom.PhantomAny

class Blinky extends PhantomAny {
  def boo1 = new PhantomAny {} // error
  def boo2 = new Inky {} // error
  def boo3 = new Pinky {} // error
}

abstract class Inky extends PhantomAny

trait Pinky extends PhantomAny
