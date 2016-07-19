import dotty.phantom.PhantomAny

class Blinky extends PhantomAny {

  def boo1() = new Blinky // error
  val boo2 = new Blinky // error

  var boo3 = new Blinky // error
  lazy val boo4 = new Blinky // error

  def foo1() = 42 // error
  val foo2 = 42 // error
  var foo3 = 42 // error
  lazy val foo4 = 42 // error

}

abstract class Inky extends PhantomAny {

  def boo1() = new Blinky // error
  val boo2 = new Blinky // error

  var boo3 = new Blinky // error
  lazy val boo4 = new Blinky // error

  def foo1() = 42 // error
  val foo2 = 42 // error
  var foo3 = 42 // error
  lazy val foo4 = 42 // error

}

trait Pinky extends PhantomAny {

  def boo1() = new Blinky // error
  val boo2 = new Blinky // error

  var boo3 = new Blinky // error
  lazy val boo4 = new Blinky // error

  def foo1() = 42 // error
  val foo2 = 42 // error
  var foo3 = 42 // error
  lazy val foo4 = 42 // error

}