import dotty.phantom.PhantomAny

class Blinky extends PhantomAny {

  def this(p: PhantomAny) = { // error: Can not have secondary constructors in phantom class.
    this()
  }

  def this(n: Int) = { // error: Can not have secondary constructors in phantom class.
    this()
  }

  def foo(n: Int) = new Blinky // error: Can not define methods in phantom class.
  def foo() = 2 // error: Can not define methods in phantom class.

  new Blinky // error: Phantom classes can not have expressions in statement position.
  println() // error: Phantom classes can not have expressions in statement position.
  System.currentTimeMillis() // error: Phantom classes can not have expressions in statement position.

  class Inky extends PhantomAny {

    def this(p: PhantomAny) = { // error: Can not have secondary constructors in phantom class.
      this()
    }

    def this(n: Int) = { // error: Can not have secondary constructors in phantom class.
      this()
    }

  }
}

class Pinky(p: PhantomAny) extends PhantomAny // error: Can not have parameters in constructor of a phantom class.

class Clyde(n: Int) extends PhantomAny // error: Can not have parameters in constructor of a phantom class.

object Casper extends PhantomAny {
  var foo1 = 42 // error: Phantom classes can not have 'var' fields.
  lazy val foo2 = 42 // error: Phantom classes can not have 'lazy val' fields.
  val foo3 = 42 // error: Phantom classes can not have 'val' fields.
  def foo4 = 42 // error: Phantom modules can only have methods that return a phantom value.

  var boo1 = Casper // error: Phantom classes can not have 'var' fields.
  lazy val boo2 = Casper // error: Phantom classes can not have 'lazy val' fields.
  val boo3 = Casper // error: error: Phantom classes can not have 'val' fields.

  def boo4 = Casper
}