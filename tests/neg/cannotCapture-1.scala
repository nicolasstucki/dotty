import scala.annotation.{CannotBeCaptured, CannotCapture}

object Foo {
  import Boo._

  val a = 44
  val b = Boo.boo
  val c = new Bar

  @CannotCapture
  def foo(boo3: BooAny) = {
    a
    b // error
    boo // error
    boo2() // error
    boo3
    c.bar // error
    val d = new Bar
    d.bar // error

    val boo4 = boo3

    def innerFoo() = {
      a
      b // error
      boo // error
      boo2() // error
      boo3
      boo4
      c.bar // error
      d.bar // error
    }
    () => {
      a
      b // error
      boo // error
      boo2() // error
      boo3
      boo4
      c.bar // error
      d.bar // error
    }
    ()
  }

  class Bar {
    def bar: BooAny = boo
  }

  @CannotBeCaptured
  object Boo extends Phantom {
    type BooAny <: this.Any
    def boo: BooAny = assume
    def boo2(): BooAny = assume
  }
}
