import dotty.phantom.PhantomAny
import dotty.phantom.Phobic
import dotty.phantom.Disallowed

object phobic {

  final class Foo
  class Bar
  val foo = new Foo
  val bar = new Bar
  val barAny: Any = new Bar
  val n = 42
  val n2: Integer = 42
  val str = "abc"

  @Phobic(Disallowed[Foo]) def baz0(foo: Foo): Unit = {
    foo
    bar
    ??? // error
  }

  @Phobic(Disallowed[Foo]) def baz1: Unit = {
    foo // error
    barAny // error
  }

  @Phobic(Disallowed[Int | Integer]) def baz2: Unit = {
    1
    n // error
    n2 // error
    barAny // error
  }

  class Qux1[T] {
    val t: T = ???
    @Phobic(Disallowed[Foo]) def baz1(t2: T): Unit = {
      foo // error
      barAny // error
      t // error
      t2
    }
  }

  class Qux2[T <: Foo] {
    val t: T = ???
    @Phobic(Disallowed[Foo]) def baz1(t2: T): Unit = {
      foo // error
      barAny // error
      t // error
      t2
    }
  }

  class Qux3[T >: Foo] {
    val t: T = ???
    @Phobic(Disallowed[Foo]) def baz1(t2: T): Unit = {
      foo // error
      barAny // error
      t // error
      t2
    }
  }

  @Phobic(Disallowed[Int]) def quux = {
    @Phobic(Disallowed[String]) def corge = {
      @Phobic(Disallowed[Foo]) def grault = {
        foo // error
        str // error
        barAny // error
        n // error
        bar
        n2
      }
    }
  }

  trait Garply
  @Phobic(Disallowed[Garply]) def garply1: Unit = () // error

  class Waldo1[T] {
    @Phobic(Disallowed[T]) def baz1: Unit = () // error

    type T2
    @Phobic(Disallowed[T2]) def baz2: Unit = () // error
  }

  @Phobic(Disallowed[AnyRef]) def onlyVals: Unit = {
    str // error
    n
    n2 // error
    foo // error
    bar // error
  }
}
