import scala.annotation.{CannotBeCaptured, CannotCapture}
import Boo._

class A {
  @CannotCapture def foo: Int = ???
  def bar: Int = ???
}

class B extends A {
  override def foo: Int = { // warn
    boo // error
    42
  }
  @CannotCapture override def bar: Int = ???
}

@CannotBeCaptured
object Boo extends Phantom {
  type BooAny <: this.Any
  def boo: BooAny = assume
  def boo2(): BooAny = assume
}
