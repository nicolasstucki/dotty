case class A(i: Int)
case class B(i: Int)
class Foo {
  def foo(obj: Object): Unit = {
    obj match {
      case A(i) if i == 4 =>
      case B(i) =>
    }
  }
}