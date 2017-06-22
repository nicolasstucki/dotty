
class A2 extends A {
  override def bar[T: Specialized](x: T): Unit = baz(x)
  def baz[T: Specialized](x: T): Unit = super.bar(x)
}
