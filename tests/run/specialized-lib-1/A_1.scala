object A {
  def throws[U: Specialized](x: U): Unit = throw new StackCheck
}

class A {
  def bar[T: Specialized](x: T): Unit = A.throws(x)
}

class A2 extends A {
  override def bar[T: Specialized](x: T): Unit = baz(x)
  def baz[T: Specialized](x: T): Unit = super.bar(x)
}

class StackCheck extends Throwable
