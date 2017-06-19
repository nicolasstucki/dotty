object A {
  def throws[U: Specialized](x: U): Unit = throw new StackCheck
}

class A {
  def bar[T: Specialized](x: T): Unit = A.throws(x)
}

class StackCheck extends Throwable
