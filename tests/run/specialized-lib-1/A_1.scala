object A {

  def foo[T: Specialized](x: T) = throws(x)

  def throws[U: Specialized](x: U): Unit = throw new StackCheck

}

class StackCheck extends Throwable
