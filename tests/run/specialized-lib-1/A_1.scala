object A {

  def throws[U: Specialized](x: U): Unit = throw new StackCheck

}

class StackCheck extends Throwable
