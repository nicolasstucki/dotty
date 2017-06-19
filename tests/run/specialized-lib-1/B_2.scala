object B {

  def foo[T: Specialized](x: T) = A.throws(x)

  def throws2[U: Specialized](x: U): Unit = throw new StackCheck

}

class B extends A {
  override def bar[T: Specialized](x: T): Unit = B.throws2(x)
}
