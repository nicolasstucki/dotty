object B {

  def foo[T: Specialized](x: T) = A.throws(x)

}
