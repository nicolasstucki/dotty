object Test {
  var i: Int = 1
  def foo(unused a: Int): Int = {
    i = a // error
    unused def r = {
      i = a
      ()
    }
    42
  }
}
