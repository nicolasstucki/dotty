object B {
  def a = {
    A.f({
      println(); "a"
    }, {
      println(); "b"
    },
      "c"
    )
  }

}