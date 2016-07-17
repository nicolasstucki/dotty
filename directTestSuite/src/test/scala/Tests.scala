

import org.junit.Test

class Tests {

  @Test def bar(): Unit = {

  }
  
  @Test def foo(): Unit = {
    DirectTest.compile(Nil, List(
        DirectTest.toFile("""class Foo { }""")))
  }
}

