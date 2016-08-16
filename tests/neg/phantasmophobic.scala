import dotty.phantom.PhantomAny
import dotty.phantom.Phantasmophobic

object phantasmophobic {

  class Boo extends PhantomAny

  object Boo extends Boo {
    def boo1 = new Boo
    def boo2() = new Boo
    def boo3[T] = new Boo
  }

  class Foo {
    def boo1 = new Boo
    def boo2() = new Boo
    def boo3[T] = new Boo
  }
  object Foo extends Foo

  val foo0 = new Foo

  def boo1 = new Boo
  def boo2() = new Boo
  def boo3[T] = new Boo

  @Phantasmophobic def foo0(boo: Boo): Unit = {
    requireBoo(boo)
  }

  @Phantasmophobic def foo1(): Unit = {
    requireBoo(boo1) // error
    requireBoo(boo2) // error
    requireBoo(boo3) // error
  }

  @Phantasmophobic def foo2(): Boo = {
    Boo // error
  }

  @Phantasmophobic def foo3(): Boo = {
    new Boo // error
  }

  def foo4(boo0: Boo): Unit = {
    def boo1 = new Boo
    def boo2() = new Boo
    def boo3[T] = new Boo
    @Phantasmophobic def foo(): Unit = {
      requireBoo(boo0) // error
      requireBoo(boo1) // error
      requireBoo(boo2) // error
      requireBoo(boo3) // error
    }
  }

  @Phantasmophobic def foo5(): Unit = {
    def boo = new Boo // error
    requireBoo(boo)
  }

  @Phantasmophobic def foo6() = {
    boo2 _ // error
  }

  @Phantasmophobic def foo7(foo: Foo) = {
    requireBoo(foo0.boo1) // error
    requireBoo(foo0.boo2) // error
    requireBoo(foo0.boo3) // error
  }

  @Phantasmophobic def foo8 = {
    requireBoo(Foo.boo1) // error
    requireBoo(Foo.boo2) // error
    requireBoo(Foo.boo3) // error
  }

  @Phantasmophobic def foo9 = {
    @Phantasmophobic def foo9Inner = {
      requireBoo(boo1) // error
      requireBoo(boo2) // error
      requireBoo(boo3) // error
    }
  }

  @Phantasmophobic def foo10(foo: Foo) = {
    requireBoo(foo.boo1) // error
    requireBoo(foo.boo2) // error
    requireBoo(foo.boo3) // error
  }

  class Bar {
    @Phantasmophobic def foo1 = ()
    @Phantasmophobic def foo2 = ()
    def foo3 = ()
  }

  class Baz extends Bar {
    @Phantasmophobic override def foo1: Unit = {
      requireBoo(boo1) // error
    }
    override def foo2: Unit = {
      requireBoo(boo1) // error
    }
    @Phantasmophobic override def foo3: Unit = {
      requireBoo(boo1) // error
    }
  }

  @Phantasmophobic class Egon(boo: Boo) {
    requireBoo(boo)
    def this(n: Int) = {
      this(boo1) // error
    }
    def foo = {
      requireBoo(boo)
      requireBoo(boo1) // error
    }

  }

  @Phantasmophobic abstract class Peter {
    requireBoo(boo1) // error
    def foo =
      requireBoo(boo1) // error
  }

  @Phantasmophobic trait Ray {
    requireBoo(boo1) // error
    def foo =
      requireBoo(boo1) // error
  }

  @Phantasmophobic object Winston {
    requireBoo(boo1) // error
    def foo =
      requireBoo(boo1) // error
  }
  
  def requireBoo(boo: Boo): Unit = ()

}
