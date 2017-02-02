import dotty.phantom.PhantomAny

object AnyObject {
  object AnyObjectBoo extends PhantomAny // error
  class AnyObjectBoo2 extends PhantomAny // error
  type AnyObjectBoo3 = PhantomAny
  type AnyObjectFoo = Int
  def boo: PhantomAny = AnyObjectBoo // error
}

class AnyClass {
  object AnyClassBoo extends PhantomAny // error
  class AnyClassBoo2 extends PhantomAny // error
  type AnyClassBoo3 = PhantomAny
  def boo: PhantomAny = AnyClassBoo // error
}

object PhantomObject extends PhantomAny {
  object PhantomObjectFoo // error
  class PhantomObjectFoo2 // error
  type PhantomObjectBoo = PhantomAny
  type PhantomObjectFoo3 = Int
  def foo: Int = 42 // error
}

class PhantomClass extends PhantomAny {
  object PhantomClassBoo extends PhantomAny // error
  class PhantomClassBoo2 extends PhantomAny // error
  type PhantomClassBoo3 = PhantomAny
  type PhantomClassFoo3 = Int
  def boo: PhantomAny = PhantomClassBoo // error

  object PhantomClassFoo // error
  class PhantomClassFoo2 // error
  def foo: Int = 42 // error
}
