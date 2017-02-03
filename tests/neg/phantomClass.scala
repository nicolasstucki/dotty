
trait Foo
trait Boo extends PhantomAny

trait TraitBlinky extends PhantomAny with Foo // error: A trait cannot extend both Any and PhantomAny.
trait TraitPinky extends Foo with Boo // error: A trait cannot extend both Any and PhantomAny.
trait TraitInky extends Boo with Foo // error: A trait cannot extend both Any and PhantomAny.

abstract class AbstractClassInky extends AnyRef with Boo // error: An abstract class cannot extend both Any and PhantomAny.
abstract class AbstractClassClyde extends PhantomAny with Foo // error: An abstract class cannot extend both Any and PhantomAny.
abstract class AbstractClassBlinky2 extends Foo with Boo // error: An abstract class cannot extend both Any and PhantomAny.
abstract class AbstractClassPiniky2 extends Boo with Foo // error: An abstract class cannot extend both Any and PhantomAny.

class ClassInky extends AnyRef with Boo // error: A class cannot extend both Any and PhantomAny.
class ClassClyde extends PhantomAny with Foo // error: A class cannot extend both Any and PhantomAny.
class ClassBlinky2 extends Foo with Boo // error: A class cannot extend both Any and PhantomAny.
class ClassPiniky2 extends Boo with Foo // error: A class cannot extend both Any and PhantomAny.
