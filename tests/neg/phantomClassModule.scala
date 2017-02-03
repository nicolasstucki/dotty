
class Boo
object Boo

class Boo2 extends PhantomAny
object Boo2 // error: Module of a phantom class must be phantom.

class Boo3
object Boo3 extends PhantomAny // error: Module of a non phantom class cannot be phantom.

class Boo4 extends PhantomAny
object Boo4 extends PhantomAny

class Boo5

object Boo6