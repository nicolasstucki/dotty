import dotty.phantom._

class Casper extends PhantomAny

object PhantomLambdas {

  ((b, i) => ()): ((Casper, Int) => Unit) // error: Function type inputs cannot have both phantom and non phantom parameters. Consider currying the parameters.

  val foo = (b: Casper, i: Int) => new Casper // error: Function type inputs cannot have both phantom and non phantom parameters. Consider currying the parameters.
}
