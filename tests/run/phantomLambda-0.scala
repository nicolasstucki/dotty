
class Casper extends PhantomAny

object Test {
  def main(args: Array[String]) = {
    (((b, i) => println(i)): ((Casper, Int) => Unit)).apply(new Casper, 42)
  }
}
