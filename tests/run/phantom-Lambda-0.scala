
object Test {
  import Boo._

  def main(args: Array[String]) = {
    (((b, i) => println(i)): ((Pinky, Int) => Unit)).apply(boo[Pinky], 42)
  }
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def boo[B <: Boo.Any]: B = assume[B]
}
