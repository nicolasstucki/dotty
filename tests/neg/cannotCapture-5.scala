import scala.annotation.{CannotBeCaptured, CannotCapture}
import Boo._

object Foo {

  type requiring[R, P1 <: Evidence] = Boo.ImplicitFunction1[P1, R]
  type pure[R] = requiring[R, Evidence]

  def map[E <: Evidence](n: Int, f: Int => Int requiring E): Int requiring E = {
    f(n)
  }

  def pureMap(n: Int, f: Int => pure[Int]): pure[Int] = {
    f(n)
  }

  pureMap(4, n => {
    4
  })

  {
    val f: Int => pure[Int] = n => 3
    map[Pure](5, f) // FIXME: inferes Boo.Nothing
  }

  {
    val f: Int => Int requiring CanCall = n => 4
    implicit def e: CanCall = canCall
    map(5, f)
  }

  {
    val f: Int => Int requiring CanCall = n => 6
    map(7, f) // error
  }
}


@CannotBeCaptured
object Boo extends Phantom {
  type Evidence <: this.Any
  type Pure = Evidence
  type CanCall <: Evidence

  implicit def pure: Evidence = assume
  def canCall: CanCall = assume
}
