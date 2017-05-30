import scala.annotation.{CannotBeCaptured, CannotCapture}
import Boo._

object Foo {

  // TODO: P should be a phantom, need phantom functions for this
  type requiring[R, P] = implicit P => R

  @CannotCapture
  def ff2(n: Int): Unit requiring Int = {
    canCall2() // error
    ()
  }

  {
    implicit def b: CanCall = canCall
    ff2(3)(4)
    implicit val a: Int = 5
    ff2(3)
  }

}


@CannotBeCaptured
object Boo extends Phantom {
  type Evidence <: this.Any
  type CanCall <: Evidence
  def canCall: CanCall = assume
  def canCall2(): CanCall = assume
}

trait StoicPhantomFunction1[-T1 <: Evidence, +R] {
  @CannotCapture def apply(x1: T1): R
}

trait StoicFunction0[+R] extends Function0[R] {
  @CannotCapture def apply(): R
}

trait StoicFunction1[-T1, +R] extends Function1[T1, R] {
  @CannotCapture def apply(x1: T1): R
}

trait StoicFunction2[-T1, -T2, +R] extends Function2[T1, T2, R] {
  @CannotCapture def apply(x1: T1, x2: T2): R
}
