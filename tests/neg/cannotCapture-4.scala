import scala.annotation.{CannotBeCaptured, CannotCapture}
import Boo.{CanCall, Evidence, canCall, canCall2, pure, Pure, CanDo, canDo, ev2, ev2_1, ev2_2, Ev2}

object Foo {

  type requiring[R, P1 <: Evidence] = Boo.ImplicitFunction1[P1, R]
  type and[P1 <: Evidence, P2 <: Evidence] = Ev2[P1, P2]
  type pure[R] = requiring[R, Pure]



  def ff1(n: Int): pure[Unit] = {
    canCall2() // error
    ()
  }

  {
    ff1(4)
  }

  def ff2(n: Int): Unit requiring CanCall = {
    canCall2() // error
    ()
  }

  {
    implicit def b: CanCall = canCall
    ff2(3)(b)
    ff2(3)
  }

// Fails before the other ones
//  def ff3(a: Int requiring CanCall): Int = {
//    a // err
//  }

  def ff4(a: Int requiring CanCall): Int requiring CanCall = {
    a
  }

  def ff5(a: Int requiring CanCall): Int = {
    implicit def cc: CanCall = canCall
    a
  }

  def ff6(a: Int requiring (CanCall and CanDo)): Int = {
    implicit def cc: CanCall = canCall
    implicit def cd: CanDo = canDo
    a
  }

  def ff7(a: Int): Int requiring (CanCall and CanDo) = {
    canCall // error
    canDo // error
    a
  }

  def ff7(a: Int requiring (CanCall and CanDo)): Int requiring (CanCall and CanDo) = {
    a
  }

}


@CannotBeCaptured
object Boo extends Phantom {
  type Evidence <: this.Any
  type Pure = Evidence
  type CanCall <: Evidence
  type CanDo <: Evidence
  type Ev2[E1 <: Evidence, E2 <: Evidence] <: Evidence

  implicit def pure: Evidence = assume
  def canCall: CanCall = assume
  def canCall2(): CanCall = assume
  def canDo: CanDo= assume

  implicit def ev2[E1 <: Evidence, E2 <: Evidence](implicit e1: E1, e2: E2): Ev2[E1, E2] = assume
  implicit def ev2_1[E1 <: Evidence, E2 <: Evidence](implicit ev: Ev2[E1, E2]): E1 = assume
  implicit def ev2_2[E1 <: Evidence, E2 <: Evidence](implicit ev: Ev2[E1, E2]): E2 = assume


}
