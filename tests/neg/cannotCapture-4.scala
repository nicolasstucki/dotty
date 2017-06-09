import scala.annotation.{CannotBeCaptured, CannotCapture}
import Capabilities._

object Foo {

  type requiring[R, P1 <: Evidence] = Capabilities.ImplicitFunction1[P1, R]
  type and[P1 <: EffectEvidence, P2 <: EffectEvidence] = PackedEvindence[P1, P2]
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

//// Fails before the other ones
////  def ff3(a: Int requiring CanCall): Int = {
////    a // err
////  }
//
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

  def ff8(a: Int requiring (CanCall and CanDo)): Int requiring (CanCall and CanDo) = {
    a
  }

  def ff9(a: => Int requiring (CanCall and CanDo)): Int requiring (CanCall and CanDo) = {
    a
  }

  def ff10(a: Int requiring CanCall): Int requiring (CanCall and CanDo) = {
    a // error // FIXME ev2_1 gets captured
  }

  def ff11(a: Int requiring CanDo): Int requiring (CanCall and CanDo) = {
    a // error // FIXME ev2_2 gets captured
  }

}


@CannotBeCaptured
object Capabilities extends Phantom {

  type Evidence <: this.Any

  type EffectEvidence <: Evidence

  type Pure = EffectEvidence

  type CanCall <: EffectEvidence
  type CanDo <: EffectEvidence


  implicit def pure: Pure = assume
  def canCall: CanCall = assume
  def canCall2(): CanCall = assume
  def canDo: CanDo= assume

  type PackedEvindence[E1 <: Evidence, E2 <: Evidence] <: Evidence
  implicit def ev2[E1 <: Evidence, E2 <: Evidence](implicit e1: E1, e2: E2): PackedEvindence[E1, E2] = assume

  /* ev2_1 and ev2_2 get captured at unboxing site. Could allow them as special case. */
  implicit def ev2_1[E1 <: Evidence, E2 <: Evidence](implicit ev: PackedEvindence[E1, E2]): E1 = assume
  implicit def ev2_2[E1 <: Evidence, E2 <: Evidence](implicit ev: PackedEvindence[E1, E2]): E2 = assume

}
