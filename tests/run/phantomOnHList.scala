import dotty.phantom.PhantomAny

/* Run this test with
 *   `run tests/run/phantomOnHList.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 *
 * Disclaimer: This is only a prototype of HList that uses phantom evidences for the append operation
 */
object Test {


  def main(args: Array[String]): Unit = {
    println(HNil)

    val l1: String :: HNil = HList1("s")
    println(l1)

    val l3: Double :: Double :: Double :: HNil = HList3(1d, 2d, 3d)
    println(l3)

    val l4: String :: Double :: Double :: Double :: HNil = HListN[String, Double :: Double :: Double :: HNil](Array("s", 1d, 2d, 3d))
    println(l4)
  }

}

// HList types ------------------------------------------------------------------------------------

sealed trait HList { def underlying: Array[Any] }
sealed trait ::[H, T <: HList] extends HList // Should be [+H, +T <: HList], see #1500
sealed trait HNil extends HList

// HList values -----------------------------------------------------------------------------------

final case object HNil extends HNil {
  val underlying: Array[Any] = Array.empty[Any]
  override def toString(): String = "()"
}

// Case class based HLists for small sizes --------------------------------------------------------

final case class HList1[T1](e1: T1) extends (T1 :: HNil) {
  def underlying: Array[Any] = Array(e1)
  override def toString(): String = s"($e1,)"
}

final case class HList2[T1, T2](e1: T1, e2: T2) extends (T1 :: T2 :: HNil) {
  def underlying: Array[Any] = Array(e1, e2)
  override def toString(): String = s"($e1, $e2)"
}

final case class HList3[T1, T2, T3](e1: T1, e2: T2, e3: T3) extends (T1 :: T2 :: T3 :: HNil) {
  def underlying: Array[Any] = Array(e1, e2, e3)
  override def toString(): String = s"($e1, $e2, $e3)"
}

// Array based HLists for large sizes -------------------------------------------------------------

final case class HListN[H, T <: HList](underlying: Array[Any]) extends AnyVal with (H :: T) {
  override def toString() = underlying.mkString("(", ", ", ")")

  override def equals(o: Any): Boolean =
    o match {
      case l: HListN[_, _] => l.underlying.sameElements(underlying)
      case _ => false
    }

  override def hashCode: Int = {
    var r = 1
    for (e <- underlying)
      r = 31 * r + e.##
    r
  }
}

object HListUnapply {
  def unapplySeq[L <: HList](l: L): Option[Seq[Any]] = Some(l.underlying)
}

// Low level (Array based) HLists Appender --------------------------------------------------------

trait Appender[L1 <: HList, L2 <: HList] {
  type Out <: HList
  def apply(l1: L1, l2: L2): Out
}

object Appender {
  type Aux[L1 <: HList, L2 <: HList, O <: HList] = Appender[L1, L2] { type Out = O }

  implicit def lowLevelAppender[L1 <: HList, L2 <: HList, O <: HList](implicit p: PhantomAppender.Aux[L1, L2, O]): Appender[L1, L2] { type Out = O } =
    new Appender[L1, L2] {
      type Out = p.Out
      def apply(l1: L1, l2: L2): Out = HListN(Array.concat(l1.underlying, l2.underlying)).asInstanceOf[Out]
    }
}

// Type level "only" computation of type Out ------------------------------------------------------

trait PhantomAppender[L1 <: HList, L2 <: HList] extends PhantomAny { type Out <: HList }
object PhantomAppender extends PhantomAny {
  type Aux[L1 <: HList, L2 <: HList, O <: HList] =     PhantomAppender[L1, L2] { type Out = O }
  def  aux[L1 <: HList, L2 <: HList, O <: HList] = new PhantomAppender[L1, L2] { type Out = O }

  implicit def caseHNil[L <: HList]: Aux[HNil, L, L] = aux
  implicit def caseHCons[H, T <: HList, L <: HList, O <: HList](implicit p: Aux[T, L, O]): Aux[H :: T, L, H :: O] = aux
}
