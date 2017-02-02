
/* This is a example of how to implement Eq using erasable phantom types.
 *
 * Run this test with
 *   `run tests/pos/phantomEq.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomParamErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 *
 * See also: ../neg/phantomEq.scala
 */
import dotty.phantom.PhantomAny

object PhantomEq {
  import PhantomEqUtil._
  import EqUtil._

  "ghi" === "jkl"
  3 === 4
  2.0 === 3.1

  List(1, 2) === Nil
  List(1, 2) === Vector(1, 2)

  1.toByte === (1: Number)
  (1: Number) === 1.toByte
}

object EqUtil {
  import PhantomEqUtil._
  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ce: PhantomEq[T, U]) = x.equals(y)
  }
}

object PhantomEqUtil extends PhantomAny {
  class PhantomEq[-L, -R] extends PhantomAny
  type PhantomEqEq[T] = PhantomEq[T, T]

  implicit def eqString: PhantomEqEq[String] = new PhantomEqEq[String]
  implicit def eqInt: PhantomEqEq[Int] = new PhantomEqEq[Int]
  implicit def eqDouble: PhantomEqEq[Double] = new PhantomEqEq[Double]

  implicit def eqByteNum: PhantomEq[Byte, Number] = new PhantomEq[Byte, Number]
  implicit def eqNumByte: PhantomEq[Number, Byte] = new PhantomEq[Number, Byte]

  implicit def eqSeq[T, U](implicit eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = new PhantomEq[Seq[T], Seq[U]]
}
