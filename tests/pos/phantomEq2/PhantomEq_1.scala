
/* This is a version of ../pos/phantomEq.scala that tests phantom clases with separate compilation */
object PhantomEqUtil {
  class PhantomEq[-L, -R] extends dotty.phantom.PhantomAny
  type PhantomEqEq[T] = PhantomEq[T, T]

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit def eqString: PhantomEqEq[String] = new PhantomEqEq[String]
  implicit def eqInt: PhantomEqEq[Int] = new PhantomEqEq[Int]
  implicit def eqDouble: PhantomEqEq[Double] = new PhantomEqEq[Double]

  implicit def eqByteNum: PhantomEq[Byte, Number] = new PhantomEq[Byte, Number]
  implicit def eqNumByte: PhantomEq[Number, Byte] = new PhantomEq[Number, Byte]

  implicit def eqSeq[T, U](implicit eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = new PhantomEq[Seq[T], Seq[U]]
}
