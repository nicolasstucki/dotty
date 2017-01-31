import dotty.phantom.PhantomAny

class Slimer extends PhantomAny

import collection.mutable.ListBuffer

class CanDoTransaction extends PhantomAny

object Test {
  type Transactional[T] = implicit CanDoTransaction => T

  trait TransOps {
    def f1(x: Int): Transactional[Int]
    def f2(x: Int): Transactional[Int]
    def f3(x: Int): Transactional[Int]
  }

  object TransOpsObj extends TransOps {

    def f1(x: Int): Transactional[Int] = {
      println(s"first step: $x")
      f2(x + 1)
    }
    def f2(x: Int): Transactional[Int] = {
      println(s"second step: $x")
      f3(x * x)
    }
    def f3(x: Int): Transactional[Int] = {
      println(s"third step: $x")
      x
    }
  }

  val transOps: TransOps = TransOpsObj

  def transaction[T](op: Transactional[T]) = {
    implicit def trans: CanDoTransaction = new CanDoTransaction
    op
  }

  def main(args: Array[String]): Unit = {
    transaction {
      println(transOps.f1(1))
      println(transOps.f2(2))
      println(transOps.f3(3))
    }
  }
}
