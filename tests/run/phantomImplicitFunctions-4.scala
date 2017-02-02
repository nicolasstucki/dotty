import dotty.phantom.PhantomAny

class Slimer extends PhantomAny

import collection.mutable.ListBuffer

class Transaction {
  private val log = new ListBuffer[String]
  def println(s: String): Unit = log += s

  private var aborted = false
  private var committed = false

  def abort(): Unit = { aborted = true }
  def isAborted = aborted

  def commit(): Unit =
    if (!aborted && !committed) {
      Console.println("******* log ********")
      log.foreach(Console.println)
      committed = true
    }
}

class CanDoTransaction extends PhantomAny
object CanDoTransaction extends PhantomAny {
  def canDoTrans: CanDoTransaction = new CanDoTransaction
}

object Test {

  def transaction[T](op: (Transaction, CanDoTransaction) => T) = {
    val trans: Transaction = new Transaction
    op.apply(trans, CanDoTransaction.canDoTrans)
    trans.commit()
  }

  def thisTransaction = $t: Transaction => $t

  def f1(x: Int) = { ($t: Transaction, $c: CanDoTransaction) =>
    thisTransaction.apply($t).println(s"first step: $x")
    f2(x + 1).apply($t, $c)
  }
  def f2(x: Int) = { ($t: Transaction, $c: CanDoTransaction) =>
    thisTransaction.apply($t).println(s"second step: $x")
    f3(x * x).apply($t, $c)
  }
  def f3(x: Int) = { ($t: Transaction, $c: CanDoTransaction) =>
    thisTransaction.apply($t).println(s"third step: $x")
    if (x % 2 != 0) thisTransaction.apply($t).abort()
    x
  }

  def main(args: Array[String]) = {
    transaction { ($t, $c) =>
      val res = f1(args.length).apply($t, $c)
      println(if (thisTransaction.apply($t).isAborted) "aborted" else s"result: $res")
    }
  }
}
