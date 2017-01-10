import java.util.Observable

import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 141, classesWithReachableMethods = 24, reachableMethods = 89)
  def main(args: Array[String]): Unit = {
    val classLoader = Test.getClass.getClassLoader()

    try {
      val mainClass = classLoader.loadClass("Test")
      val mainMethod = mainClass.getMethod("dceTest")
      mainMethod.invoke(null);
    } catch {
      case e: java.lang.Exception => e.getCause.printStackTrace()
    }
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def shouldDCE(expr: => Any): Unit = try {
    expr
    throw new Exception("Expected DCE")
  } catch {
    case dce: dotty.runtime.DeadCodeEliminated =>
    // TODO: check stack trace to see if the DCE was in the fist call of expr
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def dceTest: Unit = {
    System.out.println("dceTest")

    val b = new B {}
    Test.shouldDCE(b.f1)
    b.hashCode()
    b.equals(b)
    b.toString
    b.update(new Observable, null)
  }

  @scala.EntryPoint def entryPoint(): Unit = {
    System.out.print(new B {})
  }
}

abstract class B extends java.util.Observer {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
  override def update(o: Observable, arg: scala.Any): Unit = ()
}
