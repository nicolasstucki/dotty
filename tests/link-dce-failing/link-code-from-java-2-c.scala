import java.util.Observable

import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 106, classesWithReachableMethods = 19, reachableMethods = 70)
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
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def dceTest: Unit = {
    System.out.println("dceTest")

    val c = new C {}
    Test.shouldDCE(c.f1)
    c.hashCode()
    c.equals(c)
    c.toString
    c.update(new Observable, null)
  }

  @scala.EntryPoint def entryPoint(): Unit = {
    System.out.print(new C {})
  }
}

trait C extends java.util.Observer  {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
  override def update(o: Observable, arg: scala.Any): Unit = ()
}