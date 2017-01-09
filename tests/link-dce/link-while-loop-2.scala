import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 20, classesWithReachableMethods = 6, reachableMethods = 7)
  def main(args: Array[String]): Unit = {
    var i = 0
    do {
      System.out.println(i)
      i += 1
    } while (i < 5)
  }
}
