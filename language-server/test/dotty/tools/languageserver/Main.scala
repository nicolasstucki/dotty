package dotty.tools.languageserver

import org.junit.Test
import java.lang.reflect.InvocationTargetException

// TODO remove this and use JUnit to run the tests
object Main {
  def main(args: Array[String]): Unit = {
    var testsFailed = 0
    for (clazz <- testsClasses) {
      var passed = 0
      var failed = 0
      println(s"Starting tests in ${clazz.getSimpleName}")
      for (method <- clazz.getMethods.sortBy(_.getName)) {
        if (method.getAnnotation(classOf[Test]) ne null) {
          print(s"Testing $clazz.${method.getName} ")
          try {
            method.invoke(clazz.getConstructor().newInstance())
            println(Console.GREEN + "passed" + Console.RESET)
            passed += 1
          } catch {
            case ex: InvocationTargetException =>
              ex.getCause match {
                case ex1: AssertionError =>
                  println(Console.RED + "failed" + Console.RESET)
                  System.err.println(s"${method.getName} failed with")
                  System.err.println(ex1.getMessage)
                  failed += 1
                case _ => throw ex
              }
          }
        }
      }

      if (failed == 0) {
        println(s"${Console.GREEN}Passed all $passed tests${Console.RESET}")
      } else {
        testsFailed += 1
        System.err.println(s"Passed $passed, ${Console.RED}failed $failed${Console.RESET}, total ${passed + failed}")
      }
      println()
    }
    if (testsFailed != 0) {
      System.err.println(s"Failed $testsFailed tests")
      System.exit(1)
    }
  }

  private def testsClasses = List(
    classOf[DefinitionTest],
    classOf[HoverTest],
  )
}
