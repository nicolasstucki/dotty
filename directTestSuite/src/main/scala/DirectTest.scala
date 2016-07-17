
import java.io.{File, FileWriter}

import dotty.tools.dotc.reporting._

object DirectTest {

  def toFile(code: String): File = {
    val file = java.io.File.createTempFile("tmp", ".scala")
    file.deleteOnExit()
    var fileWriter: FileWriter = null
    try {
      fileWriter = new FileWriter(file)
      fileWriter.append(code)
    } finally {
      if (fileWriter ne null)
        fileWriter.close()
    }
    file
  }

  def compile(opts0: List[String], sources: List[File]): Reporter = {
//    val clogFWriter = new FileWriter(new File("directCompileOutput.txt"), true)
//    val clogWriter = new PrintWriter(clogFWriter, true)
    println("\ncompiling " + sources.mkString(" ") + "\noptions: " + opts0.mkString(" "))
    println()
    val processor = dotty.tools.dotc.Main
    val clogger = new ConsoleReporter //(writer = clogWriter)
    val reporter = processor.process((sources.map(_.toString) ::: opts0).toArray, clogger)
    println(reporter.summary)
    println(reporter.warningCount)
    reporter
  }
}
