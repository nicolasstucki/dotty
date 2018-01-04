package dotty.tools.languageserver.util

import java.nio.file.Paths

import dotty.tools.dotc.util.DiffUtil
import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.actions.{CodeDefinition, CodeHover, CodeMark}
import dotty.tools.languageserver.util.server.TestServer

class BaseTest {

  /** Check all actions performed in the code */
  def checkActions(code: CodeWithActions): Unit = checkActions("Foo.scala" -> code)

  /** Check all actions performed in the code */
  def checkActions(codeInFiles: (String, CodeWithActions)*): Unit = {

    val marks = {
      val markSeq = {
        for {
          (fileName, code) <- codeInFiles
          (mark, pos) <- code.marks
        } yield mark -> (fileName, pos)
      }
      val markMap = markSeq.toMap
      assert(markSeq.size == markMap.size, "Each CodeMark instance can only appear one in the code")
      markMap
    }

    val testServer = new TestServer(Paths.get("../out/ide-tests"))

    val allFilesOpened = codeInFiles.map { case (fileName, code) =>
      (testServer.openCode(code.text, fileName), code.actions)
    }

    for {
      (file, actions) <- allFilesOpened
      action <- actions
      if !action.code.isInstanceOf[CodeMark]
      (line, character) <- action.range.allPositions
    } {

      val response = action.code match {
        case _: CodeHover => testServer.hover(file, line, character).toString
        case _: CodeDefinition => testServer.definition(file, line, character).toString
      }

      val expected = action.code.expected(marks)

      if (expected != response) {

        val diff = DiffUtil.mkColoredLineDiff(expected.split("\n"), response.split("\n"))

        val message = // TODO highlight position in code
          s"""When hovering line $line on character $character
             |${codeInFiles.map { case (file, code) => s"// $file\n${code.text}"}.mkString("\n")}
             |
             |expected output (left) did not match response (right)
             |$diff
           """.stripMargin
        assert(false, message)
      }
    }
  }

}
