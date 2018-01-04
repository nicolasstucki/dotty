package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.actions.{CodeDefinition, CodeHover, CodeMark, CodeWithAction, Position}

object Code {

  implicit class CodeHelper(val sc: StringContext) extends AnyVal {
    def code(args: Any*): CodeWithActions = {
      val pi = sc.parts.iterator
      val ai = args.iterator

      var line = 0
      var char = 0
      def scan(str: String): Unit = {
        for (c <- str)
          if (c == '\n') { line += 1; char = 0 } else { char += 1 }
      }

      val stringBuilder = new StringBuilder
      val listBuilder = List.newBuilder[ActionOnRange]
      val marks = List.newBuilder[(CodeMark, Position)]

      while (ai.hasNext) {
        val next = pi.next().stripMargin
        stringBuilder.append(next)
        scan(next)

        val startLine = line
        val startChar = char
        val startPos = Position(startLine, startChar)

        ai.next() match {
          case mark: CodeMark =>
            marks += (mark -> startPos)
          case code: CodeWithAction =>
            stringBuilder.append(code.text)
            scan(code.text)
            listBuilder += ActionOnRange(code, actions.Range(startPos, Position(line, char)))
          case arg => throw new Exception("Interpolated code should be a CodeWithAction but was " + arg)
        }

      }

      if (pi.hasNext)
        stringBuilder.append(pi.next())

      CodeWithActions(stringBuilder.result(), listBuilder.result(), marks.result())
    }
  }

  implicit class CodeActions(val str: String) extends AnyVal {
    def hover(expected: String): CodeHover = CodeHover(str, expected)
    def definition(fileName: String, start: CodeMark, end: CodeMark): CodeDefinition = CodeDefinition(str, fileName, start, end)
  }

  case class CodeWithActions(text: String, actions: List[ActionOnRange], marks: List[(CodeMark, Position)])

  case class ActionOnRange(code: CodeWithAction, range: actions.Range)

}
