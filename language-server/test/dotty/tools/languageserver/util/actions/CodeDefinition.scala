package dotty.tools.languageserver.util.actions

case class CodeDefinition(text: String, fileName: String, start: CodeMark, end: CodeMark) extends CodeWithAction {
  override def expected(marks: Map[CodeMark, (String, Position)]): String = {
    assert(marks.contains(start), "CodeMark was not found in the code: " + start)
    assert(marks.contains(end), "CodeMark was not found in the code: " + end)
    val (startFileName, startPos) = marks(start)
    val (endFileName, endPos) = marks(end)
    assert(startFileName == endFileName, "Start and end markers must be in the same file")
    s"""List(Location [
       |  uri = "file:///Users/nicolasstucki/GitHub/dotty/language-server/../out/ide-tests/src/${startFileName}"
       |  range = Range [
       |    start = Position [
       |      line = ${startPos.line}
       |      character = ${startPos.char}
       |    ]
       |    end = Position [
       |      line = ${endPos.line}
       |      character = ${endPos.char}
       |    ]
       |  ]
       |])""".stripMargin
  }
}
