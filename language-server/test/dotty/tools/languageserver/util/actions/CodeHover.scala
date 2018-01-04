package dotty.tools.languageserver.util.actions

case class CodeHover(text: String, expected: String) extends CodeWithAction {
  def expected(marks: Map[CodeMark, (String, Position)]): String = {
    if (expected.isEmpty)
      s"""Hover [
         |  contents = null
         |  range = null
         |]""".stripMargin
    else
      s"""Hover [
         |  contents = SeqWrapper (
         |    Either [
         |      left = $expected
         |      right = null
         |    ]
         |  )
         |  range = null
         |]""".stripMargin
  }
}
