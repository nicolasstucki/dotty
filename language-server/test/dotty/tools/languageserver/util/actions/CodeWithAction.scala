package dotty.tools.languageserver.util.actions

trait CodeWithAction {
  def text: String
  def expected(marks: Map[CodeMark, (String, Position)]): String
}
