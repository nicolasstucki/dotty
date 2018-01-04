package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util._

/** noop action on empty code that is used to mark positions in the code */
class CodeMark extends CodeWithAction {
  def text: String = ""
  def expected(marks: Map[CodeMark, (String, Position)]): String = throw new UnsupportedOperationException
}
