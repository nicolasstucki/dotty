package dotty.tools.languageserver.util.actions

/** Inclusive range */
case class Range(start: Position, end: Position) {
  assert(start.line <= end.line)
  assert(start.line != end.line || start.char <= end.char)

  def allPositions: Iterator[(Int, Int)] = {
    assert(start.line == end.line, "multiline ranges not supported") // TODO implement multiline
    (start.char until end.char).iterator.map(char => (start.line, char))
  }
}
