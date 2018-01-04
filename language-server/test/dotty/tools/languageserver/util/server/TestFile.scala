package dotty.tools.languageserver.util.server

import org.eclipse.lsp4j.TextDocumentIdentifier

class TestFile(val uri: String) extends AnyVal {
  def textDocumentIdentifier: TextDocumentIdentifier = new TextDocumentIdentifier(uri)
}
