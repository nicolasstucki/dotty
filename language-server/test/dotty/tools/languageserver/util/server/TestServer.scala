package dotty.tools.languageserver.util.server

import java.io.PrintWriter
import java.net.URI
import java.nio.file.Path

import dotty.tools.languageserver.DottyLanguageServer
import org.eclipse.lsp4j._

import scala.collection.JavaConverters._

class TestServer(testFolder: Path) {

  private val dottyIdeJson: String =
    """|[ {
       |  "id" : "dotty-ide-test",
       |  "compilerVersion" : "0.6.0-bin-SNAPSHOT-nonbootstrapped",
       |  "compilerArguments" : [ "-feature", "-deprecation", "-unchecked", "-Xfatal-warnings", "-encoding", "UTF8", "-language:existentials,higherKinds,implicitConversions", "-bootclasspath", "/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/lib/resources.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/lib/rt.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/lib/sunrsasign.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/lib/jsse.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/lib/jce.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/lib/charsets.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/lib/jfr.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/classes" ],
       |  "sourceDirectories" : [ "/Users/nicolasstucki/GitHub/dotty/out/ide-tests/src" ],
       |  "dependencyClasspath" : [ "/Users/nicolasstucki/GitHub/dotty/compiler/../out/bootstrap/dotty-compiler-bootstrapped/scala-0.6/classes", "/Users/nicolasstucki/GitHub/dotty/interfaces/target/classes", "/Users/nicolasstucki/GitHub/dotty/library/../out/bootstrap/dotty-library-bootstrapped/scala-0.6/classes", "/Users/nicolasstucki/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.4.jar", "/Users/nicolasstucki/.ivy2/cache/org.scala-lang.modules/scala-asm/bundles/scala-asm-6.0.0-scala-1.jar", "/Users/nicolasstucki/.ivy2/cache/com.typesafe.sbt/sbt-interface/jars/sbt-interface-0.13.15.jar", "/Users/nicolasstucki/.ivy2/cache/org.scala-lang.modules/scala-xml_2.12/bundles/scala-xml_2.12-1.0.6.jar" ],
       |  "classDirectory" : "/Users/nicolasstucki/GitHub/dotty/out/ide-tests/out"
       |}
       |]
    """.stripMargin
  new PrintWriter(testFolder.resolve(".dotty-ide.json").toString) { write(dottyIdeJson); close() }

  private val server = new DottyLanguageServer
  private val client = new TestClient
  server.connect(client)

  private val initParams = new InitializeParams()
  initParams.setRootUri(testFolder.toAbsolutePath.toUri.toString)
  server.initialize(initParams).get()

  /** Open the code in the given file and returns the file.
   *  @param code code in file
   *  @param fileName file path in the source directory
   *  @return the file opened
   */
  def openCode(code: String, fileName: String): TestFile = {
    val testFile = new TestFile(sourceURI(fileName).toString)
    val didOpenTextDocumentParams = new DidOpenTextDocumentParams()
    val tdi = new TextDocumentItem()
    tdi.setUri(testFile.uri)
    tdi.setText(code)
    didOpenTextDocumentParams.setTextDocument(tdi)
    server.didOpen(didOpenTextDocumentParams)
    testFile
  }

  def definition(file: TestFile, line: Int, character: Int): List[Location] =
    server.definition(pos(file, line, character)).get().asScala.toList

  def hover(file: TestFile, line: Int, character: Int): Hover =
    server.hover(pos(file, line, character)).get()

  private def pos(file: TestFile, line: Int, character: Int) = {
    val tdpp = new TextDocumentPositionParams
    tdpp.setTextDocument(file.textDocumentIdentifier)
    tdpp.setPosition(new Position(line, character))
    tdpp
  }

  private def sourceURI(fileName: String): URI =
    testFolder.resolve("src").resolve(fileName).toAbsolutePath.toUri
}
