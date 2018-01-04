package dotty.tools.languageserver

import org.junit.Test
import dotty.tools.languageserver.util._
import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.actions.CodeMark

class DefinitionTest extends BaseTest {

  @Test def classDefinition0: Unit = {
    val mark1 = new CodeMark
    val mark2 = new CodeMark
    val refToFoo = "Foo".definition("Foo.scala", mark1, mark2)
    checkActions(
      "Foo.scala" -> code"class ${mark1}Foo$mark2 { new $refToFoo }",
      "Bar.scala" -> code"class Bar { val foo: $refToFoo = new $refToFoo }"
    )
  }

  @Test def valDefinition0: Unit = {
    val mark1 = new CodeMark
    val mark2 = new CodeMark
    val refToX = "x".definition("Foo.scala", mark1, mark2)
    checkActions(
      "Foo.scala" -> code"class Foo { val ${mark1}x$mark2 = 0; $refToX }",
      "Bar.scala" -> code"class Bar { val foo = new Foo; foo.$refToX }"
    )
  }

}
