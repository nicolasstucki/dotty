package dotty.tools.languageserver


import dotty.tools.dotc.util.DiffUtil
import org.junit.Test
import java.nio.file.Paths

import dotty.tools.languageserver.util._
import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.server.TestServer


class HoverTest extends BaseTest {

  @Test def hoverOnWhiteSpace0: Unit = checkActions(code"${" " hover ""}")
  @Test def hoverOnWhiteSpace1: Unit = checkActions(code"${" " hover ""}class Foo")
  @Test def hoverOnWhiteSpace2: Unit = checkActions(code"class Foo ${" " hover ""}")
  @Test def hoverOnWhiteSpace3: Unit = checkActions(code"class Foo { } ${" " hover ""}")

  @Test def hoverOnClass0: Unit = checkActions(code"class ${"Foo" hover "Foo" } ")
  @Test def hoverOnClass1: Unit = checkActions(code"${"class Foo { } " hover "Foo"}")

  @Test def hoverOnValDef0: Unit = checkActions(code"class Foo { ${"val x = " hover "Int"}8 }")
  @Test def hoverOnValDef1: Unit = checkActions(code"class Foo { val x = ${"8" hover "Int(8)"} }")
  @Test def hoverOnValDef2: Unit = checkActions(code"class Foo { val x = 8; ${"x" hover "Int"} }")
  @Test def hoverOnValDef3: Unit = checkActions(code"class Foo { final val x = 8; ${"x" hover "Int(8)"} }")

  @Test def hoverOnDefDef0: Unit = checkActions(code"class Foo { ${"def x = " hover "Int"}8 }")
  @Test def hoverOnDefDef1: Unit = checkActions(code"class Foo { def x = ${"8" hover "Int(8)"} }")
  @Test def hoverOnDefDef2: Unit = checkActions(code"class Foo { def x = 8; ${"x" hover "Int"} }")
  @Test def hoverOnDefDef3: Unit = checkActions(code"class Foo { final def x = 8; ${"x" hover "Int"} }")

  @Test def hoverMissingRef0: Unit = checkActions(code"class Foo { ${"x" hover "<error not found: x>"} }")

  @Test def hoverFun0: Unit = checkActions(
    code"""class Foo {
          |  def x: String = ${"\"abc\"" hover "String(\"abc\")"}
          |  ${"x" hover "String"}
          |
          |  def y(): Int = 9
          |  y(${")" hover "Int"}
          |  ${"y(" hover "(): Int"})
          |}
        """
  )

}
