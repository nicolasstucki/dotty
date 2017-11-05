package dotty.tools
package dotc

import dotty.tools.dotc.core.Types.Type // FIXME Why is this import needed in posTwice testNonCyclic?
import util.SourceFile
import dotty.tools.dotc.ast.{ tpd, untpd }
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.Symbols._

class CompilationUnit(val source: SourceFile) {

  override def toString = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  def isJava = source.file.name.endsWith(".java")

  /** Pickled TASTY binaries, indexed by class. */
  var pickled: Map[ClassSymbol, Array[Byte]] = Map()
}

object CompilationUnit {

  /** Make a compilation unit for top class `clsd` with the contends of the `unpickled` */
  def mkCompilationUnit(clsd: ClassDenotation, unpickled: tpd.Tree, forceTrees: Boolean)(implicit ctx: Context): CompilationUnit = {
    val file = Option(clsd.symbol.sourceFile).getOrElse(clsd.symbol.associatedFile)
    val unit1 = new CompilationUnit(new SourceFile(file, Seq()))
    unit1.tpdTree = unpickled
    if (forceTrees)
      force.traverse(unit1.tpdTree)
    unit1
  }

  /** Force the tree to be loaded */
  private object force extends tpd.TreeTraverser {
    def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = traverseChildren(tree)
  }
}
