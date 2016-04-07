package dotty.tools
package dotc
package core
package tasty

import Contexts._, SymDenotations._, Symbols._
import dotty.tools.dotc.ast.tpd
import TastyUnpickler._, TastyBuffer._
import dotty.tools.dotc.core.tasty.DottyUnpickler.{SourceFileUnpickler, TreeSectionUnpickler, PositionsSectionUnpickler}
import util.Positions._
import util.{SourceFile, NoSource}
import PositionUnpickler._
import Annotations.Annotation
import classfile.ClassfileParser

object DottyUnpickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)

  class SourceFileUnpickler extends SectionUnpickler[SourceFile]("Sourcefile") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
      val TastyName.Simple(sourceName) = tastyName(reader.readNameRef())
      new SourceFile(sourceName.toString, Seq())
    }
  }

  class TreeSectionUnpickler extends SectionUnpickler[TreeUnpickler]("ASTs") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table) =
      new TreeUnpickler(reader, tastyName)
  }

  class PositionsSectionUnpickler extends SectionUnpickler[(Position, AddrToPosition)]("Positions") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table) =
      new PositionUnpickler(reader).unpickle()
  }
}

/** A class for unpickling Tasty trees and symbols.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 */
class DottyUnpickler(bytes: Array[Byte]) extends ClassfileParser.Embedded {
  import tpd._

  val unpickler = new TastyUnpickler(bytes)
  private val treeUnpickler = unpickler.unpickle(new TreeSectionUnpickler).get
  private val source = unpickler.unpickle(new SourceFileUnpickler).getOrElse(NoSource)

  /** Enter all toplevel classes and objects into their scopes
   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
   */
  def enter(roots: Set[SymDenotation])(implicit ctx: Context): Unit = {
    treeUnpickler.enterTopLevel(roots)
    if (source.exists)
      for (root <- roots) root.addAnnotation(Annotation.makeSourceFile(source.path))
  }

  /** The unpickled trees, and the source file they come from
   *  @param readPositions if true, trees get decorated with position information.
   */
  def body(readPositions: Boolean = false)(implicit ctx: Context): (List[Tree], SourceFile) = {
    if (readPositions)
      for ((totalRange, positions) <- unpickler.unpickle(new PositionsSectionUnpickler))
        treeUnpickler.usePositions(totalRange, positions)
    (treeUnpickler.unpickle(), source)
  }
}
