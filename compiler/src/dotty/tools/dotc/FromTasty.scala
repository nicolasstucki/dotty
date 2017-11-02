/* dotc
 * Copyright 2005-2015 LAMP/EPFL
 * @author  Martin Odersky
 */
package dotty.tools
package dotc

import core._
import Contexts._
import Symbols._
import SymDenotations._
import typer.FrontEnd
import Phases.Phase
import util._
import reporting.Reporter
import Decorators._
import dotty.tools.dotc.transform.Pickler
import tasty.DottyUnpickler
import ast.tpd._
import NameKinds.QualifiedName

/** Compiler for TASTY files.
 *  Usage:
 *
 *    scala dotty.tools.dotc.FromTasty (option | classname)*
 *
 *  Options are as for dotc.
 *  Classnames are fully qualified names of top-level classes that need to have a TASTY attribute.
 *  Example:
 *
 *    scala dotty.tools.dotc.FromTasty -Xprint:front extMethods.T
 */
object FromTasty extends Driver {
  override def newCompiler(implicit ctx: Context): Compiler = new TASTYCompiler

  class TASTYCompiler extends Compiler {

    override def phases: List[List[Phase]] = {
      val backendPhases = super.phases.dropWhile {
        case List(_: Pickler) => false
        case _ => true
      }.tail
      List(new ReadTastyTreesFromClasses) :: backendPhases
    }

    override def newRun(implicit ctx: Context): Run = {
      reset()
      new TASTYRun(this, ctx)
    }
  }

  class TASTYRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
    override def compile(classNames: List[String]) = {
      units = classNames.map(new TASTYCompilationUnit(_))
      compileUnits()
    }
  }

  class TASTYCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
    override def toString = s"class file $className"
  }

  class ReadTastyTreesFromClasses extends FrontEnd {

    override def isTyper = false

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] =
      units.map(readTASTY)

    def readTASTY(unit: CompilationUnit)(implicit ctx: Context): CompilationUnit = unit match {
      case unit: TASTYCompilationUnit =>
        val className = unit.className.toTypeName
        val clsd = ctx.base.staticRef(className)
        def cannotUnpickle(reason: String) = {
          ctx.error(s"class $className cannot be unpickled because $reason")
          unit
        }
        clsd match {
          case clsd: ClassDenotation =>
            clsd.infoOrCompleter match {
              case info: ClassfileLoader =>
                info.load(clsd)
                val unpickled = clsd.symbol.asClass.tree
                if (unpickled != null) CompilationUnit.mkCompilationUnit(clsd, unpickled, forceTrees = true)
                else cannotUnpickle(s"its class file ${info.classfile} does not have a TASTY attribute")
              case info =>
                cannotUnpickle(s"its info of type ${info.getClass} is not a ClassfileLoader")
            }
          case _ =>
            ctx.error(s"class not found: $className")
            unit
        }
    }
  }
}
