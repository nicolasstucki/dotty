package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo, TreeTransform}

import scala.annotation.tailrec

class PhantomDeclErasure extends MiniPhaseTransform with InfoTransformer {
  import tpd._
  import Phantoms._

  override def phaseName: String = "phantomDeclErasure"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] =
    Set(classOf[PhantomRefErasure])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    def assertNotPhantom(tpe: Type): Unit =
      assert(!isPhantom(tpe), "All phantom type declarations should be erased in " + tree)

    tree match {
      case _: TypeTree       =>
      case _: TypeDef        =>
      case tree: ValOrDefDef =>
        assertNotPhantom(tree.tpt.typeOpt)
        tree.symbol match {
          case sym: ClassSymbol =>
            assertNotPhantom(sym.info)
            assert(!sym.classInfo.decls.exists(sym2 => isPhantomMethodType(sym2.info)),
                "All phantom type declarations should be erased in " + sym.classInfo)
          case _ =>
        }
      case _ => assertNotPhantom(tree.tpe)
    }
  }

  /* Tree transform */

  override def transformStats(trees: List[tpd.Tree])(implicit ctx: Context, info: TransformerInfo): List[tpd.Tree] = {
    trees.flatMap {
      case ValDef(_, tpt, block: Block) if isPhantom(tpt.tpe) => block.stats
      case stat: ValOrDefDef if isPhantom(stat.tpt.tpe) => Nil
      case stat: TypeDef if isPhantom(stat.tpe) => Nil
      case stat => List(stat)
    }
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp: ClassInfo if !tp.typeSymbol.is(Flags.Package) && !isPhantom(tp.typeRef) =>
      val newDecls = tp.decls.filteredScope(sym => !isPhantomMethodType(sym.info) && !isPhantomClassType(sym.info))
      ClassInfo(tp.prefix, tp.cls, tp.classParents, newDecls, tp.selfInfo)

    case _ => tp
  }

  @tailrec private def isPhantomMethodType(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
    case tpe: MethodicType => isPhantom(tpe.resultType) || isPhantomMethodType(tpe.resultType)
    case _                 => false
  }

  private def isPhantomClassType(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case tp: ClassInfo => tp.cls.derivesFrom(defn.PhantomAnyClass)
    case _             => false
  }

}
