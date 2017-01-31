package dotty.tools.dotc.transform.phantom


import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.annotation.tailrec

class PhantomParamErasure extends MiniPhaseTransform with InfoTransformer {
  import tpd._
  import Phantoms._

  override def phaseName: String = "phantomParamErasure"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] =
    Set(classOf[PhantomParamLift])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    def assertNotPhantom(tree: Tree): Unit =
      assert(!tree.tpe.isPhantom, "All phantom type values should be erased in " + tree)
    tree match {
      case _: TypeTree =>
      case _: TypeDef =>
      case Block(stats, _) => stats.foreach(checkPostCondition)

      case DefDef(_, _, vparamss, tpt, _) if !returnsPhantom(tpt.tpe) =>
        vparamss.foreach(_.foreach(assertNotPhantom))

      case _ =>
    }
  }

  /* Tree transform */

  override def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] = {
    trees.collect {
      case stat: TypeDef     => stat
      case stat: ValOrDefDef => stat
      case Block(stats, expr) if expr.tpe.isPhantom => Block(stats, ref(defn.BoxedUnit_UNIT))
      case stat if !stat.tpe.isPhantom => stat
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (returnsPhantom(tree.tpe) || !tree.args.exists(_.tpe.isPhantom)) tree
    else cpy.Apply(tree)(tree.fun, tree.args.filter(!_.tpe.isPhantom))
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (ddef.tpt.tpe.isPhantom) {
     ddef
    } else {
      val newVparamss = ddef.vparamss.map(_.filter(vparam => !vparam.tpt.typeOpt.isPhantom))
      if (newVparamss == ddef.vparamss) ddef
      else cpy.DefDef(ddef)(vparamss = newVparamss)
    }
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = erasedPhantomParameters(tp)

}
