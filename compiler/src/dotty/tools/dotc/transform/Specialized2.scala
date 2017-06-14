package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import Types._
import Contexts.Context
import Symbols._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags._

class Specialized2 extends MiniPhaseTransform with InfoTransformer { thisTransformer =>
  import tpd._

  override def phaseName = "specialized2"

  private var specialized1: Specialized1 = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransforms.TreeTransform = {
    specialized1 = ctx.phaseOfClass(classOf[Specialized1]).asInstanceOf[Specialized1]
    super.prepareForUnit(tree)
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    // TODO: Add specialization in the class transform
    val specTrees = specialized1.specDefDefs.getOrElse(tree.symbol, Nil)
    if (specTrees.isEmpty) tree
    else Thicket(tree :: specTrees.map(x => transform(x)))
  }

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    tp match {
      case tp: ClassInfo =>
        val newSyms = specialized1.specDefDefsInClass.getOrElse(tp.cls, Nil)
        if (newSyms.isEmpty) tp
        else {
          val newDecls = tp.decls.cloneScope.openForMutations
          newSyms.foreach(sym => newDecls.enter(sym))
          ClassInfo(tp.prefix, tp.cls, tp.classParents, newDecls, tp.selfInfo)
        }
      case _ => tp
    }
  }
}
