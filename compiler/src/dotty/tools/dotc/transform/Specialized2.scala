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

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val specializations = ctx.phaseOfClass(classOf[Specialized1]).asInstanceOf[Specialized1].specDefDefs
    // TODO: Add specialization in the class transform
    val specTrees = specializations.getOrElse(tree.symbol, Nil)
    if (specTrees.isEmpty) tree
    else Thicket(tree :: specTrees.map(x => transform(x)))
  }

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    val specializations = ctx.phaseOfClass(classOf[Specialized1]).asInstanceOf[Specialized1].specDefDefs
    tp match {
      case tp: ClassInfo =>
        val newSyms = specializations.filter(_._1.owner == tp.cls).map(_._2).flatten.map(_.symbol)
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
