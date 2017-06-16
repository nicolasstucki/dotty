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

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val specSym = specialized1.getSpecializedSym(tree)
    if (specSym.exists) {
      val specFun = tree.fun match {
        case Select(qual, _) => qual.select(specSym)
        case _ => ref(specSym)
      }
      specFun.appliedToTypes(tree.args.map(_.tpe))
    } else {
      tree
    }
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val specTrees = specialized1.specDefDefs.getOrElse(tree.symbol, Nil)
    if (specTrees.isEmpty) tree
    else {
      for (x <- specTrees) { // clear Override flag if overrides where not specialized
        if (x.symbol.is(Override) && x.symbol.allOverriddenSymbols.isEmpty)
          x.symbol.resetFlag(Override)
      }
      Thicket(tree :: specTrees.map(x => transform(x)))
    }
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
