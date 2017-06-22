package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import Types._
import Contexts.Context
import Symbols._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.linker._

class Specialized2 extends MiniPhaseTransform { thisTransformer =>
  import tpd._

  override def phaseName = "specialized2"

  private var specialized1: Specialized1 = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransforms.TreeTransform = {
    specialized1 = ctx.phaseOfClass(classOf[Specialized1]).asInstanceOf[Specialized1]
    super.prepareForUnit(tree)
  }

  override def transformThis(tree: tpd.This)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    specialized1.outerTargsOf.get(ctx.owner) match {
      case Some(outerTargs) =>
        val substMap = new SubstituteOuterTargs(outerTargs)
        val newTpe = substMap(tree.tpe.widenDealias)
        if (tree.tpe <:< newTpe) tree
        else tree.asInstance(newTpe)
      case None => tree
    }
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val specSym = specialized1.getSpecializedSym(tree)
    if (specSym.exists) {
      val specFun = tree.fun match {
        case Select(qual, _) => qual.select(specSym)
        case ident =>
          ref(specSym) match {
            case _: Select => cpy.Ident(ident)(specSym.name)
            case ref => ref
          }
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
        val sym = x.symbol
        if (sym.is(Override) && sym.allOverriddenSymbols.isEmpty) sym.resetFlag(Override)
        if (sym.owner.isClass) sym.entered
      }
      Thicket(tree :: specTrees.map(x => transform(x)))
    }
  }

}
