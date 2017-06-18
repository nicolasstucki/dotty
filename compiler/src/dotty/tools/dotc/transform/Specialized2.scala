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
import dotty.tools.dotc.linker.OuterTargs

class Specialized2 extends MiniPhaseTransform { thisTransformer =>
  import tpd._

  override def phaseName = "specialized2"

  private var specialized1: Specialized1 = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransforms.TreeTransform = {
    specialized1 = ctx.phaseOfClass(classOf[Specialized1]).asInstanceOf[Specialized1]
    super.prepareForUnit(tree)
  }


  override def transformThis(tree: tpd.This)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    // TODO make improve way to get outerTargs
    specialized1.specSymbols.find(_._2 == ctx.owner) match {
      case Some(((_, outerTargs), _)) =>
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
        val sym = x.symbol
        if (sym.is(Override) && sym.allOverriddenSymbols.isEmpty) sym.resetFlag(Override)
        if (sym.owner.isClass) sym.entered
      }
      Thicket(tree :: specTrees.map(x => transform(x)))
    }
  }

  private final class SubstituteOuterTargs(outerTargs: OuterTargs)(implicit ctx: Context) extends DeepTypeMap {
    override def apply(tp: Type): Type = tp match {
      case RefinedType(parent, name, info) =>
        outerTargs.mp.get(parent.classSymbol) match {
          case Some(targMap) => RefinedType(parent, name, targMap.getOrElse(name, info))
          case _ => mapOver(tp)
        }
      case _ => mapOver(tp)
    }
  }

}
