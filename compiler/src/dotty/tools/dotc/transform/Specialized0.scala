package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Flags._

import scala.collection.mutable

class Specialized0 extends MiniPhaseTransform { thisTransformer =>
  import tpd._

  override def phaseName = "specialized0"

  val specializedDefDefs: mutable.Map[Symbol, DefDef] = mutable.Map.empty
  val specializedOverwrites: mutable.Map[Symbol, List[Symbol]] = mutable.Map.empty

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if (isSpecilizable(tree.symbol)) {
      val sym = tree.symbol
      specializedDefDefs.put(sym, tree)
      sym.allOverriddenSymbols.foreach(s => specializedOverwrites.put(s, sym :: specializedOverwrites.getOrElse(s, Nil)))
    }
    tree
  }

  def isSpecilizable(sym: Symbol)(implicit ctx: Context): Boolean = {
    def rec(tpe: Type): Boolean = tpe match {
      case tpe: MethodType if tpe.paramInfos.exists(_.classSymbol eq defn.SpecializedClass) => true
      case tpe: MethodicType => rec(tpe.resultType)
      case _ => false
    }
    if (ctx.settings.specializeAll.value) {
      !sym.owner.is(Scala2x) && (sym.owner ne defn.AnyClass) && sym.info.widenDealias.isInstanceOf[PolyType]
    } else rec(sym.info)
  }

}
