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
    if (tree.symbol.isSpecilizable) {
      val sym = tree.symbol
      specializedDefDefs.put(sym, tree)
      sym.allOverriddenSymbols.foreach(s => specializedOverwrites.put(s, sym :: specializedOverwrites.getOrElse(s, Nil)))
    }
    tree
  }

}
