package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.TreeTransforms.MiniPhaseTransform
import Contexts.Context
import Symbols._
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.core.Flags._

import scala.collection.mutable

class SpecializedOverwrites extends MiniPhaseTransform with SymTransformer { thisTransformer =>

  override def phaseName = "specializedOverwrites"

  val allKnownOverwrites: mutable.Map[Symbol, List[Symbol]] = mutable.Map.empty

  override def transformSym(sym: SymDenotations.SymDenotation)(implicit ctx: Context): SymDenotations.SymDenotation = {
    // TODO check that this works across compilation units
    if (sym.is(Method) && sym.isSpecializable)
      sym.allOverriddenSymbols.foreach(s => allKnownOverwrites.put(s, sym.symbol :: allKnownOverwrites.getOrElse(s, Nil)))
    sym
  }
}
