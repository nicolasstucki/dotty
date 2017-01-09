package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.linker.CollectSummaries.SubstituteByParentMap

object OuterTargs {
  def empty = new OuterTargs(Map.empty)
}

final case class OuterTargs (mp: Map[Symbol, Map[Name, Type]]) extends AnyVal {

  def nonEmpty: Boolean = mp.nonEmpty

  def add(parent: Symbol, tp: Type)(implicit ctx: Context): OuterTargs = {
    this.add(parent, tp.typeSymbol.name, tp)
  }

  def add(parent: Symbol, name: Name, tp: Type): OuterTargs = {
    val old = mp.getOrElse(parent, Map.empty)
    new OuterTargs(mp.updated(parent, old + (name -> tp)))
  }

  def ++(other: OuterTargs)(implicit ctx: Context): OuterTargs = {
    other.mp.foldLeft(this) { (x, y) =>
      y._2.foldLeft(x: OuterTargs)((x: OuterTargs, z: (Name, Type)) => x.add(y._1, z._1, z._2))
    }
  }

  def combine(environment: OuterTargs)(implicit ctx: Context): OuterTargs = {
    val subst = new SubstituteByParentMap(environment)
    val newMap = mp.map(x => (x._1, x._2.map(x => (x._1, subst.apply(x._2)))))
    OuterTargs(newMap)
  }
}
