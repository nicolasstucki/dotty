package dotty.tools.dotc.linker


import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{Type, TypeAlias, TypeType}

object OuterTargs {
  val empty = new OuterTargs(Map.empty)
}

// TODO: make value class (disabled for debugging)
final case class OuterTargs(/*val*/ mp: Map[Symbol, Map[Name, Type]]) /* extends AnyVal */ {

  def nonEmpty: Boolean = mp.nonEmpty

  def add(parent: Symbol, tp: Type)(implicit ctx: Context): OuterTargs = {
    assert(!parent.isClass || tp.isInstanceOf[TypeType], tp)
    this.add(parent, tp.typeSymbol.name, tp)
  }

  def add(parent: Symbol, name: Name, tp: Type)(implicit ctx: Context): OuterTargs = {
    val tp1 = if (parent.isClass && !tp.isInstanceOf[TypeType]) TypeAlias(tp) else tp
    val old = mp.getOrElse(parent, Map.empty)
    new OuterTargs(mp.updated(parent, old + (name -> tp1)))
  }

  def addAll(parent: Symbol, namesAndTypes: List[(Name, Type)])(implicit ctx: Context): OuterTargs =
    namesAndTypes.foldLeft(this){ case (x, nameType) => x.add(parent, nameType._1, nameType._2) }

  def changeParent(oldSym: Symbol, newSym: Symbol): OuterTargs = {
    val oldMap = mp(oldSym)
    new OuterTargs((mp - oldSym).updated(newSym, oldMap))
  }

  def without(sym: Symbol): OuterTargs = new OuterTargs(mp - sym)

  override def toString: String = s"OuterTargs($mp)"
}