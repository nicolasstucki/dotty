package dotty.tools.dotc
package transform
package specialize

import core._
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.linker._

import scala.collection.mutable

class SymbolSpecialization {

  def specializedDefSymbol(sym: Symbol, outerTargs: OuterTargs)(implicit ctx: Context): Symbol = {
    val symInfo = sym.info.asInstanceOf[PolyType]
    val specName = sym.name ++ specializedNameSuffix(sym, outerTargs)
    val specFlags = sym.flags | Synthetic
    val specParamInfos: List[TypeBounds] = outerTargs.mp.get(sym) match {
      case Some(thisSpecial) =>
        symInfo.paramInfos.zipWithConserve (symInfo.paramNames) {
          case (info, name) => thisSpecial.get (name).map (tp => TypeAlias (tp) ).getOrElse(info)
        }
      case _ => symInfo.paramInfos
    }
    val subs = new SubstituteOuterTargs(outerTargs)
    val specResType = subs(symInfo.resType)
    val specInfo = symInfo.newLikeThis(symInfo.paramNames, specParamInfos, specResType)
    ctx.newSymbol(sym.owner, specName, specFlags, specInfo, sym.privateWithin, sym.coord)
  }

  def specializeTraitSymbol(sym: ClassSymbol, outerTargs: OuterTargs)(implicit ctx: Context): ClassSymbol = {
    val specName = sym.name ++ specializedNameSuffix(sym, outerTargs)
    val specFlags = sym.flags | Synthetic
    def specClsInfo(cls: ClassSymbol): Type = sym.info
    ctx.newClassSymbol(sym.owner, specName, specFlags, specClsInfo, sym.privateWithin, sym.coord, sym.assocFile)
  }



  private val boundNames = mutable.Map.empty[(Name, OuterTargs, Map[Name, Type]), Name]
  private val nameIdx = mutable.Map.empty[Name, Int]
  private def specializedNameSuffix(sym: Symbol, outerTargs: OuterTargs)(implicit ctx: Context): Name = {
    val targs = outerTargs.mp(sym)
    val outerTargs1 = outerTargs.without(sym)
    def makeNewName = {
      val idx = nameIdx.getOrElse(sym.name, 0) + 1
      nameIdx.put(sym.name, idx)
      ("$spec$" + idx).toTermName
    }
    boundNames.getOrElseUpdate((sym.name, outerTargs1, targs), makeNewName)
  }

}
