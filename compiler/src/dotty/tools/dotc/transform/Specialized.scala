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

class Specialized extends MiniPhaseTransform { thisTransformer =>
  import tpd._

  override def phaseName = "specialized"

  val specializations: mutable.Map[(Symbol, List[Type]), Symbol] = mutable.Map.empty

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    lazy val targs = tree.args.map(_.tpe.widenDealias)
    lazy val specBounds = specializedBounds(sym, targs)
    if (!isSpecilizable(sym)) tree
    else if (specBounds == sym.info.asInstanceOf[PolyType].paramInfos) tree
    else ref(specializedMethod(sym, specBounds)).appliedToTypes(targs)
  }

  private def isSpecilizable(sym: Symbol)(implicit ctx: Context): Boolean = {
    def rec(tpe: Type): Boolean = tpe match {
      case tpe: MethodType if tpe.paramInfos.exists(_.classSymbol eq defn.SpecializedClass) => true
      case tpe: MethodicType => rec(tpe.resultType)
      case _ => false
    }
    rec(sym.info)
  }

  private def specializedMethod(sym: Symbol, specBounds: List[TypeBounds])(implicit ctx: Context): Symbol = {
    assert(sym.info.isInstanceOf[PolyType])
    def newSpecializedMethod = {
      val info = sym.info.asInstanceOf[PolyType]
      val specName = sym.name ++ "$spec"
      val specFlags = sym.flags | Synthetic
      val specInfo = info.derivedLambdaType(paramInfos = specBounds)
      ctx.newSymbol(sym.owner, specName, specFlags, specInfo, sym.privateWithin, sym.coord)
    }
    specializations.getOrElseUpdate((sym, specBounds), newSpecializedMethod)
  }

  private def specializedBounds(sym: Symbol, targs: List[Type])(implicit ctx: Context): List[TypeBounds] = {
    val typeBounds = sym.info.asInstanceOf[PolyType].paramInfos
    val specializableIdxs = specilizableTypeParams(sym)

    targs.zipWithIndex.map { case (tpe, idx) =>
      if (specializableIdxs.contains(idx) && isSpecilizable(tpe)) TypeAlias(tpe.classSymbol.typeRef)
      else typeBounds(idx)
    }
  }

  private def specilizableTypeParams(sym: Symbol)(implicit ctx: Context): List[Int] = {
    sym.info.paramInfoss.flatten.collect {
      case tpe: RefinedType if tpe.classSymbol eq defn.SpecializedClass =>
        tpe.refinedInfo match {
          case tp1: TypeAlias =>
            tp1.underlying match {
              case tp2: TypeParamRef => tp2.paramNum
              case _ => assert(false); 0
            }
          case _ => assert(false); 0
        }
    }
  }

  private def isSpecilizable(tpe: Type)(implicit ctx: Context): Boolean = {
    tpe =:= defn.IntType ||
    tpe =:= defn.LongType ||
    tpe =:= defn.ShortType ||
    tpe =:= defn.CharType ||
    tpe =:= defn.ByteType ||
    tpe =:= defn.DoubleType ||
    tpe =:= defn.FloatType ||
    tpe =:= defn.UnitType
  }

}
