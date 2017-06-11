package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.Flags._

import scala.collection.mutable

class Specialized1 extends MiniPhaseTransform { thisTransformer =>
  import tpd._

  override def phaseName = "specialized1"

  private val specSymbols: mutable.Map[(Symbol, List[Type]), Symbol] = mutable.Map.empty
  val specDefDefs: mutable.Map[Symbol, List[DefDef]] = mutable.Map.empty

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    specializedTypeApply(tree)

  private def specializedTypeApply(tree: TypeApply)(implicit ctx: Context) = {
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
    val key = (sym, specBounds)
    def newSpecializedMethod = {
      val symInfo = sym.info.asInstanceOf[PolyType]
      val specName = sym.name ++ "$spec"
      val specFlags = sym.flags | Synthetic
      val specInfo = symInfo.derivedLambdaType(paramInfos = specBounds)
      val specSym = ctx.newSymbol(sym.owner, specName, specFlags, specInfo, sym.privateWithin, sym.coord)
      specSymbols.put(key, specSym)
      val specDefDef = createSpecializedDefDef(getDefDefOf(sym), specSym)
      specDefDefs.put(sym, specDefDef :: specDefDefs.getOrElse(sym, Nil))
      specSym
    }
    specSymbols.get(key) match {
      case Some(specSym) => specSym
      case None => newSpecializedMethod
    }
  }

  private def getDefDefOf(sym: Symbol)(implicit ctx: Context): DefDef =
    ctx.phaseOfClass(classOf[Specialized0]).asInstanceOf[Specialized0].specializableDefDefs(sym)

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

  private def createSpecializedDefDef(ddef: DefDef, specSym: Symbol)(implicit ctx: Context) = {
    val oldSym = ddef.symbol
    def rhsFn(tparams: List[Type])(vparamss: List[List[Tree]]) = {
      val transformTparams: Map[Symbol, Type] =
        ddef.tparams.map(_.symbol).zip(tparams).toMap

      def vparamssSyms(vparamss: List[List[Tree]]): List[Symbol] = vparamss.flatten.map(_.symbol)
      val transformVparams: Map[Symbol, Symbol] =
        (vparamssSyms(ddef.vparamss) zip vparamssSyms(vparamss)).toMap

      def treeMap(tree: Tree): Tree = tree match {
        case t: Ident if t.symbol.is(Param) && t.symbol.owner == oldSym => ref(transformVparams(t.symbol))
        case Return(t, from) if from.symbol == oldSym => Return(t, ref(specSym))
        case t => t
      }

      val typeMap = new TypeMap() {
        override def apply(tp: Type): Type = tp match {
          case tp: TypeRef if tp.typeSymbol.owner == oldSym && tp.typeSymbol.is(Param) =>
            transformTparams.getOrElse(tp.typeSymbol, tp)
          case _ => mapOver(tp)
        }
      }

      val treeTypeMap = new TreeTypeMap(typeMap, treeMap, oldSym :: Nil, specSym :: Nil)

      treeTypeMap.transform(ddef.rhs)
    }

    transformInnerCalls(polyDefDef(specSym.asTerm, rhsFn))
  }

  private def transformInnerCalls(specDefDef: DefDef)(implicit ctx: Context): DefDef = {
    def treeMap(tree: Tree): Tree = tree match {
      case tree: TypeApply => specializedTypeApply(tree)
      case _ => tree
    }
    val transformInnerCalls = new TreeTypeMap(treeMap = treeMap)
    transformInnerCalls.transform(specDefDef).asInstanceOf[DefDef]
  }
}
