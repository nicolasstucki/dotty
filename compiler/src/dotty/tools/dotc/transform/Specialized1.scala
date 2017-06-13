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
import dotty.tools.dotc.core.Names._

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
    else {
      val specSym = specializedMethod(sym, specBounds)
      val specFun = tree.fun match {
        case Select(qual, _) => qual.select(specSym)
        case _ => ref(specSym)
      }
      specFun.appliedToTypes(targs)
    }
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
      val specName = sym.name ++ specializedNameSuffix(specBounds)
      val specFlags = sym.flags | Synthetic
      val specInfo = symInfo.derivedLambdaType(paramInfos = specBounds)
      val specSym = ctx.newSymbol(sym.owner, specName, specFlags, specInfo, sym.privateWithin, sym.coord)
      specSymbols.put(key, specSym)
      val specDefDef = createSpecializedDefDef(getDefDefOf(sym), specSym)
      specDefDefs.put(sym, specDefDef :: specDefDefs.getOrElse(sym, Nil))
      // FIXME: specialize overriding symbols (collect them in Specialize0)
      specSym
    }
    specSymbols.get(key) match {
      case Some(specSym) => specSym
      case None => newSpecializedMethod
    }
  }

  private val boundNames = mutable.Map.empty[List[TypeBounds], Name]
  private var nameIdx = 0
  private def specializedNameSuffix(specBounds: List[TypeBounds])(implicit ctx: Context): Name = {
    // TODO Use unique names
    // TODO use specInfo and not specBounds? see foo14 in specialized-1.scala
    boundNames.getOrElseUpdate(specBounds, { nameIdx += 1; ("$spec$" + nameIdx).toTermName })
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
    tpe =:= defn.UnitType ||
    tpe.widenDealias.classSymbol.isValueClass
  }

  private def createSpecializedDefDef(ddef: DefDef, specSym: Symbol)(implicit ctx: Context) = {
    val oldSym = ddef.symbol
    def rhsFn(tparams: List[Type])(vparamss: List[List[Tree]]) = {
      // Transform references to types
      lazy val transformTparams: Map[Symbol, Type] = ddef.tparams.map(_.symbol).zip(tparams).toMap
      val typeMap = new TypeMap() {
        override def apply(tp: Type): Type = tp match {
          case tp: TypeRef if tp.typeSymbol.owner == oldSym && tp.typeSymbol.is(Param) =>
            transformTparams.getOrElse(tp.typeSymbol, tp)
          case _ => mapOver(tp)
        }
      }

      // Transform references to terms
      val transformVparams: Map[Symbol, Tree] = (ddef.vparamss.flatten.map(_.symbol) zip vparamss.flatten).toMap
      def treeMap(tree: Tree): Tree = tree match {
        case tree: Ident if tree.symbol.isTerm && tree.symbol.is(Param) && tree.symbol.owner == oldSym =>
          transformVparams(tree.symbol)
        case Return(expr, from) if from.symbol == oldSym => Return(expr, ref(specSym))
        case _ => tree
      }

      // Apply transforms
      val treeTypeMap = new TreeTypeMap(typeMap, treeMap, oldSym :: Nil, specSym :: Nil)
      treeTypeMap.transform(ddef.rhs)
    }

    transformSpecializedBody(polyDefDef(specSym.asTerm, rhsFn))
  }

  private def transformSpecializedBody(specDefDef: DefDef)(implicit ctx: Context): DefDef = {
    def treeMap(tree: Tree): Tree = tree match {
      case Match(sel, cases) =>
        val newCases = cases.filter {
          case CaseDef(Bind(_, Typed(_, tp)), _, _) => sel.tpe <:< tp.tpe
          // TODO: other patterns?
          case _ => true
        }
        cpy.Match(tree)(sel, newCases)
      case tree @ TypeApply(fun, args) =>
        fun match {
          case Select(qual, _) if fun.symbol == defn.Any_isInstanceOf =>
            if (qual.tpe <:< args.head.tpe) Literal(Constants.Constant(true))
            else if (!(args.head.tpe <:< qual.tpe)) Literal(Constants.Constant(false))
            else tree
          case _ =>
            specializedTypeApply(tree)
        }
      case _ => tree
    }
    val transformInnerCalls = new TreeTypeMap(treeMap = treeMap)
    transformInnerCalls.transform(specDefDef).asInstanceOf[DefDef]
  }
}
