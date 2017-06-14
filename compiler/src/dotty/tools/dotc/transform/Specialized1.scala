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

  private type SpecBounds = List[TypeBounds]

  private val specSymbols: mutable.Map[(Symbol, List[Type]), Symbol] = mutable.Map.empty
  val specDefDefs: mutable.Map[Symbol, List[DefDef]] = mutable.Map.empty
  val specDefDefsInClass: mutable.Map[Symbol, List[Symbol]] = mutable.Map.empty

  private var specialized0Phase: Specialized0 = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransforms.TreeTransform = {
    specialized0Phase = ctx.phaseOfClass(classOf[Specialized0]).asInstanceOf[Specialized0]
    super.prepareForUnit(tree)
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    specializedTypeApply(tree)

  private def specializedTypeApply(tree: TypeApply)(implicit ctx: Context) = {
    val sym = tree.symbol
    lazy val targs = tree.args.map(_.tpe)
    lazy val specBounds = specializedBounds(sym, targs)
    if (!isSpecilizableMethod(sym) || specBounds == sym.info.asInstanceOf[PolyType].paramInfos) tree
    else {
      val specSym = specializedMethod(sym, specBounds)
      allKnownOverwrites(sym).foreach(s => specializedMethod(s, specBounds))
      val specFun = tree.fun match {
        case Select(qual, _) => qual.select(specSym)
        case _ => ref(specSym)
      }
      specFun.appliedToTypes(targs)
    }
  }

  private def specializedMethod(sym: Symbol, specBounds: SpecBounds)(implicit ctx: Context): Symbol = {
    assert(sym.info.isInstanceOf[PolyType])
    val key = (sym, specBounds)
    def newSpecializedMethod = {
      val symInfo = sym.info.asInstanceOf[PolyType]
      val specName = sym.name ++ specializedNameSuffix(sym, specBounds)
      val specFlags = sym.flags | Synthetic
      val specInfo = symInfo.derivedLambdaType(paramInfos = specBounds)
      val specSym = ctx.newSymbol(sym.owner, specName, specFlags, specInfo, sym.privateWithin, sym.coord)
      specSymbols.put(key, specSym)
      val specDefDef = createSpecializedDefDef(getDefDefOf(sym), specSym)
      specDefDefs.put(sym, specDefDef :: specDefDefs.getOrElse(sym, Nil))
      if (sym.owner.isClass)
        specDefDefsInClass.put(sym.owner, specSym :: specDefDefsInClass.getOrElse(sym.owner, Nil))
      ctx.debuglog(
        s"""specialized
          |  ${sym.show + sym.info.show} into
          |  ${specSym.show + specSym.info.show}
        """.stripMargin)
      specSym
    }
    specSymbols.get(key) match {
      case Some(specSym) => specSym
      case None => newSpecializedMethod
    }
  }

  private val boundNames = mutable.Map.empty[SpecBounds, Name]
  private var nameIdx = 0
  private def specializedNameSuffix(sym: Symbol, specBounds: SpecBounds)(implicit ctx: Context): Name = {
    def makeNewName = {
      nameIdx += 1
      ("$spec$" + nameIdx).toTermName
    }
    boundNames.getOrElseUpdate(specBounds, makeNewName)
  }

  private def allKnownOverwrites(sym: Symbol)(implicit ctx: Context): List[Symbol] =
    specialized0Phase.specializedOverwrites.getOrElse(sym, Nil)

  private def getDefDefOf(sym: Symbol)(implicit ctx: Context): DefDef =
    specialized0Phase.specializedDefDefs(sym)

  private def registerDefDef(ddef: DefDef)(implicit ctx: Context): Unit =
    specialized0Phase.specializedDefDefs.put(ddef.symbol, ddef)

  private def isSpecilizableMethod(sym: Symbol)(implicit ctx: Context): Boolean =
    specialized0Phase.isSpecilizable(sym)

  private def specializedBounds(sym: Symbol, targs: List[Type])(implicit ctx: Context): SpecBounds = {
    val typeBounds = sym.info.asInstanceOf[PolyType].paramInfos
    val specializableIdxs = specilizableTypeParams(sym)

    targs.zipWithIndex.map { case (tpe, idx) =>
      if (specializableIdxs.contains(idx) && isSpecilizableType(tpe.widenDealias)) {
        // TODO constant fold parameters specialized to singleton types
        if (tpe.isInstanceOf[ConstantType]) TypeAlias(tpe)
        else TypeAlias(tpe.widenDealias.classSymbol.typeRef)
      } else typeBounds(idx)
    }
  }

  private def specilizableTypeParams(sym: Symbol)(implicit ctx: Context): List[Int] = {
    if (ctx.settings.specializeAll.value) sym.info.asInstanceOf[PolyType].paramNames.indices.toList
    else {
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
  }

  private def isSpecilizableType(tpe: Type)(implicit ctx: Context): Boolean = {
    ctx.settings.specializedForAll.value ||
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

    val specDefDef = polyDefDef(specSym.asTerm, rhsFn)

    val trav = new TreeTraverser {
      override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = {
        tree match {
          case tree: DefDef if isSpecilizableMethod(tree.symbol) => registerDefDef(tree)
          case _ =>
        }
        traverseChildren(tree)
      }
    }
    trav.traverse(specDefDef)

    transformSpecializedBody(specDefDef)
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
