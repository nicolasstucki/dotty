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
import dotty.tools.dotc.linker.OuterTargs

import scala.collection.mutable

class Specialized1 extends MiniPhaseTransform { thisTransformer =>
  import tpd._

  override def phaseName = "specialized1"

  val specSymbols: mutable.Map[(Symbol, OuterTargs), Symbol] = mutable.Map.empty
  val specDefDefs: mutable.Map[Symbol, List[DefDef]] = mutable.Map.empty
  val specDefDefsInClass: mutable.Map[Symbol, List[Symbol]] = mutable.Map.empty

  private var specialized0Phase: Specialized0 = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransforms.TreeTransform = {
    specialized0Phase = ctx.phaseOfClass(classOf[Specialized0]).asInstanceOf[Specialized0]
    super.prepareForUnit(tree)
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    getSpecializedSym(tree) // trigger creation of specialized function symbols and trees
    tree
  }

  def getSpecializedSym(tree: TypeApply)(implicit ctx: Context): Symbol = {
    val sym = tree.symbol
    val targs = tree.args.map(_.tpe)
    val qualOuterTargs = localOuterTargs(tree)
    def tparamsAsOuterTargs(acc: OuterTargs, s: Symbol): OuterTargs = {
      val names = s.info.asInstanceOf[PolyType].paramNames
      val specializedTargs = specilizableTypeParams(s).map(i => (names(i), targs(i).widenDealias))
      acc.addAll(s, specializedTargs.filter(x => isSpecilizableType(x._2)))
    }
    val outerTargs = (sym :: allKnownOverwrites(sym)).foldLeft(qualOuterTargs)(tparamsAsOuterTargs)
    if (!sym.isSpecializable || !outerTargs.mp.contains(sym)) NoSymbol
    else specializedMethod(sym, outerTargs)
  }

  def localOuterTargs(tree: TypeApply)(implicit ctx: Context): OuterTargs = {
    val sym = tree.symbol
    val targs = tree.args.map(_.tpe)
    val qualOuterTargs = tree.fun match {
      case Select(qual, _) =>
        def withParents(parents: List[Type], acc: OuterTargs): OuterTargs = {
          parents.foldLeft(acc) { (acc1, p) =>
            acc1.addAll(p.classSymbol, p.typeParams.map(t => (t.paramName, t.paramInfo.asInstanceOf[TypeBounds].lo)))
          }
        }
        // TODO add parents of parents
        val outerTargs = getOuterTargs(qual.tpe, OuterTargs.empty)
        withParents(qual.tpe.widenDealias.parents, outerTargs)

      case _ => OuterTargs.empty
    }
    val names = sym.info.asInstanceOf[PolyType].paramNames
    val specializedTargs = specilizableTypeParams(sym).map(i => (names(i), targs(i).widenDealias))
    qualOuterTargs.addAll(sym, specializedTargs.filter(t => isSpecilizableType(t._2)))
  }

  private def getOuterTargs(tpe: Type, acc: OuterTargs)(implicit ctx: Context): OuterTargs = tpe match {
    case RefinedType(parent, name, info) => getOuterTargs(parent, acc.add(parent.classSymbol, name, info))
    case _ => acc
  }

  private def specializedMethod(sym: Symbol, outerTargs: OuterTargs)(implicit ctx: Context): Symbol = {
    assert(sym.info.isInstanceOf[PolyType])
    val key = (sym, outerTargs)
    def newSpecializedMethod = {
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
      val specInfo = symInfo.derivedLambdaType(paramInfos = specParamInfos)
      val specSym = ctx.newSymbol(sym.owner, specName, specFlags, specInfo, sym.privateWithin, sym.coord)
      specSymbols.put(key, specSym)
      val specDefDef = createSpecializedDefDef(sym, specSym, outerTargs)
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
      case None =>
        val specSym = newSpecializedMethod
        // TODO move this out to getSpecializedSym
        allKnownOverwrites(sym).foreach(s => specializedMethod(s, outerTargs))
        specSym
    }
  }

  private val boundNames = mutable.Map.empty[(Name, OuterTargs), Name]
  private val nameIdx = mutable.Map.empty[Name, Int]
  private def specializedNameSuffix(sym: Symbol, outerTargs: OuterTargs)(implicit ctx: Context): Name = {
    def makeNewName = {
      val idx = nameIdx.getOrElse(sym.name, 0) + 1
      nameIdx.put(sym.name, idx)
      ("$spec$" + idx).toTermName
    }
    boundNames.getOrElseUpdate((sym.name, outerTargs), makeNewName)
  }

  private def allKnownOverwrites(sym: Symbol)(implicit ctx: Context): List[Symbol] =
    specialized0Phase.specializedOverwrites.getOrElse(sym, Nil)

  private def getDefDefOf(sym: Symbol)(implicit ctx: Context): DefDef =
    specialized0Phase.specializedDefDefs(sym)

  private def registerDefDef(ddef: DefDef)(implicit ctx: Context): Unit =
    specialized0Phase.specializedDefDefs.put(ddef.symbol, ddef)

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

  private def createSpecializedDefDef(oldSym: Symbol, specSym: Symbol, outerTargs: OuterTargs)(implicit ctx: Context) = {
    val ddef = getDefDefOf(oldSym)
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
        assert(!tree.symbol.exists || tree.symbol.owner != ddef.symbol, tree)
        tree match {
          case tree: DefDef if tree.symbol.isSpecializable => registerDefDef(tree)
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
        if (newCases.isEmpty) Throw(New(defn.MatchErrorType, List(ref(sel.symbol))))
        else cpy.Match(tree)(sel, newCases)
      case tree @ TypeApply(fun, args) =>
        fun match {
          case Select(qual, _) if fun.symbol == defn.Any_isInstanceOf =>
            if (qual.tpe <:< args.head.tpe) Literal(Constants.Constant(true))
            else if (!(args.head.tpe <:< qual.tpe)) Literal(Constants.Constant(false))
            else tree
          case _ =>
            getSpecializedSym(tree) // trigger creation of specialized function symbols and trees
            tree
        }
      case _ => tree
    }
    val transformInnerCalls = new TreeTypeMap(treeMap = treeMap)
    transformInnerCalls.transform(specDefDef).asInstanceOf[DefDef]
  }
}
