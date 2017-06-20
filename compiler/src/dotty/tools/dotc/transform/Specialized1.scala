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
import dotty.tools.dotc.linker._
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.tasty.DottyUnpickler

import scala.collection.mutable

class Specialized1 extends MiniPhaseTransform { thisTransformer =>
  import tpd._

  override def phaseName = "specialized1"

  override def runsAfterGroupsOf = Set(classOf[FirstTransform])

  val specSymbols: mutable.Map[(Symbol, OuterTargs), Symbol] = mutable.Map.empty
  val specDefDefs: mutable.Map[Symbol, List[DefDef]] = mutable.Map.empty
  val specDefDefsInClass: mutable.Map[Symbol, List[Symbol]] = mutable.Map.empty

  val outerTargsOf: mutable.Map[Symbol, OuterTargs] = mutable.Map.empty

  private val defDefsOf: mutable.Map[Symbol, DefDef] = mutable.Map.empty

  private val needsSpecialization = mutable.Map.empty[Symbol, List[(OuterTargs, Context)]]
  private val specializationFor = mutable.Map.empty[Symbol, List[(OuterTargs, Context)]]

  private var allKnownOverwrites: mutable.Map[Symbol, List[Symbol]] = _

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    getSpecializedSym(tree) // trigger creation of specialized function symbols and trees
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    if (sym.isSpecializable) {
      defDefsOf.put(sym, tree)
      sym.allOverriddenSymbols.foreach { sym0 =>
        specializationFor.getOrElse(sym0, Nil).foreach {
           case (outerTargs, ctx1) => specializedMethod(sym, outerTargs.changeParent(sym0, sym))(ctx1)
        }
      }
    }
    needsSpecialization.get(sym) match {
      case Some(outerTargsList) =>
        val outerTargsList1 = outerTargsList
        outerTargsList1.reverse.foreach { case (outerTargs, ctx1) => specializedMethod(sym, outerTargs)(ctx1) }
        needsSpecialization.remove(sym)
        specializationFor.put(sym, outerTargsList1 ::: specializationFor.getOrElse(sym, List.empty))
      case _ =>
    }
    tree
  }

  /** Get the symbol of the specialized version of this type application.
   *
   *  If the DefDef is not specializable or DefDef has not been seen yet, NoSymbol is returned.
   *  In the later case the request for specialization is registered and processed when the DefDef
   *  is reached (maybe in another compilation unit).
   */
  def getSpecializedSym(tree: TypeApply)(implicit ctx: Context): Symbol = {
    val sym = tree.symbol
    lazy val outerTargs = {
      val targs = tree.args.map(_.tpe)
      val qualOuterTargs = localOuterTargs(tree)
      val names = sym.info.asInstanceOf[PolyType].paramNames
      val specializedTargs = specilizableTypeParams(sym).map(i => (names(i), targs(i).widenDealias))
      qualOuterTargs.addAll(sym, specializedTargs.filter(x => isSpecilizableType(x._2)))
    }
    if (!sym.isSpecializable || !outerTargs.mp.contains(sym)) NoSymbol
    else specializedMethod(sym, outerTargs)
  }

  /** Compute the type arguments of the type applications agregated with the local type arguments of the prefix */
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

  /** Get the symbol of the specialized version of this type application.
   *
   *  If the DefDef has not been seen yet NoSymbol is returned. In this case a request for specialization
   *  is registered and processed when the DefDef is reached (maybe in another compilation unit).
   */
  private def specializedMethod(sym: Symbol, outerTargs: OuterTargs)(implicit ctx: Context): Symbol = {
    assert(sym.info.isInstanceOf[PolyType])
    assert(outerTargs.mp.contains(sym))
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
      val subs = new SubstituteOuterTargs(outerTargs)
      val specResType = subs(symInfo.resType)
      val specInfo = symInfo.derivedLambdaType(paramInfos = specParamInfos, resType = specResType)
      val specSym = ctx.newSymbol(sym.owner, specName, specFlags, specInfo, sym.privateWithin, sym.coord)
      specSymbols.put(key, specSym)
      outerTargsOf.put(specSym, outerTargs.without(sym))
      val specDefDef = createSpecializedDefDef(sym, specSym, outerTargs)
      specDefDefs.put(sym, specDefDef :: specDefDefs.getOrElse(sym, Nil))
      if (sym.owner.isClass)
        specDefDefsInClass.put(sym.owner, specSym :: specDefDefsInClass.getOrElse(sym.owner, Nil))
      ctx.log(
        s"""specialized
          |  ${sym.show + sym.info.show} in ${sym.owner.showFullName} into
          |  ${specSym.show + specSym.info.show}
          |  $outerTargs
          |  ${outerTargs.mp.keys.map(_.showFullName)}
        """.stripMargin)
      specSym
    }
    specSymbols.get(key) match {
      case Some(specSym) => specSym
      case None =>
        if (!defDefsOf.contains(sym)) {
          needsSpecialization.put(sym, (outerTargs, ctx) :: needsSpecialization.getOrElse(sym, List.empty))
          NoSymbol
        } else {
          val specSym = newSpecializedMethod
          specializationFor.put(sym, (outerTargs, ctx) :: specializationFor.getOrElse(sym, List.empty))
          allKnownOverwrites.getOrElse(sym, Nil).foreach(s => specializedMethod(s, outerTargs.changeParent(sym, s)))
          specSym
        }
    }
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
    val ddef = defDefsOf(oldSym)
    def rhsFn(tparams: List[Type])(vparamss: List[List[Tree]]) = {
      val transformTparams: Map[Symbol, Type] = ddef.tparams.map(_.symbol).zip(tparams).toMap
      val transformVparams: Map[Symbol, Tree] = (ddef.vparamss.flatten.map(_.symbol) zip vparamss.flatten).toMap

      // Transform references to types
      val typeMap = new TypeMap() {
        override def apply(tp: Type): Type = tp match {
          case tp: TypeRef if tp.typeSymbol.owner == oldSym && tp.typeSymbol.is(Param) =>
            transformTparams.getOrElse(tp.typeSymbol, tp)
          case tp: TermRef if tp.termSymbol.owner == oldSym && tp.termSymbol.is(Param) =>
            transformVparams.get(tp.termSymbol).map(_.symbol.termRef).getOrElse(tp)
          case _ => mapOver(tp)
        }
      }

      // Transform references to terms
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
          case tree: DefDef if tree.symbol.isSpecializable => defDefsOf.put(tree.symbol, tree)
          case _ =>
        }
        traverseChildren(tree)
      }
    }
    trav.traverse(specDefDef)

    transformSpecializedDefDef(specDefDef, outerTargs)
  }

  private def transformSpecializedDefDef(specDefDef: DefDef, outerTargs: OuterTargs)(implicit ctx: Context): DefDef = {
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
    val substMap = new SubstituteOuterTargs(outerTargs)
    val transformInnerCalls = new TreeTypeMap(typeMap = substMap, treeMap = treeMap)
    transformInnerCalls.transform(specDefDef).asInstanceOf[DefDef]
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val transformedUnits = runOn(Nil, units)
    ctx.log("Specialize methods created: " + specSymbols.valuesIterator.toList.map(_.showFullName))
    ctx.log("Did not specialize: " + needsSpecialization.keys.map(_.showFullName))
    ctx.log("Compilation units after phase: " + transformedUnits)
    transformedUnits
  }

  private def runOn(processed: List[CompilationUnit], unprocessed: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    if (unprocessed.isEmpty) processed
    else {
      val specializedOverwrites = ctx.phaseOfClass(classOf[SpecializedOverwrites]).asInstanceOf[SpecializedOverwrites]
      allKnownOverwrites = specializedOverwrites.allKnownOverwrites

      val newProcessed = processed ::: super.runOn(unprocessed)

      val topLevelClasses0 = needsSpecialization.keySet.map(x => x.topLevelClass.denot.asClass)
      val topLevelClasses1 = topLevelClasses0.filter(x => !x.is(JavaDefined) && (x.symbol ne defn.ObjectClass))
      val newUnits = topLevelClasses1.flatMap(FromTasty.loadCompilationUnits).filterNot(newProcessed.contains).toList

      def firstTransform = ctx.phaseOfClass(classOf[FirstTransform]).asInstanceOf[FirstTransform]

      val newUnitsTansformed =
        if (newUnits.isEmpty) Nil
        else super.runOn(firstTransform.runOn(newUnits))

      runOn(newProcessed, newUnitsTansformed)
    }
  }

}
