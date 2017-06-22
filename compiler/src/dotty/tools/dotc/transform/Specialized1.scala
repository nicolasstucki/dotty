package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.TreeTransforms._
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.linker._
import dotty.tools.dotc.transform.specialize.{SymbolSpecialization, TreeSpecialization}

import scala.collection.mutable

class Specialized1 extends MiniPhaseTransform { thisTransformer =>
  import tpd._

  override def phaseName = "specialized1"

  val specSymbols: mutable.Map[(Symbol, OuterTargs), Symbol] = mutable.Map.empty
  val specDefDefs: mutable.Map[Symbol, List[DefDef]] = mutable.Map.empty
  val specDefDefsInClass: mutable.Map[Symbol, List[Symbol]] = mutable.Map.empty

  val outerTargsOf: mutable.Map[Symbol, OuterTargs] = mutable.Map.empty

  private val defDefsOf: mutable.Map[Symbol, DefDef] = mutable.Map.empty

  private val needsSpecialization = mutable.Map.empty[Symbol, List[(OuterTargs, Context)]]
  private val specializationFor = mutable.Map.empty[Symbol, List[(OuterTargs, Context)]]

  private val allKnownOverwrites: mutable.Map[Symbol, List[Symbol]] = mutable.Map.empty

  private val symSpecializer = new SymbolSpecialization

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    getSpecializedSym(tree) // trigger creation of specialized function symbols and trees
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    if (sym.isSpecializable) {
      defDefsOf.put(sym, tree)
      sym.allOverriddenSymbols.foreach { sym0 =>
        allKnownOverwrites.put(sym0, sym.symbol :: allKnownOverwrites.getOrElse(sym0, Nil))
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
      val specSym = symSpecializer.specializedDefSymbol(sym, outerTargs)
      specSymbols.put(key, specSym)
      outerTargsOf.put(specSym, outerTargs.without(sym))
      val specDefDef = TreeSpecialization.specializedDefDef(defDefsOf(sym), specSym, outerTargs)

      val trav = new TreeTraverser {
        override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = {
          tree match {
            case tree: DefDef if tree.symbol.isSpecializable => defDefsOf.put(tree.symbol, tree)
            case tree: TypeApply => getSpecializedSym(tree)
            case _ =>
          }
          traverseChildren(tree)
        }
      }
      trav.traverse(specDefDef)

      specDefDefs.put(sym, specDefDef :: specDefDefs.getOrElse(sym, Nil))
      if (sym.owner.isClass)
        specDefDefsInClass.put(sym.owner, specSym :: specDefDefsInClass.getOrElse(sym.owner, Nil))
      ctx.debuglog(
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

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {

    val units1 = super.runOn(units)
    // TODO load compilation units based on needsSpecialization and super.runOn(loadedUnits)
    ctx.log("Did not specialize: " + needsSpecialization.keys)

    units1
  }

}
