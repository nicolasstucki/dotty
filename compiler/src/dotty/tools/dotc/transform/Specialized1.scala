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

  private val specSymbols = mutable.LinkedHashMap.empty[(Symbol, OuterTargs), Symbol]
  val specDefDefs = mutable.LinkedHashMap.empty[Symbol, List[DefDef]]

  private val outerTargsOf = mutable.LinkedHashMap.empty[Symbol, OuterTargs]

  private val defDefsOf = mutable.LinkedHashMap.empty[Symbol, DefDef]
  private val clsDefsOf = mutable.LinkedHashMap.empty[Symbol, TypeDef]

  private val needsSpecialization = mutable.LinkedHashMap.empty[Symbol, List[(OuterTargs, Context)]]

  private val allKnownOverwrites: mutable.Map[Symbol, List[Symbol]] = mutable.LinkedHashMap.empty

  private val symSpecializer = new SymbolSpecialization

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    val outerTargs = outerTargsOf(tree)
    if (outerTargs.mp.contains(sym))
      needsSpecialization.put(sym, (outerTargs, ctx) :: needsSpecialization.getOrElse(sym, List.empty))
    tree
  }

  override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    if (sym.isClass && sym.primaryConstructor.isSpecializable)
      clsDefsOf.put(sym, tree)
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val sym = tree.symbol
    if (sym.isSpecializable && !sym.isConstructor) {
      defDefsOf.put(sym, tree)
      sym.allOverriddenSymbols.foreach { sym0 =>
        allKnownOverwrites.put(sym0, sym.symbol :: allKnownOverwrites.getOrElse(sym0, Nil))
      }
    }
    tree
  }

  def getOuterTargsOf(sym: Symbol): Option[OuterTargs] = outerTargsOf.get(sym)

  def getSpecializedSym(tree: TypeApply)(implicit ctx: Context): Symbol = {
    val sym = tree.symbol
    val outerTargs = outerTargsOf(tree)
    val key = (sym, outerTargs)
    specSymbols.getOrElse(key, NoSymbol)
  }

  private def outerTargsOf(tree: TypeApply)(implicit ctx: Context): OuterTargs = {
    val sym = tree.symbol
    if (!sym.isSpecializable) OuterTargs.empty
    else {
      /** Compute the type arguments of the type applications agregated with the local type arguments of the prefix */
      def qualOuterTargs: OuterTargs = {
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

      val targs = tree.args.map(_.tpe)
      val names = sym.info.asInstanceOf[PolyType].paramNames
      val specializedTargs = specilizableTypeParams(sym).map(i => (names(i), targs(i).widenDealias))
      qualOuterTargs.addAll(sym, specializedTargs.filter(x => isSpecilizableType(x._2)))
    }
  }



  private def getOuterTargs(tpe: Type, acc: OuterTargs)(implicit ctx: Context): OuterTargs = tpe match {
    case RefinedType(parent, name, info) => getOuterTargs(parent, acc.add(parent.classSymbol, name, info))
    case _ => acc
  }

  private def specializeMethod(sym: Symbol, outerTargs: OuterTargs)(implicit ctx: Context): DefDef = {
    assert(sym.info.isInstanceOf[PolyType])
    assert(outerTargs.mp.contains(sym))
    val key = (sym, outerTargs)
    assert(!specSymbols.contains(key))
    val specSym = symSpecializer.specializedDefSymbol(sym, outerTargs)
    specSymbols.put(key, specSym)
    outerTargsOf.put(specSym, outerTargs.without(sym))
    val specDefDef = TreeSpecialization.specializedDefDef(defDefsOf(sym), specSym, outerTargs)

    specDefDefs.put(sym, specDefDef :: specDefDefs.getOrElse(sym, Nil))

    ctx.debuglog(
      s"""specialized
         |  ${sym.show + sym.info.show} in ${sym.owner.showFullName} into
         |  ${specSym.show + specSym.info.show}
         |  $outerTargs
         |  ${outerTargs.mp.keys.map(_.showFullName)}
      """.stripMargin)

    specDefDef
  }

  private def specializeTrait(sym: ClassSymbol, outerTargs: OuterTargs)(implicit ctx: Context): Tree = {
    assert(outerTargs.mp.contains(sym))
    val key = (sym, outerTargs)
    assert(!specSymbols.contains(key))
    val specSym = symSpecializer.specializeTraitSymbol(sym, outerTargs)
    specSymbols.put(key, specSym)
    val specTraitDef = TreeSpecialization.specializedTraitDef(clsDefsOf(sym), specSym, outerTargs)

    println(sym)
    println(sym.primaryConstructor.info.show)
    println(specSym)
    println(specSym.primaryConstructor.info.show)
    println(specTraitDef.show)
    println(specTraitDef)
    println()
    println()
    null
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

    def process(specs: mutable.LinkedHashMap[Symbol, List[(OuterTargs, Context)]]): Unit = {
      val newSpecs = mutable.LinkedHashMap.empty[Symbol, List[(OuterTargs, Context)]]
      for ((sym, outerTargsList) <- specs) {
        for ((outerTargs, ctx1) <- outerTargsList.reverse) {
          if (!sym.isConstructor) specializeForCall(sym, outerTargs, newSpecs)(ctx1)
          else if (sym.owner.is(Trait)) specializeTrait(sym.owner.asClass, outerTargs, newSpecs)(ctx1)
          else println("Not specializing constructor call " + sym.showFullName)
        }
      }
      if (newSpecs.nonEmpty)
        process(newSpecs)
    }
    process(needsSpecialization)

    units1
  }

  def specializeForCall(sym: Symbol, outerTargs: OuterTargs, newSpecs: mutable.LinkedHashMap[Symbol, List[(OuterTargs, Context)]])(implicit ctx: Context): Unit = {
    if (!specSymbols.contains((sym, outerTargs))) {
      val trav = new TreeTraverser {
        override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = {
          tree match {
            case tree: DefDef if tree.symbol.isSpecializable => defDefsOf.put(tree.symbol, tree)
            case tree: TypeApply =>
              val sym = tree.symbol
              val outerTargs = outerTargsOf(tree)
              if (outerTargs.mp.contains(sym)) {
                newSpecs.put(sym, (outerTargs, ctx) :: newSpecs.getOrElse(sym, List.empty))
                needsSpecialization.put(sym, (outerTargs, ctx) :: needsSpecialization.getOrElse(sym, List.empty))
              }

            case _ =>
          }
          traverseChildren(tree)
        }
      }

      val specDefDef = specializeMethod(sym, outerTargs)
      trav.traverse(specDefDef)

      allKnownOverwrites.getOrElse(sym, Nil).foreach(s => trav.traverse(specializeMethod(s, outerTargs.changeParent(sym, s))))
    }
  }

  def specializeTrait(sym: ClassSymbol, outerTargs: OuterTargs, newSpecs: mutable.LinkedHashMap[Symbol, List[(OuterTargs, Context)]])(implicit ctx: Context): Unit = {
    val specTrait = specializeTrait(sym, outerTargs)
    // TODO check for new specialization opportunities

  }

}
