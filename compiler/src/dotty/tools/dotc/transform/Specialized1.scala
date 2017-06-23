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

  private val specSymbols: mutable.Map[(Symbol, OuterTargs), Symbol] = mutable.Map.empty
  val specDefDefs: mutable.Map[Symbol, List[DefDef]] = mutable.Map.empty

  private val outerTargsOf: mutable.Map[Symbol, OuterTargs] = mutable.Map.empty

  private val defDefsOf: mutable.Map[Symbol, DefDef] = mutable.LinkedHashMap.empty

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


    // TODO improve
    val newSpecs = mutable.LinkedHashMap.empty[Symbol, List[(OuterTargs, Context)]]
    newSpecs ++= needsSpecialization
    do {
      val toProcess = newSpecs.toMap
      newSpecs.clear()
      for (ddef <- defDefsOf.values) {
        val sym = ddef.symbol
        toProcess.get(sym) match {
          case Some(outerTargsList) =>
            val outerTargsList1 = outerTargsList
            for ((outerTargs, ctx1) <- outerTargsList1.reverse) {
              specializeForCall(sym, outerTargs, newSpecs)(ctx1)
            }

          case _ =>
        }
      }


    } while (newSpecs.nonEmpty)

    ctx.log("Did not specialize: " + needsSpecialization.keys)

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

}
