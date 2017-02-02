package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.FunctionName
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.annotation.tailrec

/** Rewrite functions that took phantom parameters into their correspondent FunctionN */
class PhantomFunctions extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  override def phaseName: String = "phantomFunctions"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[PhantomParamErasure])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    def assertNotPhantomFunction(tpe: Type): Unit =
      assert(!isPhantomFunctionType(tpe), "All phantom functions should be erased in " + tree)

    tree match {
      case _: TypeTree       =>
      case _: TypeDef        =>
      case tree: ValOrDefDef => assertNotPhantomFunction(tree.tpt.typeOpt)
      case _                 => assertNotPhantomFunction(tree.tpe)
    }
  }

  /* Tree transform */

  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val newTypeTree = TypeTree(erasePhantomFunctions(tree.tpe))
    if (newTypeTree == tree) tree
    else newTypeTree
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.name != nme.apply || !FunctionName(tree.symbol.owner).isPhantom) tree
    else tree.qualifier.select(nme.apply)
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = erasePhantomFunctions(tp)

  /* private methods */

  @tailrec private def isPhantomFunctionType(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
    case tpe: RefinedType => isPhantomFunctionType(tpe.parent)
    case _                => FunctionName(tpe.classSymbol).isPhantom
  }

  private def erasePhantomFunctions(tpe: Type)(implicit ctx: Context): Type = {
    val erasePhantomFunctionsMap = new DeepTypeMap {
      override def apply(tp: Type): Type = tp match {
        case RefinedType(parent, refinedName, _) if isPhantomFunctionTypeParam(refinedName) =>
          apply(parent)

        case tp: TypeRef if tp.symbol.isClass =>
          val functionName = FunctionName(tp.classSymbol)
          if (!functionName.isFunctionName || !functionName.isPhantom) tp
          else defn.FunctionType(functionName.withoutPhantoms)

        case tp => mapOver(tp)
      }
    }
    erasePhantomFunctionsMap(tpe)
  }

  private def isPhantomFunctionTypeParam(name: Name): Boolean = {
    def test(sufix: Name) = name.startsWith(tpnme.scala_ ++ "$" ++ sufix)
    test(tpnme.PhantomFunction) || test(tpnme.ImplicitPhantomFunction)
  }

}
